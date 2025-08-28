unit fitproc;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

{.$DEFINE Y_ERRORS_WITH_VAR_OF_RESIDUALS}

interface

uses
  Classes, SysUtils, lcvtypes;

// It this vestion of PolyFit, the input array (a) must be allocated and
// initialized. fit must be allocated.
// Input and output arrays here are of the TArbFloatArray type for compatibility
// with NumLib.
// This version of PolyFit is used in DFT.
procedure PolyFit(const a: TArbFloatArray;      // 'Design matrix' (independent variables)
                  const Yarray: TArbFloatArray; // Observed values
                  ATrendDegree: Integer;        // Degree of the algebraic polynomial
                  ATrigPolyDegree: Integer;     // Degree of the (single) trigonometric polynomial
                  var fit: TArbFloatArray);     // Approximated values of the observations

procedure PolyFit(const Xarray: TDoubleArray;
                  const Yarray: TDoubleArray;
                  ATrendDegree: Integer;
                  const ATrigPolyDegrees: TInt5Array;
                  const AFrequencies: TDouble5Array;
                  fitXmin, fitXmax, fitXstep: Double;
                  out Xfit: TDoubleArray;
                  out Yfit: TDoubleArray;
                  out YfitErrors: TDoubleArray;
                  out FitAtPoints: TDoubleArray;
                  out FitAtPointsErrors: TDoubleArray;
                  out FitAtPointsAlgebraic: TDoubleArray;
                  out Formula: string;
                  out Info: string);

function CalcResidualSquared(const Observations, Model: TDoubleArray): Double;

{$IFDEF USE_WINMKL}
function dgels_info: string;
{$ENDIF}

implementation

uses
{$IFDEF USE_WINMKL}
  Windows,
{$ENDIF}
  math, typ, omv, inv, sle, miscutils, formatutils;

function CalcResidualSquared(const Observations, Model: TDoubleArray): Double;
var
  I: Integer;
  R: Double;
begin
  if Length(Observations) <> Length(Model) then
    CalcError('Observations and Model arrays must be of equal length');
  Result := 0.0;
  for I := 0 to Length(Observations) - 1 do begin
    R := Observations[I] - Model[I];
    Result := Result + R * R;
  end;
end;

{$IFDEF USE_WINMKL}
type
  TDGELS_SOLVE = procedure(
    var trans: AnsiChar;
    var m, n, nrhs: Integer;
    var a: Double;
    var lda: Integer;
    var b: Double;
    var ldb: Integer;
    var info: Integer
); cdecl;

var
  dgels_solve: TDGELS_SOLVE;
{$ENDIF}

// The input array (a) must be allocated and initialized.
procedure PolyFitSolution(const a: TArbFloatArray;              // 'Design matrix' (independent variables)
                          const Yarray: TArbFloatArray;         // Observations
                          NofParameters: Integer;               // Number of parameters (independent variables)
                          var solution_vector: TArbFloatArray); // Solution
var
{$IFDEF USE_WINMKL}
  aa: TArbFloatArray;
  m, n, nrhs, lda, ldb, info: LongInt;
  trans: AnsiChar;
  CW8087: WORD;
  MXCSR: DWORD;
{$ENDIF}
  ndata: Integer;
  term: Integer;
begin
{$IFDEF USE_WINMKL}
  if Assigned(dgels_solve) then begin
    // See https://www.netlib.org/lapack/double/dgels.f
    // All DGELS parameters are 'var'; we need a copy of them.
    // Solving A * X = B.
    // We must transpose the matrix because of the difference between Pascal/Fortran array handling.
    // Transposing the matrix before computation is more effective than using the DGELS 'T' mode.
    m := Length(Yarray);
    n := NofParameters;
    // Must be m >= n
    if (m < 1) or (n < 1) or (n > m) then
      raise Exception.Create('DGELS_SOLVE: invalid parameters');
    trans := 'N';
    nrhs := 1;
    lda := m;
    ldb := m;
    // The input matrix and vector get overwritten, so copy them.
    SetLength(aa, Length(a));
    // Instead of the direct copy, transpose the matrix.
    omvtrm(a[0], m, n, n, aa[0], m);
    // solution_vector provides the right hand side vector (B) at the beginning.
    SetLength(solution_vector, m);
    Move(Yarray[0], solution_vector[0], m * SizeOf(Double));

    CW8087 := Get8087CW;
    MXCSR := GetMXCSR;
    try
      dgels_solve(trans, m, n, nrhs, aa[0], lda, solution_vector[0], ldb, info);
      if info <> 0 then
        DgeslError('DGELS_SOLVE failed, INFO = ' + IntToStr(info), info);
    finally
      Set8087CW(CW8087);
      SetMXCSR(MXCSR);
    end;

    // Set the correct length of the solution vector
    SetLength(solution_vector, n);
  end else begin
{$ENDIF}
    ndata := Length(Yarray);
    SetLength(solution_vector, NofParameters);
    // solve for overdetermined matrices
    slegls(a[0], ndata, NofParameters, NofParameters, Yarray[0], solution_vector[0], term);
    case term of
      1: ; // successful completion, the solution vector x is valid
      2: SleglsError('"slegls" error: ' + IntToStr(term) + ': there is no unambiguous solution because the columns of the matrix are linearly dependant.', Term);
      3: SleglsError('"slegls" error: ' + IntToStr(term) + ': error in input values: n < 1, or n > m.', Term);
    else
      SleglsError('"slegls" error: ' + IntToStr(term), Term);
    end;
{$IFDEF USE_WINMKL}
  end;
{$ENDIF}
end;

procedure PolyFitSolutionToFormula(ATrendDegree: Integer;
                                   const ATrigPolyDegrees: TInt5Array;
                                   const AFrequencies: TDouble5Array;
                                   const solution_vector: TArbFloatArray;
                                   const solution_vector_errors: TArbFloatArray;
                                   out Formula: string;
                                   out Info: string);
var
  I, N, Idx, Idx2: Integer;
  Sign: string;
  S, S2: string;
  Amplitudes: array of array[0..1] of Double;
begin
  Formula := '';
  Info := ' Coefficients'^M^J;
  // Trend
  for I := 0 to ATrendDegree do begin
    if solution_vector[I] < 0 then
      Sign := ' - '
    else
      Sign := ' + ';
    Formula := Formula + Sign + FloatToStrLocaleIndependent(Abs(solution_vector[I]));
    Info := Info + FloatToStrMod(solution_vector[I]) + ^I'[+-' + Trim(FloatToStrMod(solution_vector_errors[I])) + ']';
    if I > 0 then begin
      Formula := Formula + ' * (t-timeZeroPoint)**' + IntToStr(I);
      Info := Info + ^I' * (t-timeZeroPoint)^' + IntToStr(I);
    end;
    Formula := Formula + ' \' + ^M^J;
    Info := Info + ^M^J;
  end;
  // Trigonometric
  SetLength(Amplitudes, 0);
  Idx2 := 1 + ATrendDegree;
  for N := 0 to Length(ATrigPolyDegrees) - 1 do begin
    if ATrigPolyDegrees[N] > 0 then begin
      for I := 1 to ATrigPolyDegrees[N] do begin
        Idx := Idx2 + 2 * (I - 1);
        S := '2*math.pi*' + FloatToStrLocaleIndependent(I * AFrequencies[N]) + '*(t-timeZeroPoint)';
        S2 := '2*Pi*' + FloatToStrMod(I * AFrequencies[N]) + '*(t-timeZeroPoint)';
        if solution_vector[Idx] < 0 then Sign := ' - ' else Sign := ' + ';
        Formula := Formula + Sign + FloatToStrLocaleIndependent(Abs(solution_vector[Idx]))     + ' * math.cos(' + S + ')';
        Info := Info + FloatToStrMod(solution_vector[Idx]) + ^I'[+-' + Trim(FloatToStrMod(solution_vector_errors[Idx])) + ']'^I' * cos(' + S2 + ')' + ^M^J;
        if solution_vector[Idx + 1] < 0 then Sign := ' - ' else Sign := ' + ';
        Formula := Formula + Sign + FloatToStrLocaleIndependent(Abs(solution_vector[Idx + 1])) + ' * math.sin(' + S + ') \' + ^M^J;
        Info := Info + FloatToStrMod(solution_vector[Idx + 1]) + ^I'[+-' + Trim(FloatToStrMod(solution_vector_errors[Idx + 1])) + ']'^I' * sin(' + S2 + ')' + ^M^J;
        SetLength(Amplitudes, Length(Amplitudes) + 1);
        Amplitudes[Length(Amplitudes) - 1][0] := I * AFrequencies[N];
        Amplitudes[Length(Amplitudes) - 1][1] := Sqrt(solution_vector[Idx] * solution_vector[Idx] + solution_vector[Idx + 1] * solution_vector[Idx + 1]);
      end;
      Idx2 := Idx2 + 2 * ATrigPolyDegrees[N];
    end;
  end;
  Info := Info + ^M^J;
  for N := 0 to Length(Amplitudes) - 1 do begin
    Info := Info + 'Period = '^I + FloatToStrMod(1.0 / Amplitudes[N][0]) + ^I + ' Amplitude = ' + FloatToStrMod(Amplitudes[N][1]) + ^M^J;
  end;
end;

// 'fit' array must be allocated
procedure CalculateFitAtPoints(const a: TArbFloatArray;
                               const solution_vector: TArbFloatArray;
                               ATrendDegree, ATrigPolyDegree: Integer;
                               var fit: TArbFloatArray);
var
  I, II, Idx, Idx2: Integer;
  NofParameters: Integer;
begin
  NofParameters := 1 + ATrendDegree + ATrigPolyDegree * 2;
  for I := 0 to Length(fit) - 1 do begin
    fit[I] := 0.0;
    for II := 0 to ATrendDegree do begin
      Idx := I * NofParameters + II;
      fit[I] := fit[I] + solution_vector[II] * a[Idx];
    end;
    for II := 1 to ATrigPolyDegree do begin
      Idx := 1 + ATrendDegree + 2 * (II - 1);
      Idx2 := I * NofParameters + Idx;
      fit[I] := fit[I] + solution_vector[Idx] * a[Idx2] + solution_vector[Idx + 1] * a[Idx2 + 1];
    end;
  end;
end;

// It this vestion of PolyFit, the input array (a) must be allocated and
// initialized. fit must be allocated.
// Input and output arrays here are of the TArbFloatArray type for compatibility
// with NumLib.
// This version of PolyFit is used in DFT.
procedure PolyFit(const a: TArbFloatArray;      // 'Design matrix' (independent variables)
                  const Yarray: TArbFloatArray; // Observed values
                  ATrendDegree: Integer;        // Degree of the algebraic polynomial
                  ATrigPolyDegree: Integer;     // Degree of the (single) trigonometric polynomial
                  var fit: TArbFloatArray);     // Approximated values of the observations
var
  solution_vector: TArbFloatArray;
  NofParameters: Integer;
begin
  NofParameters := 1 + ATrendDegree + ATrigPolyDegree * 2;
  PolyFitSolution(a, Yarray, NofParameters, solution_vector);
  CalculateFitAtPoints(a, solution_vector, ATrendDegree, ATrigPolyDegree, fit);
end;

procedure CalculateFitAtPointsExt(const a: TArbFloatArray;
                                  const solution_vector: TArbFloatArray;
                                  ATrendDegree: Integer;
                                  const ATrigPolyDegrees: TInt5Array;
                                  SigmaSq: ArbFloat;
                                  const XTXI: TArbFloatArray;
                                  ndata: Integer;
                                  out fit: TDoubleArray;
                                  out fitError: TDoubleArray;
                                  out fit_algebraic: TDoubleArray);
var
  I, II, N, Idx, Idx1, Idx2: Integer;
  NofParameters: Integer;
begin
  NofParameters := 1 + ATrendDegree;
  for I := 0 to Length(ATrigPolyDegrees) - 1 do
    NofParameters := NofParameters + ATrigPolyDegrees[I] * 2;

  SetLength(fit, ndata);
  SetLength(fit_algebraic, ndata);
  SetLength(fitError, ndata);

  for I := 0 to Length(fit) - 1 do begin
    fit[I] := 0.0;
    fit_algebraic[I] := 0.0;

    for II := 0 to ATrendDegree do begin
      Idx := I * NofParameters + II;
      fit[I] := fit[I] + solution_vector[II] * a[Idx];
    end;

    fit_algebraic[I] := fit[I];

    Idx2 := 1 + ATrendDegree;
    for N := 0 to Length(ATrigPolyDegrees) - 1 do begin
      if ATrigPolyDegrees[N] > 0 then begin
        for II := 1 to ATrigPolyDegrees[N] do begin
          Idx1 := Idx2 + 2 * (II - 1);
          Idx := I * NofParameters + Idx1;
          fit[I] := fit[I] + solution_vector[Idx1] * a[Idx] + solution_vector[Idx1 + 1] * a[Idx + 1];
        end;
      end;
      Idx2 := Idx2 + ATrigPolyDegrees[N] * 2;
    end;

    fitError[I] := 0.0;
    for Idx1 := 0 to NofParameters - 1 do begin
      for Idx2 := 0 to NofParameters - 1 do begin
        fitError[I] := fitError[I] + XTXI[Idx1 * NofParameters + Idx2] * a[I * NofParameters + Idx1] * a[I * NofParameters + Idx2];
      end;
    end;

{$IFDEF Y_ERRORS_WITH_VAR_OF_RESIDUALS}
    fitError[I] := fitError[I] + SigmaSq;
{$ENDIF}

    fitError[I] := Sqrt(fitError[I]);
  end;
end;

procedure CalculateFit(fitXmin, fitXmax, fitXstep: Double;
                       ATrendDegree: Integer;
                       const ATrigPolyDegrees: TInt5Array;
                       const AFrequencies: TDouble5Array;
                       const solution_vector: TArbFloatArray;
                       SigmaSq: ArbFloat;
                       const XTXI: TArbFloatArray;
                       out Xfit: TDoubleArray;
                       out Yfit: TDoubleArray;
                       out YfitErrors: TDoubleArray);
var
  Xvector: TDoubleArray;
  x, nu, angle: Double;
  I, II, N, Idx, Idx1, Idx2, nfit: Integer;
  NofParameters: Integer;
begin
  nfit := Ceil((fitXmax - fitXmin) / fitXstep);
  if fitXmin + nfit * fitXstep < fitXmax then
    nfit := nfit + 1;
  nfit := nfit + 1;
  SetLength(Xfit, nfit);
  SetLength(Yfit, nfit);
  SetLength(YfitErrors, nfit);

  NofParameters := 1 + ATrendDegree;
  for I := 0 to Length(ATrigPolyDegrees) - 1 do
    NofParameters := NofParameters + 2 * ATrigPolyDegrees[I];
  SetLength(Xvector, NofParameters);

  for I := 0 to nfit - 1 do begin
    x := fitXmin + I * fitXstep;
    Xfit[I] := x;
    Yfit[I] := 0;
    for II := 0 to ATrendDegree do begin
      Xvector[II] := Power(x, II);
      Yfit[I] := Yfit[I] + solution_vector[II] * Xvector[II];
    end;

    Idx2 := 1 + ATrendDegree;
    for N := 0 to Length(ATrigPolyDegrees) - 1 do begin
      if ATrigPolyDegrees[N] > 0 then begin
        nu := AFrequencies[N];
        for II := 1 to ATrigPolyDegrees[N] do begin
          angle := 2 * Pi * nu * x;
          Idx := Idx2 + 2 * (II - 1);
          Xvector[Idx] := Cos(II * angle);
          Xvector[Idx + 1] := Sin(II * angle);
          Yfit[I] := Yfit[I] + solution_vector[Idx] * Xvector[Idx] + solution_vector[Idx + 1] * Xvector[Idx + 1];
        end;
        Idx2 := Idx2 + ATrigPolyDegrees[N] * 2;
      end;
    end;

    // Y error
    YfitErrors[I] := 0.0;
    for Idx1 := 0 to NofParameters - 1 do begin
      for Idx2 := 0 to NofParameters - 1 do begin
        YfitErrors[I] := YfitErrors[I] + XTXI[Idx1 * NofParameters + Idx2] * Xvector[Idx1] * Xvector[Idx2];
      end;
    end;

{$IFDEF Y_ERRORS_WITH_VAR_OF_RESIDUALS}
    YfitErrors[I] := YfitErrors[I] + SigmaSq;
{$ENDIF}

    YfitErrors[I] := Sqrt(YfitErrors[I]);
  end;
end;

procedure CalcCoefficientErrors(const Xmatrix: TArbFloatArray; // design matrix
                                const Yvector: TArbFloatArray; // dependent variable
                                const beta: TArbFloatArray;    // solution vector
                                m: Integer;                    // number of equations  (rows in Xmatrix)
                                n: Integer;                    // number of parameters (columns in Xmatrix)
                                out SigmaSq: ArbFloat;         // Variance of residuals
                                out XTXI: TArbFloatArray;      // Variance-Covariance Matrix of the Coefficients
                                out Errors: TArbFloatArray);   // errors of the coefficients
var
  XmatrixTrans: TArbFloatArray;
  YvectorPredicted: TArbFloatArray;
  RSS, TempV: ArbFloat;
  C, R, Idx: Integer;
  term: Integer;
begin
  Assert(n > 0);

  if (Length(Xmatrix) <> m * n) or (Length(Yvector) <> m) or (Length(beta) <> n) then
    CalcError('Cannot calculate coefficients'' errors: invalid parameters');

  // Calculate XTXI: Variance-Covariance Matrix

  SetLength(XmatrixTrans, Length(Xmatrix));
  // XmatrixTrans is the transposed matrix
  omvtrm(Xmatrix[0], m, n, n, XmatrixTrans[0], m);

  SetLength(XTXI, n * n);
  // Mult. the transposed mutrix by the original one
  omvmmm(XmatrixTrans[0], n, m, m, Xmatrix[0], n, n, XTXI[0], n);
  // Invert the symmetric XTXI matrix
  //if n > 1 then begin
    // invgsy causes ACCESS VIOLATION if n = 1 so, we treat n = 1 as a special case. (invgen works normally even in this extreme case)
    // also, invgsy causes memory leaks.
    // invgsy(n, n, XTXI[0], term);
    // use invgen instead (performance penalty)
    invgen(n, n, XTXI[0], term);
    case term of
      1: ; // successful completion, the solution vector x is valid
      2: CalcError('"invgsy" error: ' + IntToStr(term) + ': the inverse could not be calculated because the input matrix is (almost) singular.');
      3: CalcError('"invgsy" error: ' + IntToStr(term) + ': incorrect input data, n < 1.');
    else
      CalcError('"invgsy" error: ' + IntToStr(term));
    end;
  //end
  //else
  //  XTXI[0] := 1 / XTXI[0];

  // Calculate the predicted Y vector
  SetLength(YvectorPredicted, Length(Yvector));
  for R := 0 to m - 1 do begin
     YvectorPredicted[R] := 0.0;
     for C := 0 to n - 1 do begin
       Idx := R * n + C;
       YvectorPredicted[R] := YvectorPredicted[R] + Xmatrix[Idx] * beta[C];
     end;
  end;

  // Residual Sum of Squares
  RSS := 0.0;
  for R := 0 to m - 1 do begin
    TempV := YvectorPredicted[R] - Yvector[R];
    RSS := RSS + TempV * TempV;
  end;

  // Variance of the Residuals
  SigmaSq := RSS / (m - n);

  // XTXI: Variance-Covariance Matrix
  for Idx := 0 to n * n - 1 do begin
    XTXI[Idx] := SigmaSq * XTXI[Idx];
  end;

  SetLength(Errors, Length(beta));
  for Idx := 0 to n - 1 do begin
    // Square roots of diagonal elements
    TempV := XTXI[Idx * n + Idx];
    if not IsZero(TempV) then
      Errors[Idx] := Sqrt(XTXI[Idx * n + Idx])
    else
      Errors[Idx] := 0.0;
  end;
end;

procedure PolyFit(const Xarray: TDoubleArray;
                  const Yarray: TDoubleArray;
                  ATrendDegree: Integer;
                  const ATrigPolyDegrees: TInt5Array;
                  const AFrequencies: TDouble5Array;
                  fitXmin, fitXmax, fitXstep: Double;
                  out Xfit: TDoubleArray;
                  out Yfit: TDoubleArray;
                  out YfitErrors: TDoubleArray;
                  out FitAtPoints: TDoubleArray;
                  out FitAtPointsErrors: TDoubleArray;
                  out FitAtPointsAlgebraic: TDoubleArray;
                  out Formula: string;
                  out Info: string);
var
  YarrayArbFloat: TArbFloatArray;
  a: TArbFloatArray;
  solution_vector: TArbFloatArray;
  SigmaSq: ArbFloat;
  XTXI: TArbFloatArray;
  solution_vector_errors: TArbFloatArray;
  nu, angle: ArbFloat;
  I, II, N, Idx, Idx2: Integer;
  NofParameters: Integer;
  ndata: Integer;
begin
  if Length(Xarray) <> Length(Yarray) then
    CalcError('X and Y arrays myst be of equal length');

  if (ATrendDegree < 0) then
    CalcError('Trend degree must be >= 0');

  for I := 0 to Length(ATrigPolyDegrees) - 1 do begin
    if (ATrigPolyDegrees[I] < 0) then
      CalcError('Trigonometric polynomial degree must be >= 0');
  end;

  NofParameters := 1 + ATrendDegree;
  for I := 0 to Length(ATrigPolyDegrees) - 1 do
    NofParameters := NofParameters + ATrigPolyDegrees[I] * 2;

  if NofParameters > 101 then
    CalcError('Too many parameters. Please reduce trend or trigonometric polynomial degree');

  ndata := Length(Xarray);
  SetLength(a, ndata * NofParameters);

  // Trend
  for I := 0 to ndata - 1 do begin
    for II := 0 to ATrendDegree do begin
      Idx := I * NofParameters + II;
      a[Idx] := math.Power(Xarray[I], II);
    end;
  end;
  // Trigonometric polinomials
  Idx2 := 1 + ATrendDegree;
  for N := 0 to Length(ATrigPolyDegrees) - 1 do begin
    if ATrigPolyDegrees[N] > 0 then begin
      nu := AFrequencies[N];
      for I := 0 to ndata - 1 do begin
        angle := 2 * Pi * nu * Xarray[I];
        for II := 1 to ATrigPolyDegrees[N] do begin
          Idx := I * NofParameters + Idx2 + 2 * (II - 1);
          a[Idx]     := Cos(II * angle);
          a[Idx + 1] := Sin(II * angle);
        end;
      end;
      Idx2 := Idx2 + ATrigPolyDegrees[N] * 2;
    end;
  end;

  // ArbFloat can be Extended, so we need a temporary array
  SetLength(YarrayArbFloat, ndata);
  for I := 0 to ndata - 1 do
    YarrayArbFloat[I] := Yarray[I];

  if NofParameters = 1 then begin
    // The special 'degenerated' case: a constant.
    // We use Mean to avoid a possible error in PolyFitSolution (also there is no need to use a complicated procedure)
    SetLength(solution_vector, NofParameters);
    solution_vector[0] := Math.Mean(YarrayArbFloat);
  end
  else begin
    PolyFitSolution(a, YarrayArbFloat, NofParameters, solution_vector);
  end;

  CalcCoefficientErrors(a, YarrayArbFloat, solution_vector, ndata, NofParameters, SigmaSq, XTXI, solution_vector_errors);

  CalculateFitAtPointsExt(a, solution_vector, ATrendDegree, ATrigPolyDegrees, SigmaSq, XTXI, ndata, FitAtPoints, FitAtPointsErrors, FitAtPointsAlgebraic);

  CalculateFit(fitXmin, fitXmax, fitXstep, ATrendDegree, ATRigPolyDegrees, AFrequencies, solution_vector, SigmaSq, XTXI, Xfit, Yfit, YfitErrors);

  PolyFitSolutionToFormula(ATrendDegree, ATrigPolyDegrees, AFrequencies, solution_vector, solution_vector_errors, Formula, Info);
end;

procedure TestLS;
var
  a, b, x: TArbFloatArray;
  m, n, i, j: Integer;
  b_test: TArbFloatArray;
  sum: ArbFloat;
  Log: TextFile;
begin
  AssignFile(Log, '$$$test-ls.txt');
  Rewrite(Log);
  try
    m := 4;
    n := 3;
    SetLength(a, m*n);
    SetLength(b, m);
    SetLength(b_test, m);
  //    1, 0, 1,
  //    1, 1, 1,
  //    0, 1, 0,
  //    1, 1, 0
    a[0] := 1; a[1]  := 0; a[2]  := 1;
    a[3] := 1; a[4]  := 1; a[5]  := 1;
    a[6] := 0; a[7]  := 1; a[8]  := 0;
    a[9] := 1; a[10] := 1; a[11] := 0;

    b[0] := 21;
    b[1] := 39;
    b[2] := 21;
    b[3] := 30;

    WriteLn(Log, 'Solve A x = b with the least-squares method using DGELS');
    WriteLn(Log);

    // Display input data
    WriteLn(Log, 'A = ');
    for i := 0 to m-1 do
    begin
      for j := 0 to n-1 do
        Write(Log, a[i*n + j]:10:0);
      WriteLn(Log);
    end;
    WriteLn(Log);
    WriteLn(Log, 'b = ');
    for i := 0 to m-1 do
      Write(Log, b[i]:10:0);
    WriteLn(Log);

    PolyFitSolution(a, b, n, x);

    WriteLn(Log);
    WriteLn(Log, 'Solution x = ');
    for j:= 0 to n-1 do
      Write(Log, x[j]:10:0);
    WriteLn(Log);

    WriteLn(Log);
    WriteLn(Log, 'b = ');
    for i := 0 to m-1 do
      Write(Log, b[i]:10:0);
    WriteLn(Log);

    // Calculate and display residuals
    WriteLn(Log);
    WriteLn(Log, 'Residuals A x - b = ');
    sum := 0;
    omvmmv(a[0], m, n, n, x[0], b_test[0]);
    for i:=0 to m-1 do begin
      Write(Log, (b_test[i] - b[i]):10:0);
      sum := sum + sqr(b_test[i] - b[i]);
    end;
    WriteLn(Log);

    // Sum of squared residuals
    WriteLn(Log);
    WriteLn(Log, 'Sum of squared residuals');
    WriteLn(Log, sum:10:0);

    WriteLn(Log);
    WriteLn(Log, '----------------------------------------------------------------------------');
    WriteLn(Log);

    // Modify solution to show that the sum of squared residuals increases';
    WriteLn(Log, 'Modified solution x'' (to show that it has a larger sum of squared residuals)');
    x[0] := x[0] + 1;
    x[1] := x[1] - 1;
    WriteLn(Log);
    for j:=0 to n-1 do
      Write(Log, x[j]:10:0);
    omvmmv(a[0], m, n, n, x[0], b_test[0]);
    sum := 0;
    for i:=0 to m-1 do
      sum := sum + sqr(b_test[i] - b[i]);
    WriteLn(Log);
    WriteLn(Log);
    WriteLn(Log, 'Sum of squared residuals');
    WriteLn(Log, sum:10:0);
  finally
    CloseFile(Log);
  end;
end;

{$IFDEF USE_WINMKL}
function dgels_info: string;
begin
  if Assigned(dgels_solve) then
    Result := 'Intel oneMKL DGELS routine is active.'
  else
    Result := 'The program was compiled with Intel oneMKL DGELS support, but an initialization error occurred.'
end;

var
  hMKL: THandle;
{$ENDIF}

var
  S: string;
  I: Integer;

initialization
{$IFDEF USE_WINMKL}
  dgels_solve := nil;
  // We use our own threading!
  // Check for all required DLLs
  hMKL := SafeLoadLibrary('lapack_min.dll');
  if hMKL <> 0 then begin
    dgels_solve := TDGELS_SOLVE(GetProcAddress(hMKL, 'dgels_solve'));
    if not Assigned(dgels_solve) then begin
      UnloadLibrary(hMKL);
      hMKL := 0;
    end;
  end;
{$ENDIF}

  for I := 1 to ParamCount do begin
    S := ParamStr(I);
    if S = '--test-ls' then begin
      TestLS;
      Break;
    end;
  end;

{$IFDEF USE_WINMKL}
finalization
  if hMKL <> 0 then begin
    dgels_solve := nil;
    UnloadLibrary(hMKL);
    hMKL := 0;
  end;
{$ENDIF}
end.


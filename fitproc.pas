unit fitproc;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

{.$DEFINE Y_ERRORS_WITH_VAR_OF_RESIDUALS}

interface

uses
  Classes, SysUtils, lcvtypes;

// It this vestion of PolyFit, the input array (a) must be allocated and
// initialized. fit must be allocated.
// This version of PolyFit is used in DFT.
procedure PolyFit(const a: TDoubleArray;      // 'Design matrix' (independent variables)
                  const Yarray: TDoubleArray; // Observed values
                  ATrendDegree: Integer;      // Degree of the algebraic polynomial
                  ATrigPolyDegree: Integer;   // Degree of the (single) trigonometric polynomial
                  var fit: TDoubleArray);     // Approximated values of the observations

procedure PolyFit(const Xarray: TDoubleArray;
                  const Yarray: TDoubleArray;
                  ATrendDegree: Integer;
                  const ATrigPolyDegrees: THarmonicIntArray;
                  const AFrequencies: THarmonicDblArray;
                  fitXmin, fitXmax, fitXstep: Double;
                  out Xfit: TDoubleArray;
                  out Yfit: TDoubleArray;
                  out YfitErrors: TDoubleArray;
                  out FitAtPoints: TDoubleArray;
                  out FitAtPointsErrors: TDoubleArray;
                  out FitAtPointsAlgebraic: TDoubleArray;
                  X_scale: Double;             // X-scale factor: Xarray is divided by it
                  out Formula: string;
                  out Info: string);

function CalcResidualSquared(const Observations, Model: TDoubleArray): Double;

implementation

uses
  math, la, miscutils, formatutils;

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

// The input array (a) must be allocated and initialized.
procedure PolyFitSolution(const a: TDoubleArray;              // 'Design matrix' (independent variables)
                          const Yarray: TDoubleArray;         // Observations
                          NofParameters: Integer;             // Number of parameters (independent variables)
                          var solution_vector: TDoubleArray); // Solution
var
  aa: TDoubleArray;
  m, n, nrhs, lda, ldb, info: LongInt;
  trans: AnsiChar;
  ndata: Integer;
  term: Integer;
  CW8087: WORD;
  MXCSR: DWORD;
begin
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
  // solution_vector provides the right hand side vector (B) at the beginning.
  SetLength(solution_vector, m);
  Move(Yarray[0], solution_vector[0], m * SizeOf(Double));
  // The input matrix and vector get overwritten, so copy them.
  SetLength(aa, Length(a));
  // Instead of the direct copy, transpose the matrix.
  //TransposeMatrix(a, m, n, aa);
  CW8087 := Get8087CW;
  MXCSR := GetMXCSR;
  try
    transpose_matrix(a[0], m, n, aa[0]); // This version is somewhat faster (not critically)
    dgels_solve(trans, m, n, nrhs, aa[0], lda, solution_vector[0], ldb, info);
  finally
    Set8087CW(CW8087);
    SetMXCSR(MXCSR);
  end;
  if info <> 0 then
    DgeslError('DGELS_SOLVE failed, INFO = ' + IntToStr(info), info);

  // Set the correct length of the solution vector
  SetLength(solution_vector, n);
end;

procedure PolyFitSolutionToFormula(ATrendDegree: Integer;
                                   const ATrigPolyDegrees: THarmonicIntArray;
                                   const AFrequencies: THarmonicDblArray;
                                   const solution_vector: TDoubleArray;
                                   const solution_vector_errors: TDoubleArray;
                                   const XTXI: TDoubleArray;
                                   X_scale: Double;
                                   out Formula: string;
                                   out Info: string);
var
  I, N, Idx, Idx2: Integer;
  Sign: string;
  S, S2: string;
  Amplitudes: array of array[0..2] of Double;
  C1, C2, A, var1, var2, cov12, Avar: Double;
  NofParameters: Integer;
begin
  Formula := '';
  Info := ' Coefficients'^M^J;

  NofParameters := 1 + ATrendDegree;
  for I := 0 to Length(ATrigPolyDegrees) - 1 do
    NofParameters := NofParameters + ATrigPolyDegrees[I] * 2;

  // Trend
  for I := 0 to ATrendDegree do begin
    if solution_vector[I] < 0 then
      Sign := ' - '
    else
      Sign := ' + ';
    Formula := Formula + Sign + FloatToStrLocaleIndependent(Abs(solution_vector[I] / Power(X_scale, I)));
    Info := Info + FloatToStrMod(solution_vector[I] / Power(X_scale, I)) + ^I'±'^I + Trim(FloatToStrMod(solution_vector_errors[I] / Power(X_scale, I)));
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
        S := '2*math.pi*' + FloatToStrLocaleIndependent(I * AFrequencies[N] / X_scale) + '*(t-timeZeroPoint)';
        S2 := '2*π*' + Trim(FloatToStrMod(I * AFrequencies[N] / X_scale)) + '*(t-timeZeroPoint)';
        if solution_vector[Idx] < 0 then Sign := ' - ' else Sign := ' + ';
        Formula := Formula + Sign + FloatToStrLocaleIndependent(Abs(solution_vector[Idx]))     + ' * math.cos(' + S + ')';
        Info := Info + FloatToStrMod(solution_vector[Idx]) + ^I'±'^I + Trim(FloatToStrMod(solution_vector_errors[Idx])) + ^I' * cos(' + S2 + ')' + ^M^J;
        if solution_vector[Idx + 1] < 0 then Sign := ' - ' else Sign := ' + ';
        Formula := Formula + Sign + FloatToStrLocaleIndependent(Abs(solution_vector[Idx + 1])) + ' * math.sin(' + S + ') \' + ^M^J;
        Info := Info + FloatToStrMod(solution_vector[Idx + 1]) + ^I'±'^I + Trim(FloatToStrMod(solution_vector_errors[Idx + 1])) + ^I' * sin(' + S2 + ')' + ^M^J;
        SetLength(Amplitudes, Length(Amplitudes) + 1);
        Amplitudes[Length(Amplitudes) - 1][0] := I * AFrequencies[N] / X_scale;
        Amplitudes[Length(Amplitudes) - 1][1] := Sqrt(solution_vector[Idx] * solution_vector[Idx] + solution_vector[Idx + 1] * solution_vector[Idx + 1]);

        // Amplitude error
        var1 := XTXI[Idx * NofParameters + Idx];
        var2 := XTXI[(Idx + 1) * NofParameters + Idx + 1];
        cov12:= XTXI[Idx * NofParameters + Idx + 1];
        C1 := solution_vector[Idx];
        C2 := solution_vector[Idx + 1];
        A := Amplitudes[Length(Amplitudes) - 1][1];
        Avar := (C1 * C1 * var1 + C2 * C2 * var2 + 2 * C1 * C2 * cov12) / (A * A);
        if Avar >= 0 then
          Amplitudes[Length(Amplitudes) - 1][2] := Sqrt(Avar)
        else
          Amplitudes[Length(Amplitudes) - 1][2] := NaN;
      end;
      Idx2 := Idx2 + 2 * ATrigPolyDegrees[N];
    end;
  end;
  Info := Info + ^M^J;
  for N := 0 to Length(Amplitudes) - 1 do begin
    Info := Info + 'Period = '^I + FloatToStrMod(1.0 / Amplitudes[N][0]) + ^I + ' Amplitude = '^I + FloatToStrMod(Amplitudes[N][1]) + ^I'±'^I + Trim(FloatToStrMod(Amplitudes[N][2])) + ^M^J;
  end;
end;

// 'fit' array must be allocated
procedure CalculateFitAtPoints(const a: TDoubleArray;
                               const solution_vector: TDoubleArray;
                               ATrendDegree, ATrigPolyDegree: Integer;
                               var fit: TDoubleArray);
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
// with NumLib.
// This version of PolyFit is used in DFT.
procedure PolyFit(const a: TDoubleArray;      // 'Design matrix' (independent variables)
                  const Yarray: TDoubleArray; // Observed values
                  ATrendDegree: Integer;      // Degree of the algebraic polynomial
                  ATrigPolyDegree: Integer;   // Degree of the (single) trigonometric polynomial
                  var fit: TDoubleArray);     // Approximated values of the observations
var
  solution_vector: TDoubleArray;
  NofParameters: Integer;
begin
  NofParameters := 1 + ATrendDegree + ATrigPolyDegree * 2;
  PolyFitSolution(a, Yarray, NofParameters, solution_vector);
  CalculateFitAtPoints(a, solution_vector, ATrendDegree, ATrigPolyDegree, fit);
end;

procedure CalculateFitAtPointsExt(const a: TDoubleArray;
                                  const solution_vector: TDoubleArray;
                                  ATrendDegree: Integer;
                                  const ATrigPolyDegrees: THarmonicIntArray;
                                  SigmaSq: Double;
                                  const XTXI: TDoubleArray;
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

    if fitError[I] >= 0.0 then
      fitError[I] := Sqrt(fitError[I])
    else
      fitError[I] := NaN;
  end;
end;

procedure CalculateFit(fitXmin, fitXmax, fitXstep: Double;
                       ATrendDegree: Integer;
                       const ATrigPolyDegrees: THarmonicIntArray;
                       const AFrequencies: THarmonicDblArray;
                       const solution_vector: TDoubleArray;
                       SigmaSq: Double;
                       const XTXI: TDoubleArray;
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

    if YfitErrors[I] >= 0.0 then
      YfitErrors[I] := Sqrt(YfitErrors[I])
    else
      YfitErrors[I] := NaN;
  end;
end;

procedure CalcCoefficientErrors(const Xmatrix: TDoubleArray; // design matrix
                                const Yvector: TDoubleArray; // dependent variable
                                const beta: TDoubleArray;    // solution vector
                                m: Integer;                  // number of equations  (rows in Xmatrix)
                                n: Integer;                  // number of parameters (columns in Xmatrix)
                                out SigmaSq: Double;         // Variance of residuals
                                out XTXI: TDoubleArray;      // Variance-Covariance Matrix of the Coefficients
                                out Errors: TDoubleArray);   // errors of the coefficients
var
  XmatrixTrans: TDoubleArray;
  YvectorPredicted: TDoubleArray;
  RSS, TempV: Double;
  C, R, Idx: Integer;
  info: Integer;
  CW8087: WORD;
  MXCSR: DWORD;
begin
  if (n <= 0) or (Length(Xmatrix) <> m * n) or (Length(Yvector) <> m) or (Length(beta) <> n) then
    CalcError('Cannot calculate coefficients'' errors: invalid parameters');

  // Calculate XTXI: Variance-Covariance Matrix

  SetLength(XmatrixTrans, Length(Xmatrix));
  // XmatrixTrans is the transposed matrix
  //TransposeMatrix(Xmatrix, m, n, XmatrixTrans);
  CW8087 := Get8087CW;
  MXCSR := GetMXCSR;
  try
    transpose_matrix(Xmatrix[0], m, n, XmatrixTrans[0]);
  finally
    Set8087CW(CW8087);
    SetMXCSR(MXCSR);
  end;
  SetLength(XTXI, n * n);
  // Mult. the transposed mutrix by the original one
  MultiplyMatrices(XmatrixTrans, Xmatrix, n, m, n, XTXI);
  // Invert the XTXI matrix
  CW8087 := Get8087CW;
  MXCSR := GetMXCSR;
  try
    info := invert_matrix(XTXI[0], n);
  finally
    Set8087CW(CW8087);
    SetMXCSR(MXCSR);
  end;
  if info > 0 then
    CalcError('LAPACKE_dgetrf/LAPACKE_dgetri error: ' + IntToStr(info) + ': matrix is singular.')
  else
  if info < 0 then
    CalcError('LAPACKE_dgetrf/LAPACKE_dgetri error: ' + IntToStr(info) + ': illegal arg or malloc error.');

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
    if TempV >= 0 then
      Errors[Idx] := Sqrt(TempV)
    else
      Errors[Idx] := NaN;
  end;
end;

procedure PolyFit(const Xarray: TDoubleArray;
                  const Yarray: TDoubleArray;
                  ATrendDegree: Integer;
                  const ATrigPolyDegrees: THarmonicIntArray;
                  const AFrequencies: THarmonicDblArray;
                  fitXmin, fitXmax, fitXstep: Double;
                  out Xfit: TDoubleArray;
                  out Yfit: TDoubleArray;
                  out YfitErrors: TDoubleArray;
                  out FitAtPoints: TDoubleArray;
                  out FitAtPointsErrors: TDoubleArray;
                  out FitAtPointsAlgebraic: TDoubleArray;
                  X_scale: Double;             // X-scale factor: Xarray is divided by it
                  out Formula: string;
                  out Info: string);
var
  a: TDoubleArray;
  solution_vector: TDoubleArray;
  SigmaSq: Double;
  XTXI: TDoubleArray;
  solution_vector_errors: TDoubleArray;
  nu, angle: Double;
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

  if NofParameters = 1 then begin
    // The special 'degenerated' case: a constant.
    // We use Mean to avoid a possible error in PolyFitSolution (also there is no need to use a complicated procedure)
    SetLength(solution_vector, NofParameters);
    solution_vector[0] := Math.Mean(Yarray);
  end
  else begin
    PolyFitSolution(a, Yarray, NofParameters, solution_vector);
  end;

  CalcCoefficientErrors(a, Yarray, solution_vector, ndata, NofParameters, SigmaSq, XTXI, solution_vector_errors);

  CalculateFitAtPointsExt(a, solution_vector, ATrendDegree, ATrigPolyDegrees, SigmaSq, XTXI, ndata, FitAtPoints, FitAtPointsErrors, FitAtPointsAlgebraic);

  CalculateFit(fitXmin, fitXmax, fitXstep, ATrendDegree, ATRigPolyDegrees, AFrequencies, solution_vector, SigmaSq, XTXI, Xfit, Yfit, YfitErrors);

  PolyFitSolutionToFormula(ATrendDegree, ATrigPolyDegrees, AFrequencies, solution_vector, solution_vector_errors, XTXI, X_scale, Formula, Info);
end;

end.


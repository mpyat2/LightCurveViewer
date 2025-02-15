unit common;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, StdCtrls, Dialogs, Grids, math, typ, omv, inv, sle;

type
  FitColumnType = (x, yFit, yErrors, yObserved);

type
  PFloatArray = ^TFloatArray;
  TFloatArray = array of Double;
  TInt3Array = array[0..2] of Integer;
  TDouble3Array = array[0..2] of Double;
  TFitColumnArray = array[FitColumnType] of TFloatArray;

type
  TXY = class
    X, Y: Double;
    constructor Create(AX, AY: Double);
  end;

type
  TDouble = class
    D: Double;
    constructor Create(V: Double);
  end;

function CompareXY(Item1, Item2: Pointer): Integer;

function CompareD(Item1, Item2: Pointer): Integer;

function GetMedianInterval(A: TFloatArray): Double;

function GetRecommendedFrequencyResolution(Xmin, Xmax: Double; TrigPolyDegree: Integer): Double;

// It this vestion of PolyFit, the input array (a) must be allocated and
// initialized. fit must be allocated.
procedure PolyFit(const a: TFloatArray;       // 'Design matrix' (independent variables)
                  const Yarray: TFloatArray;  // Observed values
                  ATrendDegree: Integer;      // Degree of the algebraic polynomial
                  ATrigPolyDegree: Integer;   // Degree of the (single) trigonometric polynomial
                  var fit: TFloatArray);      // Approximated values of the observations

procedure PolyFit(const Xarray: TFloatArray;
                  const Yarray: TFloatArray;
                  ATrendDegree: Integer;
                  const ATrigPolyDegrees: TInt3Array;
                  const AFrequencies: TDouble3Array;
                  fitXmin, fitXmax, fitXstep: Double;
                  out Xfit: TFloatArray;
                  out Yfit: TFloatArray;
                  out YfitErrors: TFloatArray;
                  out FitAtPoints: TFloatArray;
                  out FitAtPointsErrors: TFloatArray;
                  out Formula: string;
                  out Info: string);

function CalcResidualSquared(const Observations, Model: TFloatArray): Double;

function StringToFloatLocaleIndependent(const S: string; out V: Double): Boolean;

function FloatToStrLocaleIndependent(V: Double): string;

function FloatToStrMod(V: Double): string;

function GetFieldValue(const Field: TEdit; Min, Max: Double; const FieldName: string; out V: Double): Boolean;

function GetFieldValue(const Field: TEdit; Min, Max: Integer; const FieldName: string; out V: integer): Boolean;

type
  TGetGridCell = function(Grid: TDrawGrid; C, R: Integer): string of object;

function GetGridSelectionAsText(Grid: TDrawGrid; GetGridCell: TGetGridCell): string;

procedure CalcError(const S: string);

implementation

{ TXY }

constructor TXY.Create(AX, AY: Double);
begin
  inherited Create;
  X := AX;
  Y := AY;
end;

function CompareXY(Item1, Item2: Pointer): Integer;
begin
  if TXY(Item1).X < TXY(Item2).X then
    Result := -1
  else
  if TXY(Item1).X > TXY(Item2).X then
    Result := 1
  else
    Result := 0;
end;

{ TDouble }

constructor TDouble.Create(V: Double);
begin
  Self.D := V;
end;

function CompareD(Item1, Item2: Pointer): Integer;
begin
  if TDouble(Item1).D < TDouble(Item2).D then
    Result := -1
  else
  if TDouble(Item1).D > TDouble(Item2).D then
    Result := 1
  else
    Result := 0;
end;

function GetMedianInterval(A: TFloatArray): Double;
var
  DoubleList: TList;
  Intervals: TList;
  Interval: Double;
  I: Integer;
begin
  Result := NaN;
  if Length(A) < 1 then
    Exit;
  DoubleList := TList.Create;
  try
    for I := 0 to Length(A) - 1 do
      DoubleList.Add(TDouble.Create(A[I]));
    DoubleList.Sort(@CompareD);
    Intervals := TList.Create;
    try
      for I := 1 to DoubleList.Count - 1 do begin
        Interval := TDouble(DoubleList[I]).D - TDouble(DoubleList[I-1]).D;
        if Interval > 0 then
          Intervals.Add(TDouble.Create(Interval));
      end;
      if Intervals.Count = 0 then
        Exit;
      Intervals.Sort(@CompareD);
      // Median
      I := Intervals.Count div 2; // Central element
      if Odd(Intervals.Count) then
        Result := TDouble(Intervals[I]).D
      else
        Result := (TDouble(Intervals[I - 1]).D + TDouble(Intervals[I]).D) / 2;
    finally
      for I := Intervals.Count - 1 downto 0 do begin
        TDouble(Intervals[I]).Free;
        Intervals[I] := nil;
      end;
      FreeAndNil(Intervals);
    end;
  finally
    for I := DoubleList.Count - 1 downto 0 do begin
      TDouble(Doublelist[I]).Free;
      Doublelist[I] := nil;
    end;
    FreeAndNil(DoubleList);
  end;
end;

function GetRecommendedFrequencyResolution(Xmin, Xmax: Double; TrigPolyDegree: Integer): Double;
begin
  if (Xmax > Xmin) and (TrigPolyDegree > 0) then
    Result := 0.05 / (Xmax - Xmin) / TrigPolyDegree
  else
    Result := NaN;
end;

////////////////////////////////////////////////////////////////////////////////
// Approximation
////////////////////////////////////////////////////////////////////////////////

function CalcResidualSquared(const Observations, Model: TFloatArray): Double;
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
procedure PolyFitSolution(const a: TFloatArray;              // 'Design matrix' (independent variables)
                          const Yarray: TFloatArray;         // Observations
                          NofParameters: Integer;            // Number of parameters (independent variables)
                          out solution_vector: TFloatArray); // Solution
var
  ndata: Integer;
  term: typ.ArbInt;
begin
  ndata := Length(Yarray);
  SetLength(solution_vector, NofParameters);
  // solve for overdetermined matrices
  slegls(a[0], ndata, NofParameters, NofParameters, Yarray[0], solution_vector[0], term);
  case term of
    1: ; // successful completion, the solution vector x is valid
    2: CalcError('"slegls" error: ' + IntToStr(term) + ': there is no unambiguous solution because the columns of the matrix are linearly dependant.');
    3: CalcError('"slegls" error: ' + IntToStr(term) + ': error in input values: n < 1, or n > m.');
  else
    CalcError('"slegls" error: ' + IntToStr(term));
  end;
end;

procedure PolyFitSolutionToFormula(ATrendDegree: Integer;
                                   const ATrigPolyDegrees: TInt3Array;
                                   const AFrequencies: TDouble3Array;
                                   const solution_vector: TFloatArray;
                                   const solution_vector_errors: TFloatArray;
                                   out Formula: string;
                                   out Info: string);
var
  I, N, Idx, Idx2: Integer;
  Sign: string;
  S, S2: string;
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
      end;
      Idx2 := Idx2 + 2 * ATrigPolyDegrees[N];
    end;
  end;
end;

// 'fit' array must be allocated
procedure CalculateFitAtPoints(const a: TFloatArray;
                               const solution_vector: TFloatArray;
                               ATrendDegree, ATrigPolyDegree: Integer;
                               var fit: TFloatArray);
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
procedure PolyFit(const a: TFloatArray;       // 'Design matrix' (independent variables)
                  const Yarray: TFloatArray;  // Observed values
                  ATrendDegree: Integer;      // Degree of the algebraic polynomial
                  ATrigPolyDegree: Integer;   // Degree of the (single) trigonometric polynomial
                  var fit: TFloatArray);      // Approximated values of the observations
var
  solution_vector: TFloatArray;
  NofParameters: Integer;
begin
  NofParameters := 1 + ATrendDegree + ATrigPolyDegree * 2;
  PolyFitSolution(a, Yarray, NofParameters, solution_vector);
  CalculateFitAtPoints(a, solution_vector, ATrendDegree, ATrigPolyDegree, fit);
end;

procedure CalculateFitAtPointsExt(const a: TFloatArray;
                                  const solution_vector: TFloatArray;
                                  ATrendDegree: Integer;
                                  const ATrigPolyDegrees: TInt3Array;
                                  const XTXI: TFloatArray;
                                  ndata: Integer;
                                  out fit: TFloatArray;
                                  out fitError: TFloatArray);
var
  I, II, N, Idx, Idx1, Idx2: Integer;
  NofParameters: Integer;
begin
  NofParameters := 1 + ATrendDegree;
  for I := 0 to Length(ATrigPolyDegrees) - 1 do
    NofParameters := NofParameters + ATrigPolyDegrees[I] * 2;

  SetLength(fit, ndata);
  SetLength(fitError, ndata);

  for I := 0 to Length(fit) - 1 do begin
    fit[I] := 0.0;
    for II := 0 to ATrendDegree do begin
      Idx := I * NofParameters + II;
      fit[I] := fit[I] + solution_vector[II] * a[Idx];
    end;

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

    fitError[I] := Sqrt(fitError[I]);
  end;
end;

procedure CalculateFit(fitXmin, fitXmax, fitXstep: Double;
                       ATrendDegree: Integer;
                       const ATrigPolyDegrees: TInt3Array;
                       const AFrequencies: TDouble3Array;
                       const solution_vector: TFloatArray;
                       const XTXI: TFloatArray;
                       out Xfit: TFloatArray;
                       out Yfit: TFloatArray;
                       out YfitErrors: TFloatArray);
var
  Xvector: TFloatArray;
  x, nu, angle: Double;
  I, II, N, Idx, Idx1, Idx2, nfit: Integer;
  NofParameters: Integer;
begin
  nfit := Ceil((fitXmax - fitXmin) / fitXstep);
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

    YfitErrors[I] := Sqrt(YfitErrors[I]);
  end;
end;

procedure CalcCoefficientErrors(const Xmatrix: TFloatArray; // design matrix
                                const Yvector: TFloatArray; // dependent variable
                                const beta: TFloatArray;    // solution vector
                                m: Integer;                 // number of equations  (rows in Xmatrix)
                                n: Integer;                 // number of parameters (columns in Xmatrix)
                                out XTXI: TFloatArray;      // Variance-Covariance Matrix of the Coefficients
                                out Errors: TFloatArray);   // errors of the coefficients
var
  XmatrixTrans: TFloatArray;
  YvectorPredicted: TFloatArray;
  RSS, SigmaSq, TempV: Double;
  C, R, Idx: Integer;
  term: typ.ArbInt;
begin
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
  invgsy(n, n, XTXI[0], term);
  case term of
    1: ; // successful completion, the solution vector x is valid
    2: CalcError('"invgsy" error: ' + IntToStr(term) + ': the inverse could not be calculated because the input matrix is (almost) singular.');
    3: CalcError('"invgsy" error: ' + IntToStr(term) + ': incorrect input data, n < 1.');
  else
    CalcError('"invgsy" error: ' + IntToStr(term));
  end;

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
    Errors[Idx] := Sqrt(XTXI[Idx * n + Idx]); // Square roots of diagonal elements
  end;
end;

procedure PolyFit(const Xarray: TFloatArray;
                  const Yarray: TFloatArray;
                  ATrendDegree: Integer;
                  const ATrigPolyDegrees: TInt3Array;
                  const AFrequencies: TDouble3Array;
                  fitXmin, fitXmax, fitXstep: Double;
                  out Xfit: TFloatArray;
                  out Yfit: TFloatArray;
                  out YfitErrors: TFloatArray;
                  out FitAtPoints: TFloatArray;
                  out FitAtPointsErrors: TFloatArray;
                  out Formula: string;
                  out Info: string);
var
  nfit: Integer;
  a: TFloatArray;
  solution_vector: TFloatArray;
  XTXI: TFloatArray;
  solution_vector_errors: TFloatArray;
  nu, angle, c, s: Double;
  x: Double;
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

  PolyFitSolution(a, Yarray, NofParameters, solution_vector);

  CalcCoefficientErrors(a, Yarray, solution_vector, ndata, NofParameters, XTXI, solution_vector_errors);

  CalculateFitAtPointsExt(a, solution_vector, ATrendDegree, ATrigPolyDegrees, XTXI, ndata, FitAtPoints, FitAtPointsErrors);

  CalculateFit(fitXmin, fitXmax, fitXstep, ATrendDegree, ATRigPolyDegrees, AFrequencies, solution_vector, XTXI, Xfit, Yfit, YfitErrors);

  PolyFitSolutionToFormula(ATrendDegree, ATrigPolyDegrees, AFrequencies, solution_vector, solution_vector_errors, Formula, Info);
end;

////////////////////////////////////////////////////////////////////////////////
// Format functions
////////////////////////////////////////////////////////////////////////////////

function StringToFloatLocaleIndependent(const S: string; out V: Double): Boolean;
var
  Code: Integer;
begin
  Val(S, V, Code);
  Result := Code = 0;
end;

function FloatToStrLocaleIndependent(V: Double): string;
var
  F: TFormatSettings;
begin
  GetLocaleFormatSettings(0409, F);
  if Abs(V) > 0.0000001 then
    Result := FloatToStrF(V, ffFixed, 0, 15, F)
  else
    Result := FloatToStr(V, F);
end;

function FloatToStrMod(V: Double): string;
begin
  if Abs(V) > 0.0000001 then
    Result := FloatToStrF(V, ffFixed, 0, 15)
  else
    Result := FloatToStr(V);
  if V >= 0 then
    Result := ' ' + Result;
end;

////////////////////////////////////////////////////////////////////////////////
// GUI-related functions
////////////////////////////////////////////////////////////////////////////////

function GetFieldValue(const Field: TEdit; Min, Max: Double; const FieldName: string; out V: Double): Boolean;
var
  S: string;
begin
  Result := False;
  S := Trim(Field.Text);
  if not TryStrToFloat(S, V) then begin
    Field.SetFocus;
    Field.SelectAll;
    ShowMessage(FieldName + ': Invalid value');
    Exit;
  end;
  if (not IsNaN(Min)) and (V < Min) then begin
    Field.SetFocus;
    Field.SelectAll;
    ShowMessage(FieldName + ': Value must be greater than or equal to ' + FloatToStr(Min));
    Exit;
  end;
  if (not IsNaN(Max)) and (V > Max) then begin
    Field.SetFocus;
    Field.SelectAll;
    ShowMessage(FieldName + ': Value must be less than or equal to ' + FloatToStr(Max));
    Exit;
  end;
  Result := True;
end;

function GetFieldValue(const Field: TEdit; Min, Max: Integer; const FieldName: string; out V: integer): Boolean;
var
  S: string;
begin
  Result := False;
  S := Trim(Field.Text);
  if not TryStrToInt(S, V) then begin
    Field.SetFocus;
    Field.SelectAll;
    ShowMessage(FieldName + ': Invalid value');
    Exit;
  end;
  if V < Min then begin
    Field.SetFocus;
    Field.SelectAll;
    ShowMessage(FieldName + ': Value must be greater than or equal to ' + FloatToStr(Min));
    Exit;
  end;
  if V > Max then begin
    Field.SetFocus;
    Field.SelectAll;
    ShowMessage(FieldName + ': Value must be less than or equal to ' + FloatToStr(Max));
    Exit;
  end;
  Result := True;
end;

function GetGridSelectionAsText(Grid: TDrawGrid; GetGridCell: TGetGridCell): string;
var
  Selection: TRect;
  R, C: Integer;
  S2: string;
begin
  Result := '';
  Selection := Grid.Selection;
  for R := Selection.Top to Selection.Bottom do begin
    S2 := '';
    for C := Selection.Left to Selection.Right do begin
      S2 := S2 + GetGridCell(Grid, C, R);
      if C < Selection.Right then
        S2 := S2 + ^I;
    end;
    Result := Result + S2 + ^M^J;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Error messages, etc.
////////////////////////////////////////////////////////////////////////////////

procedure CalcError(const S: string);
begin
  raise Exception.Create(S);
end;

end.


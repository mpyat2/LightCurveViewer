unit common;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, StdCtrls, Dialogs, Grids, math, typ, sle;

type
  PFloatArray = ^TFloatArray;
  TFloatArray = array of Double;
  TInt3Array = array[0..2] of Integer;
  TDouble3Array = array[0..2] of Double;

// It this vestion of PolyFit, the input array (a) must be allocated and
// initialized. fit must be allocated.
procedure PolyFit(const Xarray: TFloatArray;
                  const Yarray: TFloatArray;
                  nu: Double;
                  ATrendDegree: Integer;
                  ATrigPolyDegree: Integer;
                  const a: TFloatArray;
                  var fit: TFloatArray);

procedure PolyFit(const Xarray: TFloatArray;
                  const Yarray: TFloatArray;
                  ATrendDegree: Integer;
                  ATrigPolyDegrees: TInt3Array;
                  AFrequencies: TDouble3Array;
                  fitXmin, fitXmax, fitXstep: Double;
                  out Xfit: TFloatArray;
                  out Yfit: TFloatArray;
                  out FitAtPoints: TFloatArray;
                  out Formula: string;
                  out Info: string);

function CalcResidualSquared(const Observations, Model: TFloatArray): Double;

function FloatToStrLocaleIndependent(V: Double): string;

procedure CalcError(const S: string);

function GetFieldValue(const Field: TEdit; Min, Max: Double; const FieldName: string; out V: Double): Boolean;

function GetFieldValue(const Field: TEdit; Min, Max: Integer; const FieldName: string; out V: integer): Boolean;

type
  TGetGridCell = function(Grid: TDrawGrid; C, R: Integer): string of object;

function GetGridSelectionAsText(Grid: TDrawGrid; GetGridCell: TGetGridCell): string;

implementation

procedure CalcError(const S: string);
begin
  raise Exception.Create(S);
end;

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
procedure PolyFitSolution(const Xarray: TFloatArray;
                          const Yarray: TFloatArray;
                          NofParameters: Integer;
                          const a: TFloatArray;
                          out solution_vector: TFloatArray);
var
  ndata: Integer;
  term: typ.ArbInt;
  I, II, Idx: Integer;
begin
  ndata := Length(Xarray);
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

function FloatToStrLocaleIndependent(V: Double): string;
begin
  Str(V:0:15, Result);
end;

procedure PolyFitSolutionToFormula(ATrendDegree: Integer;
                                   ATrigPolyDegrees: TInt3Array;
                                   AFrequencies: TDouble3Array;
                                   const solution_vector: TFloatArray;
                                   out Formula: string;
                                   out Info: string);
var
  I, N, Idx, Idx2: Integer;
  Sign: string;
  S: string;
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
    Info := Info + Trim(Sign) + FloatToStrLocaleIndependent(Abs(solution_vector[I]));
    if I > 0 then begin
      Formula := Formula + ' * (t-timeZeroPoint)**' + IntToStr(I);
      Info := Info + ' * (t-timeZeroPoint)^' + IntToStr(I);
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
        if solution_vector[Idx] < 0 then Sign := ' - ' else Sign := ' + ';
        Formula := Formula + Sign + FloatToStrLocaleIndependent(Abs(solution_vector[Idx]))     + ' * math.cos(' + S + ')';
        Info := Info + Trim(Sign) + FloatToStrLocaleIndependent(Abs(solution_vector[Idx])) + ' * cos(' + S + ')' + ^M^J;
        if solution_vector[Idx + 1] < 0 then Sign := ' - ' else Sign := ' + ';
        Formula := Formula + Sign + FloatToStrLocaleIndependent(Abs(solution_vector[Idx + 1])) + ' * math.sin(' + S + ') \' + ^M^J;
        Info := Info + Trim(Sign) + FloatToStrLocaleIndependent(Abs(solution_vector[Idx + 1])) + ' * sin(' + S + ')' + ^M^J;
      end;
      Idx2 := Idx2 + 2 * ATrigPolyDegrees[N];
    end;
  end;
end;

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

procedure CalculateFitAtPointsExt(const a: TFloatArray;
                                  const solution_vector: TFloatArray;
                                  ATrendDegree: Integer;
                                  ATrigPolyDegrees: TInt3Array;
                                  var fit: TFloatArray);
var
  I, II, N, Idx, Idx1, Idx2: Integer;
  NofParameters: Integer;
begin
  NofParameters := 1 + ATrendDegree;
  for I := 0 to Length(ATrigPolyDegrees) - 1 do
    NofParameters := NofParameters + ATrigPolyDegrees[I] * 2;

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
  end;
end;

// It this vestion of PolyFit, the input array (a) must be allocated and
// initialized. fit must be allocated.
procedure PolyFit(const Xarray: TFloatArray;
                  const Yarray: TFloatArray;
                  nu: Double;
                  ATrendDegree: Integer;
                  ATrigPolyDegree: Integer;
                  const a: TFloatArray;
                  var fit: TFloatArray);
var
  ndata: Integer;
  angle: Double;
  solution_vector: TFloatArray;
  I, II, Idx, Idx2: Integer;
  NofParameters: Integer;
begin
  NofParameters := 1 + ATrendDegree + ATrigPolyDegree * 2;
  PolyFitSolution(Xarray, Yarray, NofParameters, a, solution_vector);
  CalculateFitAtPoints(a, solution_vector, ATrendDegree, ATrigPolyDegree, fit);
end;

procedure PolyFit(const Xarray: TFloatArray;
                  const Yarray: TFloatArray;
                  ATrendDegree: Integer;
                  ATrigPolyDegrees: TInt3Array;
                  AFrequencies: TDouble3Array;
                  fitXmin, fitXmax, fitXstep: Double;
                  out Xfit: TFloatArray;
                  out Yfit: TFloatArray;
                  out FitAtPoints: TFloatArray;
                  out Formula: string;
                  out Info: string);
var
  nfit: Integer;
  a: TFloatArray;
  solution_vector: TFloatArray;
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

  if NofParameters > 51 then
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

  PolyFitSolution(Xarray, Yarray, NofParameters, a, solution_vector);
  SetLength(FitAtPoints, ndata);
  CalculateFitAtPointsExt(a, solution_vector, ATrendDegree, ATrigPolyDegrees, FitAtPoints);

  nfit := Ceil((fitXmax - fitXmin) / fitXstep);
  SetLength(Xfit, nfit);
  SetLength(Yfit, nfit);

  for I := 0 to nfit - 1 do begin
    x := fitXmin + I * fitXstep;
    Xfit[I] := x;
    Yfit[I] := 0;
    for II := 0 to ATrendDegree do begin
      Yfit[I] := Yfit[I] + solution_vector[II] * Power(x, II);
    end;

    Idx2 := 1 + ATrendDegree;
    for N := 0 to Length(ATrigPolyDegrees) - 1 do begin
      if ATrigPolyDegrees[N] > 0 then begin
        nu := AFrequencies[N];
        for II := 1 to ATrigPolyDegrees[N] do begin
          angle := 2 * Pi * nu * x;
          Idx := Idx2 + 2 * (II - 1);
          c := Cos(II * angle);
          s := Sin(II * angle);
          Yfit[I] := Yfit[I] + solution_vector[Idx] * c + solution_vector[Idx + 1] * s;
        end;
        Idx2 := Idx2 + ATrigPolyDegrees[N] * 2;
      end;
    end;
  end;

  PolyFitSolutionToFormula(ATrendDegree, ATrigPolyDegrees, AFrequencies, solution_vector, Formula, Info);
end;

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

end.


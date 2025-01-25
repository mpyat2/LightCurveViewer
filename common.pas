unit common;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, StdCtrls, Dialogs, Grids, math, typ, sle;

type
  PFloatArray = ^TFloatArray;
  TFloatArray = array of ArbFloat;

procedure PolyFit(const Xarray: TFloatArray;
                  const Yarray: TFloatArray;
                  nu: ArbFloat;
                  ATrendDegree: ArbInt;
                  ATrigPolyDegree: ArbInt;
                  out trend: TFloatArray;
                  out trig_fit: TFloatArray);

procedure PolyFit(const Xarray: TFloatArray;
                  const Yarray: TFloatArray;
                  nu: ArbFloat;
                  ATrendDegree: ArbInt;
                  ATrigPolyDegree: ArbInt;
                  fitXmin, fitXmax, fitXstep: Double;
                  out Xfit: TFloatArray;
                  out Yfit: TFloatArray;
                  out Formula: string);

procedure CalcError(const S: string);

function GetFieldValue(const Field: TEdit; Min, Max: Double; const FieldName: string; out V: Double): Boolean;

function GetFieldValue(const Field: TEdit; Min, Max: Integer; const FieldName: string; out V: integer): Boolean;

type
  TGetGridCell = function(C, R: Integer): string of object;

function GetGridSelectionAsText(Grid: TDrawGrid; GetGridCell: TGetGridCell): string;

implementation

// to-do: get rid of the fixed-length TFloat21
type
  TFloat21 = array[0..20] of ArbFloat;
  TFloat21Array = array of TFloat21;

procedure CalcError(const S: string);
begin
  raise Exception.Create(S);
end;

procedure PolyFitSolution(const Xarray: TFloatArray;
                          const Yarray: TFloatArray;
                          nu: ArbFloat;
                          ATrendDegree: ArbInt;
                          ATrigPolyDegree: ArbInt;
                          out a: TFloat21Array;
                          out solution_vector: TFloat21);
var
  ndata: ArbInt;
  angle: ArbFloat;
  term: ArbInt;
  I, II, Idx: ArbInt;
begin
  if (ATrendDegree < 0) then
    CalcError('Trend degree must be >= 0');

  if (ATrigPolyDegree < 0) then
    CalcError('Trigonometric polynomial degree must be >= 0');

  if (1 + ATrendDegree + ATrigPolyDegree * 2) > Length(TFloat21) then
    CalcError('Too many parameters. Please reduce trend or trigonometric polynomial degree');

  if Length(Xarray) <> Length(Yarray) then
    CalcError('X and Y arrays must be of equal length');

  ndata := Length(Xarray);
  SetLength(a, ndata);

  // Trend
  // to-do: for the periodogram: calculate the trend basic functions once and pass them to the procedure.
  for I := 0 to ndata - 1 do begin
    for II := 0 to ATrendDegree do begin
      a[I][II] := math.Power(Xarray[I], II);
    end;
  end;

  for I := 0 to ndata - 1 do begin
    angle := 2 * Pi * nu * Xarray[I];
    for II := 1 to ATrigPolyDegree do begin
      Idx := 1 + ATrendDegree + 2 * (II - 1);
      a[I][Idx]     := Cos(II * angle);
      a[I][Idx + 1] := Sin(II * angle);
    end;
  end;

  // solve for overdetermined matrices
  slegls(a[0, 0], ndata, 1 + ATrendDegree + ATrigPolyDegree * 2, Length(TFloat21), Yarray[0], solution_vector[0], term);
  case term of
    1: ; // successful completion, the solution vector x is valid
    2: CalcError('"slegls" error: ' + IntToStr(term) + ': there is no unambiguous solution because the columns of the matrix are linearly dependant.');
    3: CalcError('"slegls" error: ' + IntToStr(term) + ': error in input values: n < 1, or n > m.');
  else
    CalcError('"slegls" error: ' + IntToStr(term));
  end;
end;

function FloatToStrMod(V: ArbFloat): string;
begin
  //Result := FloatToStrF(V, ffFixed, 0, 15);
  Result := FloatToStr(V);
end;

function PolyFitSolutionToFormula(ATrendDegree: ArbInt; ATrigPolyDegree: ArbInt; nu: ArbFloat; const solution_vector: TFloat21): string;
var
  I, Idx: Integer;
  Sign: string;
  S: string;
begin
  Result := '';
  for I := 0 to ATrendDegree do begin
    if solution_vector[I] < 0 then
      Sign := ' - '
    else begin
      if I > 0 then Sign := ' + ' else Sign := '   ';
    end;
    Result := Result + Sign + FloatToStrMod(Abs(solution_vector[I]));
    if I > 0 then
      Result := Result + ' * (t-timeZeroPoint)^' + IntToStr(I);
    Result := Result + ^M^J;
  end;
  for I := 1 to ATrigPolyDegree do begin
    Idx := 1 + ATrendDegree + 2 * (I - 1);
    S := '2*Pi*' + FloatToStrMod(I * nu) + '*(t-timeZeroPoint)';
    if solution_vector[Idx] < 0 then Sign := ' - ' else Sign := ' + ';
    Result := Result + Sign + FloatToStrMod(Abs(solution_vector[Idx]))     + ' * cos(' + S + ')';
    if solution_vector[Idx + 1] < 0 then Sign := ' - ' else Sign := ' + ';
    Result := Result + Sign + FloatToStrMod(Abs(solution_vector[Idx + 1])) + ' * sin(' + S + ')' + ^M^J;
  end;
end;

procedure PolyFit(const Xarray: TFloatArray;
                  const Yarray: TFloatArray;
                  nu: ArbFloat;
                  ATrendDegree: ArbInt;
                  ATrigPolyDegree: ArbInt;
                  out trend: TFloatArray;
                  out trig_fit: TFloatArray);
var
  ndata: ArbInt;
  a: TFloat21Array; // to-do: get rid of the fixed-length TFloat21
  solution_vector: TFloat21;
  angle: ArbFloat;
  term: ArbInt;
  I, II, Idx: ArbInt;
begin
  PolyFitSolution(Xarray, Yarray, nu, ATrendDegree, ATrigPolyDegree, a, solution_vector);

  ndata := Length(Xarray);

  SetLength(trig_fit, ndata);
  SetLength(trend, ndata);

  for I := 0 to ndata - 1 do begin
    trig_fit[I] := 0.0;
    for II := 1 to ATrigPolyDegree do begin
      Idx := 1 + ATrendDegree + 2 * (II - 1);
      trig_fit[I] := trig_fit[I] + solution_vector[Idx] * a[I][Idx] + solution_vector[Idx + 1] * a[I][Idx + 1];
    end;
  end;

  for I := 0 to ndata - 1 do begin
    trend[I] := 0.0;
    for II := 0 to ATrendDegree do begin
      trend[I] := trend[I] + solution_vector[II] * a[I][II];
    end;
  end;
end;

procedure PolyFit(const Xarray: TFloatArray;
                  const Yarray: TFloatArray;
                  nu: ArbFloat;
                  ATrendDegree: ArbInt;
                  ATrigPolyDegree: ArbInt;
                  fitXmin, fitXmax, fitXstep: Double;
                  out Xfit: TFloatArray;
                  out Yfit: TFloatArray;
                  out Formula: string);
var
  ndata, nfit: ArbInt;
  a: TFloat21Array; // to-do: get rid of the fixed-length TFloat21
  solution_vector: TFloat21;
  angle, c, s: ArbFloat;
  x: ArbFloat;
  term: ArbInt;
  I, II, Idx: ArbInt;
begin
  PolyFitSolution(Xarray, Yarray, nu, ATrendDegree, ATrigPolyDegree, a, solution_vector);

  ndata := Length(Xarray);

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
    for II := 1 to ATrigPolyDegree do begin
      angle := 2 * Pi * nu * x;
      Idx := 1 + ATrendDegree + 2 * (II - 1);
      c := Cos(II * angle);
      s := Sin(II * angle);
      Yfit[I] := Yfit[I] + solution_vector[Idx] * c + solution_vector[Idx + 1] * s;
    end;
  end;

  Formula := PolyFitSolutionToFormula(ATrendDegree, ATrigPolyDegree, nu, solution_vector);
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
      S2 := S2 + GetGridCell(C, R);
      if C < Selection.Right then
        S2 := S2 + ^I;
    end;
    Result := Result + S2 + ^M^J;
  end;
end;

end.


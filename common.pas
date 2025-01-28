unit common;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, StdCtrls, Dialogs, Grids, math, typ, sle;

type
  PFloatArray = ^TFloatArray;
  TFloatArray = array of ArbFloat;

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
                  nu: Double;
                  ATrendDegree: Integer;
                  ATrigPolyDegree: Integer;
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

procedure CalcError(const S: string);
begin
  raise Exception.Create(S);
end;


// The input array (a) must be allocated and initialized.
procedure PolyFitSolution(const Xarray: TFloatArray;
                          const Yarray: TFloatArray;
                          NofParameters: Integer;
                          const a: TFloatArray;
                          out solution_vector: TFloatArray);
var
  ndata: Integer;
  term: ArbInt;
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

function FloatToStrMod(V: Double): string;
begin
  //Result := FloatToStrF(V, ffFixed, 0, 15);
  Result := FloatToStr(V);
end;

function PolyFitSolutionToFormula(ATrendDegree: Integer; ATrigPolyDegree: Integer; nu: Double; const solution_vector: TFloatArray): string;
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

  ndata := Length(Xarray);

  for I := 0 to ndata - 1 do begin
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

procedure PolyFit(const Xarray: TFloatArray;
                  const Yarray: TFloatArray;
                  nu: Double;
                  ATrendDegree: Integer;
                  ATrigPolyDegree: Integer;
                  fitXmin, fitXmax, fitXstep: Double;
                  out Xfit: TFloatArray;
                  out Yfit: TFloatArray;
                  out Formula: string);
var
  nfit: Integer;
  a: TFloatArray;
  solution_vector: TFloatArray;
  angle, c, s: Double;
  x: Double;
  I, II, Idx: Integer;
  NofParameters: Integer;
  ndata: Integer;
begin
  if Length(Xarray) <> Length(Yarray) then
    CalcError('X and Y arrays myst be of equal length');

  if (ATrendDegree < 0) then
    CalcError('Trend degree must be >= 0');

  if (ATrigPolyDegree < 0) then
    CalcError('Trigonometric polynomial degree must be >= 0');

  NofParameters := 1 + ATrendDegree + ATrigPolyDegree * 2;

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
  // Trigonometric polinomial
  for I := 0 to ndata - 1 do begin
    angle := 2 * Pi * nu * Xarray[I];
    for II := 1 to ATrigPolyDegree do begin
      Idx := I * NofParameters + 1 + ATrendDegree + 2 * (II - 1);
      a[Idx]     := Cos(II * angle);
      a[Idx + 1] := Sin(II * angle);
    end;
  end;
  PolyFitSolution(Xarray, Yarray, NofParameters, a, solution_vector);

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


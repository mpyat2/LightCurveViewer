unit guiutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Grids, Dialogs;

type
  TGetGridCell = function(Grid: TDrawGrid; C, R: Integer): string of object;

function GetFieldValue(const Field: TEdit; Min, Max: Double; const FieldName: string; out V: Double): Boolean;

function GetFieldValue(const Field: TEdit; Min, Max: Integer; const FieldName: string; out V: integer): Boolean;

function GetGridSelectionAsText(Grid: TDrawGrid; GetGridCell: TGetGridCell): string;

implementation

uses
  math;

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


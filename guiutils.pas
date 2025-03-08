unit guiutils;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Grids, Forms, Dialogs;

type

  { TWaitCursor }

  TWaitCursor = class(TInterfacedObject)
  private
    FCursor: TCursor;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type
  TGetGridCell = function(Grid: TDrawGrid; C, R: Integer): string of object;

function GetFieldValue(const Field: TEdit; Min, Max: Double; const FieldName: string; out V: Double): Boolean;

function GetFieldValue(const Field: TEdit; Min, Max: Integer; const FieldName: string; out V: integer): Boolean;

function GetGridSelectionAsText(Grid: TDrawGrid; GetGridCell: TGetGridCell; Titles: Boolean = False): string;

procedure RearrangeButtons(ButtonOK, ButtonCancel: TButton);

procedure RearrangeButtons(ButtonOK, ButtonCancel, ButtonApply: TButton);

implementation

uses
  math;

{ TWaitCursor }

constructor TWaitCursor.Create;
begin
  inherited;
  FCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
end;

destructor TWaitCursor.Destroy;
begin
  Screen.Cursor := FCursor;
  inherited Destroy;
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

function GetGridSelectionAsText(Grid: TDrawGrid; GetGridCell: TGetGridCell; Titles: Boolean = False): string;
var
  Selection: TRect;
  R, C: Integer;
  S2: string;
begin
  Result := '';
  Selection := Grid.Selection;
  if Titles then begin
    for R := 0 to Grid.FixedRows - 1 do begin
      S2 := '';
      for C := 0 to Grid.FixedCols - 1 do
        S2 := S2 + GetGridCell(Grid, C, R) + ^I;
    end;
    for C := Selection.Left to Selection.Right do begin
      S2 := S2 + GetGridCell(Grid, C, R);
      if C < Selection.Right then
        S2 := S2 + ^I;
    end;
    Result := Result + S2 + ^M^J;
  end;
  for R := Selection.Top to Selection.Bottom do begin
    S2 := '';
    if Titles then begin
      for C := 0 to Grid.FixedCols - 1 do
        S2 := S2 + GetGridCell(Grid, C, R) + ^I;
    end;
    for C := Selection.Left to Selection.Right do begin
      S2 := S2 + GetGridCell(Grid, C, R);
      if C < Selection.Right then
        S2 := S2 + ^I;
    end;
    Result := Result + S2;
    if R < Selection.Bottom then
      Result := Result + ^M^J;
  end;
end;

procedure RearrangeButtons(ButtonOK, ButtonCancel: TButton);
{$IF defined(linux)}
var
  LOK: Integer;
{$ENDIF}
begin
{$IF defined(linux)}
  LOK := ButtonOK.Left;
  ButtonOK.Left := ButtonCancel.Left;
  ButtonCancel.Left := LOK;
  if ButtonCancel.Left < ButtonOK.Left then
    ButtonCancel.TabOrder := ButtonOK.TabOrder;
{$ENDIF}
end;

procedure RearrangeButtons(ButtonOK, ButtonCancel, ButtonApply: TButton);
{$IF defined(linux)}
var
  LOK, LCancel: Integer;
{$ENDIF}
begin
{$IF defined(linux)}
  LOK := ButtonOK.Left;
  LCancel := ButtonCancel.Left;
  ButtonOK.Left := ButtonApply.Left;
  ButtonApply.Left := LCancel;
  ButtonCancel.Left := LOK;
  if ButtonCancel.Left < ButtonOK.Left then
    ButtonCancel.TabOrder := ButtonOK.TabOrder;
  if ButtonApply.Left < ButtonOK.Left then
    ButtonApply.TabOrder := ButtonOK.TabOrder;
{$ENDIF}
end;

end.


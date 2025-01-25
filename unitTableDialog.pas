unit unitTableDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Menus, ActnList,
  common;

type

  { TFormTable }

  TFormTable = class(TForm)
    ActionCopyAll: TAction;
    ActionList1: TActionList;
    DrawGrid1: TDrawGrid;
    MenuItemCopy: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure ActionCopyAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FX: TFloatArray;
    FY: TFloatArray;
    FXname, FYname: string;
    function GetGridCell(C, R: Integer): string;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  public

  end;

procedure ShowTable(const X, Y: TFloatArray; const Xname, Yname: string);

implementation

{$R *.lfm}

uses
  Clipbrd;

procedure ShowTable(const X, Y: TFloatArray; const Xname, Yname: string);
var
  F: TFormTable;
begin
  if Length(X) <> Length(Y) then
    raise Exception.Create('X and Y arrays must be of equal length');
  F := TFormTable.Create(Application);
  try
    F.FX := X;
    F.FY := Y;
    F.FXname := XName;
    F.FYname := YName;
    F.DrawGrid1.RowCount := F.DrawGrid1.FixedRows + Length(X);
    F.ShowModal;
  finally
    FreeAndNil(F);
  end;
end;

{ TFormTable }

procedure TFormTable.FormCreate(Sender: TObject);
begin
  DrawGrid1.OnDrawCell := @GridDrawCell;
end;

procedure TFormTable.ActionCopyAllExecute(Sender: TObject);
var
  CurrentCursor: TCursor;
  S, S1: string;
  C, R: Integer;
begin
  CurrentCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    S := '';
    for R := 0 to DrawGrid1.RowCount - 1 do begin
      S1 := '';
      for C := 0 to DrawGrid1.ColCount - 1 do begin
        S1 := S1 + GetGridCell(C, R);
        if C < DrawGrid1.ColCount - 1 then
          S1 := S1 + ^I;
      end;
      S := S + S1 + ^M^J;
    end;
    Clipboard.AsText := S;
  finally
    Screen.Cursor := CurrentCursor;
  end;
end;

function TFormTable.GetGridCell(C, R: Integer): string;
var
  Idx: Integer;
begin
  Result := '';
  if R < DrawGrid1.FixedRows then begin
    if C = DrawGrid1.FixedCols then
      Result := FXname
    else
    if C = DrawGrid1.FixedCols + 1 then
      Result := FYname;
  end
  else begin
    Idx := R - DrawGrid1.FixedRows;
    if (Idx >= 0) and (Idx < Length(FX)) then begin
      if C = DrawGrid1.FixedCols then
        Result := FloatToStr(FX[Idx])
      else
      if C = DrawGrid1.FixedCols + 1 then
        Result := FloatToStr(FY[Idx]);
    end;
  end;
end;

procedure TFormTable.GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  GridCanvas: TCanvas;
  S: string;
begin
  GridCanvas := DrawGrid1.Canvas;
  GridCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, GetGridCell(aCol, aRow));
end;


end.


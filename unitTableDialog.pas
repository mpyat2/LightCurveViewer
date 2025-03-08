unit unitTableDialog;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Menus, ActnList,
  ExtCtrls, StdCtrls, lcvtypes;

type

  { TFormTable }

  TFormTable = class(TForm)
    ActionSelectAll: TAction;
    ActionCopy: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    DrawGrid1: TDrawGrid;
    MenuItem1: TMenuItem;
    MenuItemCopy: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FX: TDoubleArray;
    FY: TDoubleArray;
    FXname, FYname: string;
    function GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  public

  end;

procedure ShowTable(const X, Y: TDoubleArray; const Xname, Yname: string);

implementation

{$R *.lfm}

uses
  Clipbrd, guiutils;

procedure ShowTable(const X, Y: TDoubleArray; const Xname, Yname: string);
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
  DrawGrid1.ColWidths[0] := 64;
end;

procedure TFormTable.ActionCopyExecute(Sender: TObject);
var
  CurrentCursor: TCursor;
begin
  CurrentCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    Clipboard.AsText := GetGridSelectionAsText(DrawGrid1, @GetGridCell);
  finally
    Screen.Cursor := CurrentCursor;
  end;
end;

procedure TFormTable.ActionSelectAllExecute(Sender: TObject);
var
  Selection: TRect;
begin
  Selection.Top := DrawGrid1.FixedRows;
  Selection.Bottom := DrawGrid1.RowCount - 1;
  Selection.Left := DrawGrid1.FixedCols;
  Selection.Right := DrawGrid1.ColCount - 1;
  DrawGrid1.Selection := Selection;
end;

function TFormTable.GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
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
        Result := FloatToStr(FY[Idx])
      else
      if C = DrawGrid1.FixedCols - 1 then
        Result := Format('%8d', [Idx + 1]);
    end;
  end;
end;

procedure TFormTable.GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  GridCanvas: TCanvas;
begin
  GridCanvas := DrawGrid1.Canvas;
  GridCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, GetGridCell(DrawGrid1, aCol, aRow));
end;


end.


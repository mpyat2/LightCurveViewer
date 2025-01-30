unit unitModelInfoDialog;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Grids, ActnList, Menus, common;

type

  { TFormInfo }

  TFormInfo = class(TForm)
    ActionSelectAll: TAction;
    ActionCopyTable: TAction;
    ActionList1: TActionList;
    ButtonClose: TButton;
    DrawGridModelPoints: TDrawGrid;
    DrawGridModelData: TDrawGrid;
    MemoInfo: TMemo;
    MemoFormula: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PageControl1: TPageControl;
    PanelButtons: TPanel;
    PopupMenu1: TPopupMenu;
    TabSheetModelPoints: TTabSheet;
    TabSheetInfo: TTabSheet;
    TabSheetFormula: TTabSheet;
    TabSheetModelData: TTabSheet;
    procedure ActionCopyTableExecute(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FX, FY: array[0..1] of TFloatArray;
    Y2_2: TFloatArray;
    procedure UpdateGridProps;
    function GetActiveGrid: TDrawGrid;
    function GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  public

  end;

procedure ShowModelInfo(const Info, Formula: string;
                        const X1, Y1, X2, Y2, Y2_2: TFloatArray);

implementation

{$R *.lfm}

uses
  Clipbrd;

procedure ShowModelInfo(const Info, Formula: string;
                        const X1, Y1, X2, Y2, Y2_2: TFloatArray);
var
  F: TFormInfo;
begin
  F := TFormInfo.Create(Application);
  try
    F.MemoInfo.Text := Info;
    F.MemoFormula.Text := Formula;
    F.FX[0] := X1;
    F.FY[0] := Y1;
    F.FX[1] := X2;
    F.FY[1] := Y2;
    F.Y2_2 := Y2_2;
    F.UpdateGridProps;
    F.ShowModal;
  finally
    FreeAndNil(F);
  end;
end;

{ TFormInfo }

function TFormInfo.GetActiveGrid: TDrawGrid;
begin
  if PageControl1.ActivePage = TabSheetModelData then
    Result := DrawGridModelData
  else
  if PageControl1.ActivePage = TabSheetModelPoints then
    Result := DrawGridModelPoints
  else
    Result := nil;
end;

procedure TFormInfo.ActionCopyTableExecute(Sender: TObject);
var
  Grid: TDrawGrid;
  CurrentCursor: TCursor;
begin
  Grid := GetActiveGrid;
  if Grid = nil then
    Exit;
  CurrentCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    Clipboard.AsText := GetGridSelectionAsText(Grid, @GetGridCell);
  finally
    Screen.Cursor := CurrentCursor;
  end;
  //ShowMessage('Copied to Clipboard');
end;

procedure TFormInfo.ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
begin
  if (AAction = ActionCopyTable) or (AACtion = ActionSelectAll) then begin
    (AAction as TAction).Enabled := (PageControl1.ActivePage = TabSheetModelData) or (PageControl1.ActivePage = TabSheetModelPoints);
  end;
end;

procedure TFormInfo.ActionSelectAllExecute(Sender: TObject);
var
  Grid: TDrawGrid;
  Selection: TRect;
begin
  Grid := GetActiveGrid;
  if Grid = nil then
    Exit;
  Selection.Top := Grid.FixedRows;
  Selection.Bottom := Grid.RowCount - 1;
  Selection.Left := Grid.FixedCols;
  Selection.Right := Grid.ColCount - 1;
  Grid.Selection := Selection;
end;

procedure TFormInfo.FormCreate(Sender: TObject);
begin
  DrawGridModelData.OnDrawCell := @GridDrawCell;
  DrawGridModelPoints.OnDrawCell := @GridDrawCell;
end;

procedure TFormInfo.UpdateGridProps;
begin
  DrawGridModelData.RowCount := DrawGridModelData.FixedRows + Length(FX[0]);
  DrawGridModelPoints.RowCount := DrawGridModelData.FixedRows + Length(FX[1]);
end;

function TFormInfo.GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
var
  Idx: Integer;
  GridIndex: Integer;
begin
  Result := '';
  if Grid = DrawGridModelData then
    GridIndex := 0
  else
  if Grid = DrawGridModelPoints then
    GridIndex := 1
  else
    Exit;
  if R < Grid.FixedRows then begin
    if C = Grid.FixedCols then
      Result := 'Time'
    else
    if C = Grid.FixedCols + 1 then
      Result := 'Calculated Magnitude'
    else
    if C = Grid.FixedCols + 2 then
      Result := 'Observed Magnitude'
    else
    if C = Grid.FixedCols + 3 then
      Result := 'Observed - Calculated';
  end
  else begin
    Idx := R - Grid.FixedRows;
    if (Idx >= 0) and (Idx < Length(FX[GridIndex])) then begin
      if C = Grid.FixedCols then
        Result := FloatToStr(FX[GridIndex][Idx])
      else
      if C = Grid.FixedCols + 1 then
        Result := FloatToStr(FY[GridIndex][Idx])
      else
      if Grid = DrawGridModelPoints then begin
        if C = Grid.FixedCols + 2 then
          Result := FloatToStr(Y2_2[Idx])
        else
        if C = Grid.FixedCols + 3 then
          Result := FloatToStr(Y2_2[Idx] - FY[GridIndex][Idx]);
      end;
    end;
  end;
end;

procedure TFormInfo.GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  GridCanvas: TCanvas;
begin
  GridCanvas := (Sender as TDrawGrid).Canvas;
  GridCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, GetGridCell((Sender as TDrawGrid), aCol, aRow));
end;

end.


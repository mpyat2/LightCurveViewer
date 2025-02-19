unit unitModelInfoDialog;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Grids, ActnList, Menus, lcvtypes;

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
    FContiniousFit: TFitColumnArray;
    FFitAtPoints: TFitColumnArray;
    procedure UpdateGridProps;
    function GetActiveGrid: TDrawGrid;
    function GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  public

  end;

procedure ShowModelInfo(const Info, Formula: string;
                        const ContiniousFit: TFitColumnArray;
                        const FitAtPoints: TFitColumnArray);

implementation

{$R *.lfm}

uses
  Clipbrd, guiutils;

procedure ShowModelInfo(const Info, Formula: string;
                        const ContiniousFit: TFitColumnArray;
                        const FitAtPoints: TFitColumnArray);
var
  F: TFormInfo;
begin
  F := TFormInfo.Create(Application);
  try
    F.MemoInfo.Text := Info;
    F.MemoFormula.Text := Formula;
    F.FContiniousFit := ContiniousFit;
    F.FFitAtPoints := FitAtPoints;
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
  DrawGridModelData.ColCount := DrawGridModelData.FixedCols + 3;
  DrawGridModelData.RowCount := DrawGridModelData.FixedRows + Length(FContiniousFit[FitColumnType.x]);

  DrawGridModelPoints.ColCount := DrawGridModelData.FixedCols + 5;
  DrawGridModelPoints.RowCount := DrawGridModelData.FixedRows + Length(FFitAtPoints[FitColumnType.x]);
end;

function TFormInfo.GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
var
  Idx: Integer;
  DataArray: TFitColumnArray;
begin
  Result := '';
  if Grid = DrawGridModelData then
    DataArray := FContiniousFit
  else
  if Grid = DrawGridModelPoints then
    DataArray := FFitAtPoints
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
      Result := 'Calc. Mag. Error'
    else
    if C = Grid.FixedCols + 3 then
      Result := 'Observed Magnitude'
    else
    if C = Grid.FixedCols + 4 then
      Result := 'Observed - Calculated';
  end
  else begin
    Idx := R - Grid.FixedRows;
    if (Idx >= 0) and (Idx < Length(DataArray[FitColumnType.x])) then begin
      if C = Grid.FixedCols then
        Result := FloatToStr(DataArray[FitColumnType.x][Idx])
      else
      if C = Grid.FixedCols + 1 then
        Result := FloatToStr(DataArray[FitColumnType.yFit][Idx])
      else
      if C = Grid.FixedCols + 2 then
        Result := FloatToStr(DataArray[FitColumnType.yErrors][Idx])
      else
      if (C = Grid.FixedCols + 3) and (DataArray[FitColumnType.yObserved] <> nil) then
        Result := FloatToStr(DataArray[FitColumnType.yObserved][Idx])
      else
      if (C = Grid.FixedCols + 4) and (DataArray[FitColumnType.yObserved] <> nil) then
        Result := FloatToStr(DataArray[FitColumnType.yObserved][Idx] - DataArray[FitColumnType.yFit][Idx]);
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


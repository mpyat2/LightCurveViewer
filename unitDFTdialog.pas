unit unitDFTdialog;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Grids, Menus, ActnList, StdCtrls, TAGraph, TACustomSource, TACustomSeries,
  TASeries, TATools, common, Types;

type

  { TFormDFTDialog }

  TFormDFTDialog = class(TForm)
    ActionGridSelectAll: TAction;
    ActionGridCopy: TAction;
    ActionList: TActionList;
    ButtonClose: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointClickTool1: TDataPointClickTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    DrawGrid1: TDrawGrid;
    EditPeriod: TEdit;
    EditPower: TEdit;
    EditFrequency: TEdit;
    Label1: TLabel;
    LabelPower: TLabel;
    LabelPeriod: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PageControl1: TPageControl;
    PanelChartControls: TPanel;
    PanelButtons: TPanel;
    PopupMenuGrid: TPopupMenu;
    TabSheetFrequencies: TTabSheet;
    TabSheetTable: TTabSheet;
    procedure ActionGridCopyExecute(Sender: TObject);
    procedure ActionGridSelectAllExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ChartToolset1DataPointClickTool1AfterMouseDown(ATool: TChartTool;
      APoint: TPoint);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    function GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  public

  end;

procedure PlotDFTresult(const Caption: string; const frequencies, periods, power: TFloatArray);
procedure CloseDFTdialog;

implementation

{$R *.lfm}

uses
  math, Clipbrd;

var
  FormDFTDialog: TFormDFTDialog = nil;

procedure CloseDFTdialog;
begin
  if FormDFTDialog <> nil then
    FormDFTDialog.Close;
end;

procedure PlotDFTresult(const Caption: string; const frequencies, periods, power: TFloatArray);
var
  I: Integer;
begin
  FormDFTDialog := TFormDFTDialog.Create(Application);
  try
    FormDFTDialog.Caption := Caption;
    //Chart1LineSeries2.Clear;
    //Chart1LineSeries1.Clear;
    //DrawGrid1.ClearSelections;
    for I := 0 to Length(frequencies) - 1 do begin
        if not IsNan(power[I]) then
          FormDFTDialog.Chart1LineSeries1.AddXY(frequencies[I], power[I]);
    end;
    FormDFTDialog.DrawGrid1.ColCount := 2 + FormDFTDialog.DrawGrid1.FixedCols;
    FormDFTDialog.DrawGrid1.RowCount := FormDFTDialog.DrawGrid1.FixedRows + 1;
    if FormDFTDialog.Chart1LineSeries1.Count > 0 then
      FormDFTDialog.DrawGrid1.RowCount := FormDFTDialog.Chart1LineSeries1.Count + FormDFTDialog.DrawGrid1.FixedRows;
    //FormDFTDialog.DrawGrid1.Row := FormDFTDialog.DrawGrid1.FixedRows;
    //FormDFTDialog.DrawGrid1.Col := FormDFTDialog.DrawGrid1.FixedCols;
  except
    FormDFTDialog.Release;
    FormDFTDialog := nil;
  end;
  FormDFTDialog.Show;
end;

{ TFormDFTDialog }

procedure TFormDFTDialog.FormCreate(Sender: TObject);
begin
  DrawGrid1.OnDrawCell := @GridDrawCell;
end;

procedure TFormDFTDialog.ActionGridCopyExecute(Sender: TObject);
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

procedure TFormDFTDialog.ActionGridSelectAllExecute(Sender: TObject);
var
  Selection: TRect;
begin
  Selection.Top := DrawGrid1.FixedRows;
  Selection.Bottom := DrawGrid1.RowCount - 1;
  Selection.Left := DrawGrid1.FixedCols;
  Selection.Right := DrawGrid1.ColCount - 1;
  DrawGrid1.Selection := Selection;
end;

procedure TFormDFTDialog.ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  if (AAction = ActionGridSelectAll) or (AAction = ActionGridCopy) then
    (AAction as TAction).Enabled := PageControl1.ActivePage = TabSheetTable;
end;

procedure TFormDFTDialog.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormDFTDialog.ChartToolset1DataPointClickTool1AfterMouseDown(
  ATool: TChartTool; APoint: TPoint);
var
  Item: PChartDataItem;
var
  PointClickTool: TDataPointClickTool;
  Series: TChartSeries;
  PointIndex: Integer;
begin
  PointClickTool := ATool as TDataPointClickTool;
  Series := PointClickTool.Series as TChartSeries;

  if Series = Chart1LineSeries2 then
    Exit;

  EditFrequency.Text := '';
  EditPeriod.Text := '';
  EditPower.Text := '';
  Chart1LineSeries2.Clear;

  if Series = nil then
    Exit;

  PointIndex := PointClickTool.PointIndex;
  Item := Series.ListSource.Item[PointIndex];
  EditFrequency.Text := FloatToStr(Item^.X);
  EditPower.Text := FloatToStr(Item^.Y);
  if (Item^.X <> 0) then
    EditPeriod.Text := FloatToStr(1.0 / Item^.X);
  Chart1LineSeries2.AddXY(Item^.X, Item^.Y);
end;

function TFormDFTDialog.GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
var
  V: Double;
begin
  Result := '';
  if R < DrawGrid1.FixedRows + Chart1LineSeries1.Count then begin
    if R >= DrawGrid1.FixedRows then begin
      if C = DrawGrid1.FixedCols then
        V := Chart1LineSeries1.XValue[R - DrawGrid1.FixedRows]
      else
      if C = DrawGrid1.FixedCols + 1 then
        V := Chart1LineSeries1.YValue[R - DrawGrid1.FixedRows]
      else
        Exit;
      Result := FloatToStr(V);
    end
    else begin
      if C = DrawGrid1.FixedCols then
        Result := 'Frequency'
      else
      if C = DrawGrid1.FixedCols + 1 then
        Result := 'Power'
      else
        Exit;
    end;
  end;
end;

procedure TFormDFTDialog.GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  GridCanvas: TCanvas;
begin
  GridCanvas := DrawGrid1.Canvas;
  GridCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, GetGridCell(DrawGrid1, aCol, aRow));
end;

procedure TFormDFTDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FormDFTDialog := nil;
end;

end.


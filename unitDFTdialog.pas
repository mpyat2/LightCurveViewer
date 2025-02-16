unit unitDFTdialog;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Grids, Menus, ActnList, StdCtrls, TAGraph, TACustomSource, TACustomSeries,
  TASeries, TATools, Types, lcvtypes;

type

  { TFormDFTDialog }

  TFormDFTDialog = class(TForm)
    ActionCopyChart: TAction;
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
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    DrawGrid1: TDrawGrid;
    EditPeriod: TEdit;
    EditPower: TEdit;
    EditFrequency: TEdit;
    Label1: TLabel;
    LabelPower: TLabel;
    LabelPeriod: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemCopyChart: TMenuItem;
    PageControl1: TPageControl;
    PanelChartControls: TPanel;
    PanelButtons: TPanel;
    PopupMenuChart: TPopupMenu;
    PopupMenuGrid: TPopupMenu;
    TabSheetFrequencies: TTabSheet;
    TabSheetTable: TTabSheet;
    procedure ActionCopyChartExecute(Sender: TObject);
    procedure ActionGridCopyExecute(Sender: TObject);
    procedure ActionGridSelectAllExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ChartToolset1DataPointClickTool1AfterMouseDown(ATool: TChartTool;
      APoint: TPoint);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  public

  end;

procedure PlotDFTresult(const Caption: string; const frequencies, power: TFloatArray);
//procedure CloseDFTdialogs;

implementation

{$R *.lfm}

uses
  math, Clipbrd, Contnrs, guiutils;

//var
//  FormDFTDialogList: TObjectList = nil;

//procedure CloseDFTdialogs;
//var
//  I: Integer;
//begin
//  if FormDFTDialogList <> nil then begin
//    for I := FormDFTDialogList.Count - 1 downto 0 do begin
//      if FormDFTDialogList[I] is TForm then begin
//        (FormDFTDialogList[I] as TForm).Close;
//        FormDFTDialogList.Delete(I);
//      end;
//    end;
//  end;
//end;

procedure PlotDFTresult(const Caption: string; const frequencies, power: TFloatArray);
var
  F: TFormDFTDialog;
  I: Integer;
begin
  F := TFormDFTDialog.Create(Application);
  try
    //FormDFTDialogList.Add(F);
    F.Caption := Caption;
    for I := 0 to Length(frequencies) - 1 do begin
        if not IsNan(power[I]) then
          F.Chart1LineSeries1.AddXY(frequencies[I], power[I]);
    end;
    F.DrawGrid1.ColCount := 2 + F.DrawGrid1.FixedCols;
    F.DrawGrid1.RowCount := F.DrawGrid1.FixedRows + 1;
    if F.Chart1LineSeries1.Count > 0 then
      F.DrawGrid1.RowCount := F.Chart1LineSeries1.Count + F.DrawGrid1.FixedRows;
  except
    F.Release;
  end;
  F.Show;
end;

{ TFormDFTDialog }

procedure TFormDFTDialog.FormCreate(Sender: TObject);
begin
  DrawGrid1.OnDrawCell := @GridDrawCell;
end;

procedure TFormDFTDialog.FormDestroy(Sender: TObject);
begin
  //FormDFTDialogList.Remove(Self);
  //OutputDebugString('TFormDFTDialog.FormDestroy');
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

procedure TFormDFTDialog.ActionCopyChartExecute(Sender: TObject);
begin
  Chart1.CopyToClipboard(TPortableNetworkGraphic);
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
end;

//var
//  I: Integer;
//
//initialization
//  FormDFTDialogList := TObjectList.Create(False);
//finalization
//  if FormDFTDialogList <> nil then begin
//    for I := FormDFTDialogList.Count - 1 downto 0 do begin
//      if FormDFTDialogList[I] is TForm then begin
//        (FormDFTDialogList[I] as TForm).Release;
//      end;
//    end;
//    FreeAndNil(FormDFTDialogList);
//  end;
end.


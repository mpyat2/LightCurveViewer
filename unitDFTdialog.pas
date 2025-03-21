unit unitDFTdialog;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Grids, Menus, ActnList, StdCtrls, TAGraph, TACustomSource,
  TACustomSeries, TASeries, TATools, Types, lcvtypes, unitPhaseDialog;

type

  { TFormDFTDialog }

  TFormDFTDialog = class(TForm)
    ActionCopyChart: TAction;
    ActionGridSelectAll: TAction;
    ActionGridCopy: TAction;
    ActionList: TActionList;
    ButtonPhasePlot: TButton;
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
    DrawGrid2: TDrawGrid;
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
    TabSheetMaxima: TTabSheet;
    TabSheetFrequencies: TTabSheet;
    TabSheetTable: TTabSheet;
    procedure ActionCopyChartExecute(Sender: TObject);
    procedure ActionGridCopyExecute(Sender: TObject);
    procedure ActionGridSelectAllExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonPhasePlotClick(Sender: TObject);
    procedure ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool; APoint: TPoint);
    procedure DrawGrid1SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure DrawGrid2SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSelectCellEventActive: Boolean;
    FSelectedPointIndex: Integer;
    FApplyPhasePlotParamsProc: TApplyPhasePlotParams;
    FmaximaX: TDoubleArray;
    FmaximaY: TDoubleArray;
    FmaximaN: TIntegerArray;
    function GetGrid1Cell(C, R: Integer): string;
    function GetGrid2Cell(C, R: Integer): string;
    function GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
    function GetSelectedRowsAsString(Grid: TDrawGrid): string;
    procedure InitMaxima;
    procedure UnSelectChartPoint;
    procedure SelectChartPoint(Series: TChartSeries; PointIndex: Integer);
    procedure SelectGrid1Row(PointIndex: Integer);
    procedure SelectGrid2Row(PointIndex: Integer);
  public
  end;

procedure PlotDFTresult(const Caption: string; const frequencies, power: TDoubleArray; ApplyPhasePlotParamsProc: TApplyPhasePlotParams);

implementation

{$R *.lfm}

uses
//{$IF defined(Windows)}
//  Windows,
//{$ENDIF}
  math, Clipbrd, Contnrs, sortutils, guiutils;

const
  UnsincSelectColor = clLtGray;

procedure PlotDFTresult(const Caption: string; const frequencies, power: TDoubleArray; ApplyPhasePlotParamsProc: TApplyPhasePlotParams);
var
  F: TFormDFTDialog;
  I: Integer;
begin
  F := TFormDFTDialog.Create(Application);
  try
    F.Caption := Caption;
    for I := 0 to Length(frequencies) - 1 do begin
      if (I > 0) and (frequencies[I] < frequencies[I - 1]) then
        raise Exception.Create('PlotDFTresult: Internal error: "frequencies" must be sorted.');
      if not IsNan(power[I]) then
        F.Chart1LineSeries1.AddXY(frequencies[I], power[I]);
    end;
    F.InitMaxima;

    F.DrawGrid1.ColCount := 2 + F.DrawGrid1.FixedCols;
    F.DrawGrid1.RowCount := F.DrawGrid1.FixedRows + 1;
    if F.Chart1LineSeries1.Count > 0 then
      F.DrawGrid1.RowCount := F.Chart1LineSeries1.Count + F.DrawGrid1.FixedRows;

    F.DrawGrid2.ColCount := 2 + F.DrawGrid2.FixedCols;
    F.DrawGrid2.RowCount := F.DrawGrid2.FixedRows + 1;
    if Length(F.FMaximaX) > 0 then
      F.DrawGrid2.RowCount := Length(F.FMaximaX) + F.DrawGrid2.FixedRows;

    F.DrawGrid1.SelectedColor := UnsincSelectColor;
    F.DrawGrid2.SelectedColor := UnsincSelectColor;

    F.FSelectedPointIndex := -1;

    F.FApplyPhasePlotParamsProc := ApplyPhasePlotParamsProc;
  except
    F.Release;
  end;
  F.Show;
end;

{ TFormDFTDialog }

procedure TFormDFTDialog.FormCreate(Sender: TObject);
begin
  FSelectCellEventActive := False;
end;

procedure TFormDFTDialog.FormDestroy(Sender: TObject);
begin
//{$IF defined(Windows)}
//  OutputDebugString('TFormDFTDialog.FormDestroy');
//{$ENDIF}
end;

procedure TFormDFTDialog.FormShow(Sender: TObject);
begin
  FSelectCellEventActive := True;
end;

procedure TFormDFTDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormDFTDialog.ActionGridSelectAllExecute(Sender: TObject);
var
  Grid: TDrawGrid;
  Selection: TRect;
begin
  if PageControl1.ActivePage = TabSheetTable then
    Grid := DrawGrid1
  else
  if PageControl1.ActivePage = TabSheetMaxima then
    Grid := DrawGrid2
  else
    Exit;
  Selection.Top := Grid.FixedRows;
  Selection.Bottom := Grid.RowCount - 1;
  Selection.Left := Grid.FixedCols;
  Selection.Right := Grid.ColCount - 1;
  Grid.Selection := Selection;
end;

procedure TFormDFTDialog.ActionGridCopyExecute(Sender: TObject);
var
  Grid: TDrawGrid;
  CurrentCursor: TCursor;
begin
  if PageControl1.ActivePage = TabSheetTable then
    Grid := DrawGrid1
  else
  if PageControl1.ActivePage = TabSheetMaxima then
    Grid := DrawGrid2
  else
    Exit;
  CurrentCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    Clipboard.AsText := GetSelectedRowsAsString(Grid);
  finally
    Screen.Cursor := CurrentCursor;
  end;
end;

procedure TFormDFTDialog.ActionCopyChartExecute(Sender: TObject);
begin
  Chart1.CopyToClipboard(Graphics.TBitmap);
end;

procedure TFormDFTDialog.ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  if (AAction = ActionGridSelectAll) or (AAction = ActionGridCopy) then
    (AAction as TAction).Enabled :=
      (PageControl1.ActivePage = TabSheetTable) or (PageControl1.ActivePage = TabSheetMaxima);
end;

procedure TFormDFTDialog.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormDFTDialog.ButtonPhasePlotClick(Sender: TObject);
begin
  if EditPeriod.Text <> '' then
    PhasePlot(FApplyPhasePlotParamsProc, StrToFloat(EditPeriod.Text))
  else
    ShowMessage('No period specified');
end;

procedure TFormDFTDialog.ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool; APoint: TPoint);
var
  PointClickTool: TDataPointClickTool;
  Series: TChartSeries;
  PointIndex: Integer;
begin
  UnSelectChartPoint;
  SelectGrid1Row(-1);
  SelectGrid2Row(-1);

  PointClickTool := ATool as TDataPointClickTool;
  Series := PointClickTool.Series as TChartSeries;

  if Series = Chart1LineSeries2 then
    Exit;

  if Series = nil then
    Exit;

  PointIndex := PointClickTool.PointIndex;
//{$IF defined(Windows)}
//  OutputDebugString(PChar('DataPointClick: PointIndex = ' + IntToStr(PointIndex)));
//{$ENDIF}
  SelectChartPoint(Series, PointIndex);
  SelectGrid1Row(PointIndex);
  SelectGrid2Row(PointIndex);
end;

procedure TFormDFTDialog.DrawGrid1SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var
  PointIndex: Integer;
begin
  if FSelectCellEventActive then begin
//{$IF defined(Windows)}
//    OutputDebugString(PChar((Sender as TDrawGrid).Name + '; aCol = ' + IntToStr(aCol) + '; aRow = ' + IntToStr(aRow)));
//{$ENDIF}
    DrawGrid1.SelectedColor := clHighlight;
    UnSelectChartPoint;
    SelectGrid2Row(-1);
    if (aRow >= DrawGrid1.FixedRows) and (aRow < DrawGrid1.FixedRows + Chart1LineSeries1.Count) then begin
      PointIndex := aRow - DrawGrid1.FixedRows;
      SelectChartPoint(Chart1LineSeries1, PointIndex);
      SelectGrid2Row(PointIndex);
    end;
  end;
end;

procedure TFormDFTDialog.DrawGrid2SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var
  PointIndex: Integer;
begin
  if FSelectCellEventActive then begin
//{$IF defined(Windows)}
//    OutputDebugString(PChar((Sender as TDrawGrid).Name + '; aCol = ' + IntToStr(aCol) + '; aRow = ' + IntToStr(aRow)));
//{$ENDIF}
    DrawGrid2.SelectedColor := clHighlight;
    UnSelectChartPoint;
    SelectGrid1Row(-1);
    if (aRow >= DrawGrid2.FixedRows) and (aRow < DrawGrid2.FixedRows + Length(FmaximaN)) then begin
      PointIndex := FmaximaN[aRow - DrawGrid2.FixedRows];
      SelectChartPoint(Chart1LineSeries1, PointIndex);
      SelectGrid1Row(PointIndex);
    end;
  end;
end;

procedure TFormDFTDialog.UnSelectChartPoint;
begin
  FSelectedPointIndex := -1;
  EditFrequency.Text := '';
  EditPeriod.Text := '';
  ButtonPhasePlot.Enabled := False;
  EditPower.Text := '';
  Chart1LineSeries2.Clear;
end;

procedure TFormDFTDialog.SelectChartPoint(Series: TChartSeries; PointIndex: Integer);
var
  Item: PChartDataItem;
begin
  Item := Series.ListSource.Item[PointIndex];
  EditFrequency.Text := FloatToStr(Item^.X);
  EditPower.Text := FloatToStr(Item^.Y);
  if (Item^.X <> 0) then begin
    try
      EditPeriod.Text := FloatToStr(1.0 / Item^.X);
      ButtonPhasePlot.Enabled := True;
    except
      EditPeriod.Text := '';
    end;
  end
  else begin
    EditPeriod.Text := '';
    ButtonPhasePlot.Enabled := False;
  end;
  FSelectedPointIndex := PointIndex;
  Chart1LineSeries2.AddXY(Item^.X, Item^.Y);
end;

procedure TFormDFTDialog.SelectGrid1Row(PointIndex: Integer);
var
  GridSelection: TGridRect;
begin
  FSelectCellEventActive := False;
  try
    DrawGrid1.ClearSelections;
    DrawGrid1.Row := DrawGrid1.FixedRows;
    DrawGrid1.SelectedColor := UnsincSelectColor;
    if PointIndex < 0 then
      Exit;
    GridSelection.Left := DrawGrid1.FixedCols;
    GridSelection.Right := DrawGrid1.ColCount - 1;
    GridSelection.Top := DrawGrid1.FixedRows + PointIndex;
    GridSelection.Bottom := GridSelection.Top;
    DrawGrid1.Row := GridSelection.Top;
    DrawGrid1.Selection := GridSelection;
    DrawGrid1.SelectedColor := clHighlight;
  finally
    FSelectCellEventActive := True;
  end;
end;

procedure TFormDFTDialog.SelectGrid2Row(PointIndex: Integer);
var
  GridSelection: TGridRect;
  I, N: Integer;
begin
  FSelectCellEventActive := False;
  try
    DrawGrid2.ClearSelections;
    DrawGrid2.Row := DrawGrid2.FixedRows;
    DrawGrid2.SelectedColor := UnsincSelectColor;
    if PointIndex < 0 then
      Exit;
    N := -1;
    for I := 0 to Length(FMaximaN) - 1 do begin
      if FMaximaN[I] = PointIndex then begin
        N := I;
        Break;
      end;
    end;
    if N < 0 then
      Exit;
    GridSelection.Left := DrawGrid2.FixedCols;
    GridSelection.Right := DrawGrid2.ColCount - 1;
    GridSelection.Top := DrawGrid2.FixedRows + N;
    GridSelection.Bottom := GridSelection.Top;
    DrawGrid2.Row := GridSelection.Top;
    DrawGrid2.Selection := GridSelection;
    DrawGrid2.SelectedColor := clHighlight;
  finally
    FSelectCellEventActive := True;
  end;
end;

procedure TFormDFTDialog.GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  GridCanvas: TCanvas;
  Grid: TDrawGrid;
begin
  Grid := Sender as TDrawGrid;
  GridCanvas := Grid.Canvas;
  GridCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, GetGridCell(Grid, aCol, aRow));
end;

function TFormDFTDialog.GetGrid1Cell(C, R: Integer): string;
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

function TFormDFTDialog.GetGrid2Cell(C, R: Integer): string;
var
  V: Double;
begin
  Result := '';
  if R < DrawGrid2.FixedRows + Length(FmaximaX) then begin
    if R >= DrawGrid2.FixedRows then begin
      if C = DrawGrid2.FixedCols then
        V := FmaximaX[R - DrawGrid2.FixedRows]
      else
      if C = DrawGrid2.FixedCols + 1 then
        V := FMaximaY[R - DrawGrid2.FixedRows]
      else
        Exit;
      Result := FloatToStr(V);
    end
    else begin
      if C = DrawGrid2.FixedCols then
        Result := 'Frequency'
      else
      if C = DrawGrid2.FixedCols + 1 then
        Result := 'Power'
      else
        Exit;
    end;
  end;
end;

function TFormDFTDialog.GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
begin
  Result := '';
  if Grid = DrawGrid1 then
    Result := GetGrid1Cell(C, R)
  else
  if Grid = DrawGrid2 then
    Result := GetGrid2Cell(C, R);
end;

function TFormDFTDialog.GetSelectedRowsAsString(Grid: TDrawGrid): string;
var
  S2: string;
  R, C: Integer;
begin
  // Multiselection: full rows only!
  Result := '';
  for R := Grid.FixedRows to Grid.RowCount - 1 do begin
    S2 := '';
    if Grid.IsCellSelected[Grid.FixedCols, R] then begin
      for C := Grid.FixedCols to Grid.ColCount - 1 do begin
        S2 := S2 + GetGridCell(Grid, C, R) + ^I;
      end;
      if (Length(S2) > 0) and (S2[Length(S2)] = ^I) then
        Delete(S2, Length(S2), 1);
      Result := Result + S2 + ^M^J;
    end;
  end;
  if (Length(Result) > 1) and (Copy(Result, Length(Result) - 1, 2) = ^M^J) then
    Delete(Result, Length(Result) - 1, 2);
end;

procedure TFormDFTDialog.InitMaxima;
var
  Item: PChartDataItem;
  Y, Yprev, Ynext: Double;
  I, N: Integer;
begin
  SetLength(FmaximaX, Chart1LineSeries1.Count);
  SetLength(FmaximaY, Chart1LineSeries1.Count);
  SetLength(FmaximaN, Chart1LineSeries1.Count);
  N := 0;
  for I := 1 to Chart1LineSeries1.Count - 2 do begin
    Y := Chart1LineSeries1.GetYValue(I);
    Yprev := Chart1LineSeries1.GetYValue(I - 1);
    Ynext := Chart1LineSeries1.GetYValue(I + 1);
    if (Y > Yprev) and (Y >= Ynext) then begin
      FmaximaX[N] := Chart1LineSeries1.GetXValue(I);
      FmaximaY[N] := Y;
      FmaximaN[N] := I;
      Inc(N);
    end;
  end;
  SetLength(FmaximaX, N);
  SetLength(FmaximaY, N);
  SetLength(FmaximaN, N);
  SortDataNPointsYDesc(FmaximaX, FmaximaY, FmaximaN);
end;

end.


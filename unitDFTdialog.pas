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
    ButtonClose: TButton;
    ButtonModelFromMaxima: TButton;
    ButtonPhasePlot: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointClickTool1: TDataPointClickTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    DrawGridFrequencies: TDrawGrid;
    DrawGridMaxima: TDrawGrid;
    EditFrequency: TEdit;
    EditPeriod: TEdit;
    EditPower: TEdit;
    Label1: TLabel;
    LabelPeriod: TLabel;
    LabelPower: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemCopyChart: TMenuItem;
    PageControl1: TPageControl;
    PanelModel: TPanel;
    PanelPhasePlot: TPanel;
    PopupMenuChart: TPopupMenu;
    PopupMenuGrid: TPopupMenu;
    TabSheetFrequencies: TTabSheet;
    TabSheetTable: TTabSheet;
    procedure ActionCopyChartExecute(Sender: TObject);
    procedure ActionGridCopyExecute(Sender: TObject);
    procedure ActionGridSelectAllExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonModelClick(Sender: TObject);
    procedure ButtonPhasePlotClick(Sender: TObject);
    procedure ChartToolset1DataPointClickTool1BeforeMouseUp(ATool: TChartTool; APoint: TPoint);
    procedure ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool; APoint: TPoint);
    procedure DrawGridFrequenciesSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure DrawGridMaximaSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSelectCellEventActive: Boolean;
    FSelectedPointIndex: Integer;
    FTrendDegreeForModel: Integer;
    FTrigPolyDegreesForModel: TInt5Array;
    FApplyPhasePlotParamsProc: TApplyPhasePlotParams;
    FmaximaX: TDoubleArray;
    FmaximaY: TDoubleArray;
    FmaximaN: TIntegerArray;
    function GetGrid1Frequency(R: Integer): Double;
    function GetGrid2Frequency(R: Integer): Double;
    function GetGridFrequency(Grid: TDrawGrid; R: Integer): Double;
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
  math, Clipbrd, settings, sortutils, unitFitParamDialog, unitMain;

const
  UnsincSelectColor = clLtGray;

procedure PlotDFTresult(const Caption: string; const frequencies, power: TDoubleArray; ApplyPhasePlotParamsProc: TApplyPhasePlotParams);
var
  NaNValuesFound: Boolean;
  NegValuesFound: Boolean;
  AddFreq: Boolean;
  Pow: Double;
  F: TFormDFTDialog;
  I: Integer;
  Msg: string;
begin
  F := TFormDFTDialog.Create(Application);
  try
    F.Caption := Caption;
    NaNValuesFound := False;
    NegValuesFound := False;
    for I := 0 to Length(frequencies) - 1 do begin
      if (I > 0) and (frequencies[I] < frequencies[I - 1]) then
        raise Exception.Create('PlotDFTresult: Internal error: "frequencies" must be sorted.');
      AddFreq := True;
      Pow := power[I];
      if IsNan(Pow) then begin
        if frequencies[I] = 0.0 then begin
          // Zero-frequency is a special case: this is a common start value and is always ignored.
          AddFreq := False;
        end
        else begin
          NaNValuesFound := True;
        end;
      end else
      if Pow < 0.0 then begin
        NegValuesFound := True;
        Pow := 0.0;
      end;
      if AddFreq then
        F.Chart1LineSeries1.AddXY(frequencies[I], Pow);
    end;
    if NaNValuesFound or NegValuesFound then begin
      Msg := 'The result could not be calculated for some non-zero frequencies';
      if NaNValuesFound then
        Msg := Msg + ^M^J + 'NaN values found for non-zero frequencies';
      if NegValuesFound then
        Msg := Msg + ^M^J + 'Negative Power values found: replaced by zeros';
      ShowMessage(Msg);
    end;

    F.InitMaxima;

    F.DrawGridFrequencies.ColCount := 3 + F.DrawGridFrequencies.FixedCols;
    F.DrawGridFrequencies.RowCount := F.DrawGridFrequencies.FixedRows + 1;
    if F.Chart1LineSeries1.Count > 0 then
      F.DrawGridFrequencies.RowCount := F.Chart1LineSeries1.Count + F.DrawGridFrequencies.FixedRows;

    F.DrawGridMaxima.ColCount := 3 + F.DrawGridMaxima.FixedCols;
    F.DrawGridMaxima.RowCount := F.DrawGridMaxima.FixedRows + 1;
    if Length(F.FMaximaX) > 0 then
      F.DrawGridMaxima.RowCount := Length(F.FMaximaX) + F.DrawGridMaxima.FixedRows;

    F.DrawGridFrequencies.SelectedColor := UnsincSelectColor;
    F.DrawGridMaxima.SelectedColor := UnsincSelectColor;

    F.FSelectedPointIndex := -1;

    F.FApplyPhasePlotParamsProc := ApplyPhasePlotParamsProc;
  except
    F.Release;
  end;
  F.Show;
end;

{ TFormDFTDialog }

procedure TFormDFTDialog.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FSelectCellEventActive := False;
  FTrendDegreeForModel := 0;
  for I := 0 to Length(FTrigPolyDegreesForModel) - 1 do
    FTrigPolyDegreesForModel[I] := 1;
  Chart1.AxisList[0].Grid.Visible := GetGlobalBoolParameter('ShowGrid', True);
  Chart1.AxisList[1].Grid.Visible := Chart1.AxisList[0].Grid.Visible;
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
  if DrawGridFrequencies.Focused then
    Grid := DrawGridFrequencies
  else
  if DrawGridMaxima.Focused then
    Grid := DrawGridMaxima
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
  if DrawGridFrequencies.Focused then
    Grid := DrawGridFrequencies
  else
  if DrawGridMaxima.Focused then
    Grid := DrawGridMaxima
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
  if (AAction = ActionGridSelectAll) or (AAction = ActionGridCopy) then begin
    (AAction as TAction).Enabled := DrawGridFrequencies.Focused or DrawGridMaxima.Focused;
  end;
end;

procedure TFormDFTDialog.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormDFTDialog.ButtonModelClick(Sender: TObject);
var
  Frequencies: TDouble5Array;
  Grid: TDrawGrid;
  R, N: Integer;
begin
  if not FormMain.CalculationInProgress and (FormMain.LCSrcDataCount > 0) then begin
    if PageControl1.ActivePage = TabSheetFrequencies then begin
      Grid := DrawGridMaxima;
    end
    else
    if PageControl1.ActivePage = TabSheetTable then begin
      Grid := DrawGridFrequencies;
    end
    else
      Exit;

    for R := 0 to Length(Frequencies) - 1 do
      Frequencies[R] := NaN;

    N := 0;
    for R := Grid.FixedRows to Grid.RowCount - 1 do begin
      if Grid.IsCellSelected[Grid.FixedCols, R] then begin
        if N > Length(Frequencies) - 1 then begin
          ShowMessage('In the current implementation, no more than ' + IntToStr(Length(Frequencies)) + ' independent frequencies are allowed.' + ^M^J +
                      'Please select fewer frequencies');
          Exit;
        end;
        Frequencies[N] := GetGridFrequency(Grid, R);
        Inc(N);
      end;
    end;

    if not GetFitParams(FTrendDegreeForModel, FTrigPolyDegreesForModel, Frequencies, True) then
      Exit;
    FormMain.SaveDataSettings;
    FormMain.DoPolyFitProc(FTrendDegreeForModel, FTrigPolyDegreesForModel, Frequencies);
  end
  else
    ShowMessage('Cannot do it now');
end;

procedure TFormDFTDialog.ButtonPhasePlotClick(Sender: TObject);
begin
  if not FormMain.CalculationInProgress and (FormMain.LCSrcDataCount > 0) then begin
    if EditPeriod.Text <> '' then
      PhasePlot(FApplyPhasePlotParamsProc, StrToFloat(EditPeriod.Text))
    else
      ShowMessage('No period specified');
  end
  else
    ShowMessage('Cannot do it now');
end;

procedure TFormDFTDialog.ChartToolset1DataPointClickTool1BeforeMouseUp(ATool: TChartTool; APoint: TPoint);
begin
  UnSelectChartPoint;
  SelectGrid1Row(-1);
  SelectGrid2Row(-1);
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

  if Series <> Chart1LineSeries1 then
    Exit;

  PointIndex := PointClickTool.PointIndex;
//{$IF defined(Windows)}
//  OutputDebugString(PChar('DataPointClick: PointIndex = ' + IntToStr(PointIndex)));
//{$ENDIF}
  SelectChartPoint(Series, PointIndex);
  SelectGrid1Row(PointIndex);
  SelectGrid2Row(PointIndex);
end;

procedure TFormDFTDialog.DrawGridFrequenciesSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var
  PointIndex: Integer;
begin
  if FSelectCellEventActive then begin
//{$IF defined(Windows)}
//    OutputDebugString(PChar((Sender as TDrawGrid).Name + '; aCol = ' + IntToStr(aCol) + '; aRow = ' + IntToStr(aRow)));
//{$ENDIF}
    DrawGridFrequencies.SelectedColor := clHighlight;
    UnSelectChartPoint;
    SelectGrid2Row(-1);
    if (aRow >= DrawGridFrequencies.FixedRows) and (aRow < DrawGridFrequencies.FixedRows + Chart1LineSeries1.Count) then begin
      PointIndex := aRow - DrawGridFrequencies.FixedRows;
      SelectChartPoint(Chart1LineSeries1, PointIndex);
      SelectGrid2Row(PointIndex);
    end;
  end;
end;

procedure TFormDFTDialog.DrawGridMaximaSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var
  PointIndex: Integer;
begin
  if FSelectCellEventActive then begin
//{$IF defined(Windows)}
//    OutputDebugString(PChar((Sender as TDrawGrid).Name + '; aCol = ' + IntToStr(aCol) + '; aRow = ' + IntToStr(aRow)));
//{$ENDIF}
    DrawGridMaxima.SelectedColor := clHighlight;
    UnSelectChartPoint;
    SelectGrid1Row(-1);
    if (aRow >= DrawGridMaxima.FixedRows) and (aRow < DrawGridMaxima.FixedRows + Length(FmaximaN)) then begin
      PointIndex := FmaximaN[aRow - DrawGridMaxima.FixedRows];
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
    DrawGridFrequencies.ClearSelections;
    DrawGridFrequencies.Row := DrawGridFrequencies.FixedRows;
    DrawGridFrequencies.SelectedColor := UnsincSelectColor;
    if PointIndex < 0 then
      Exit;
    GridSelection.Left := DrawGridFrequencies.FixedCols;
    GridSelection.Right := DrawGridFrequencies.ColCount - 1;
    GridSelection.Top := DrawGridFrequencies.FixedRows + PointIndex;
    GridSelection.Bottom := GridSelection.Top;
    DrawGridFrequencies.Row := GridSelection.Top;
    DrawGridFrequencies.Selection := GridSelection;
    DrawGridFrequencies.SelectedColor := clHighlight;
  finally
    FSelectCellEventActive := True;
  end;
end;

procedure TFormDFTDialog.SelectGrid2Row(PointIndex: Integer);
var
  GridSelection: TGridRect;
  X, Distance: Double;
  I, N, Napprox: Integer;
begin
  FSelectCellEventActive := False;
  try
    DrawGridMaxima.ClearSelections;
    DrawGridMaxima.Row := DrawGridMaxima.FixedRows;
    DrawGridMaxima.SelectedColor := UnsincSelectColor;
    if PointIndex < 0 then
      Exit;
    N := -1;
    Napprox := -1;
    for I := 0 to Length(FMaximaN) - 1 do begin
      if FMaximaN[I] = PointIndex then begin
        N := I;
        Break;
      end;
    end;
    if N < 0 then begin
      Distance := NaN;
      X := Chart1LineSeries1.GetXValue(PointIndex);
      for I := 0 to Length(FmaximaX) - 1 do begin
        if IsNaN(Distance) then begin
          Distance := Abs(X - FmaximaX[I]);
          Napprox := I;
        end
        else begin
          if Abs(X - FmaximaX[I]) < Distance then begin
            Distance := Abs(X - FmaximaX[I]);
            Napprox := I;
          end;
        end;
      end;
    end;
    if (N < 0) and (Napprox < 0) then
      Exit;
    GridSelection.Left := DrawGridMaxima.FixedCols;
    GridSelection.Right := DrawGridMaxima.ColCount - 1;
    if N >= 0 then
      GridSelection.Top := DrawGridMaxima.FixedRows + N
    else
      GridSelection.Top := DrawGridMaxima.FixedRows + Napprox;
    GridSelection.Bottom := GridSelection.Top;
    DrawGridMaxima.Row := GridSelection.Top;
    DrawGridMaxima.Selection := GridSelection;
    if N >= 0 then
      DrawGridMaxima.SelectedColor := clHighlight;
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

function TFormDFTDialog.GetGrid1Frequency(R: Integer): Double;
begin
  Result := NaN;
  if R < DrawGridFrequencies.FixedRows + Chart1LineSeries1.Count then begin
    if R >= DrawGridFrequencies.FixedRows then begin
      Result := Chart1LineSeries1.XValue[R - DrawGridFrequencies.FixedRows];
    end;
  end;
end;

function TFormDFTDialog.GetGrid2Frequency(R: Integer): Double;
begin
  Result := NaN;
  if R < DrawGridMaxima.FixedRows + Length(FmaximaX) then begin
    if R >= DrawGridMaxima.FixedRows then begin
      Result := FmaximaX[R - DrawGridMaxima.FixedRows];
    end;
  end;
end;

function TFormDFTDialog.GetGridFrequency(Grid: TDrawGrid; R: Integer): Double;
begin
  Result := NaN;
  if Grid = DrawGridFrequencies then
    Result := GetGrid1Frequency(R)
  else
  if Grid = DrawGridMaxima then
    Result := GetGrid2Frequency(R);
end;

function TFormDFTDialog.GetGrid1Cell(C, R: Integer): string;
var
  V: Double;
begin
  Result := '';
  if R < DrawGridFrequencies.FixedRows + Chart1LineSeries1.Count then begin
    if R >= DrawGridFrequencies.FixedRows then begin
      if C = DrawGridFrequencies.FixedCols then
        V := Chart1LineSeries1.XValue[R - DrawGridFrequencies.FixedRows]
      else
      if C = DrawGridFrequencies.FixedCols + 1 then
        V := Chart1LineSeries1.YValue[R - DrawGridFrequencies.FixedRows]
      else
      if C = DrawGridFrequencies.FixedCols + 2 then begin
        V := Chart1LineSeries1.XValue[R - DrawGridFrequencies.FixedRows];
        if V <> 0 then begin
          try
            V := 1.0 / V
          except
            Result := '*ERROR*';
            Exit;
          end;
        end
        else begin
          Result := '';
          Exit;
        end;
      end
      else
        Exit;
      Result := FloatToStr(V);
    end
    else begin
      if C = DrawGridFrequencies.FixedCols then
        Result := 'Frequency'
      else
      if C = DrawGridFrequencies.FixedCols + 1 then
        Result := 'Power'
      else
      if C = DrawGridFrequencies.FixedCols + 2 then
        Result := 'Period'
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
  if R < DrawGridMaxima.FixedRows + Length(FmaximaX) then begin
    if R >= DrawGridMaxima.FixedRows then begin
      if C = DrawGridMaxima.FixedCols then
        V := FmaximaX[R - DrawGridMaxima.FixedRows]
      else
      if C = DrawGridMaxima.FixedCols + 1 then
        V := FMaximaY[R - DrawGridMaxima.FixedRows]
      else
      if C = DrawGridFrequencies.FixedCols + 2 then begin
        V := FmaximaX[R - DrawGridMaxima.FixedRows];
        if V <> 0 then begin
          try
            V := 1.0 / V
          except
            Result := '*ERROR*';
            Exit;
          end;
        end
        else begin
          Result := '';
          Exit;
        end;
      end
      else
        Exit;
      Result := FloatToStr(V);
    end
    else begin
      if C = DrawGridMaxima.FixedCols then
        Result := 'Frequency (Maxima)'
      else
      if C = DrawGridMaxima.FixedCols + 1 then
        Result := 'Power'
      else
      if C = DrawGridFrequencies.FixedCols + 2 then
        Result := 'Period'
      else
        Exit;
    end;
  end;
end;

function TFormDFTDialog.GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
begin
  Result := '';
  if Grid = DrawGridFrequencies then
    Result := GetGrid1Cell(C, R)
  else
  if Grid = DrawGridMaxima then
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
    if (not isNAN(Y)) and (not isNAN(Yprev) and (not isNAN(Ynext))) and (Y > Yprev) and (Y >= Ynext) then begin
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


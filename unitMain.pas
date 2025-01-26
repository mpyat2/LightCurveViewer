unit unitMain;

{$mode objfpc}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ComCtrls, TAGraph, TASources, TASeries, TACustomSource, TATools, DoLongOp;

type

  { TFormMain }

  TFormMain = class(TForm)
    ActionFitInfo: TAction;
    ActionModelAsTable: TAction;
    ActionPolyFit: TAction;
    ActionPeriodogram: TAction;
    ActionPhasePlotSimple: TAction;
    ActionRawData: TAction;
    ActionPhasePlot: TAction;
    ActionInvertedY: TAction;
    ActionOpen: TAction;
    ActionExit: TAction;
    ActionList1: TActionList;
    Chart1: TChart;
    Chart1LineSeriesModel: TLineSeries;
    Chart1LineSeriesData: TLineSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ImageList1: TImageList;
    ListChartSourceModel: TListChartSource;
    ListChartSourceFoldedModel: TListChartSource;
    ListChartSourceFoldedData: TListChartSource;
    ListChartSourceData: TListChartSource;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemModelInfo: TMenuItem;
    Separator1: TMenuItem;
    MenuItemPolyFit: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemRawData: TMenuItem;
    MenuItemPhasePlot: TMenuItem;
    MenuItemInvertedY: TMenuItem;
    MenuView: TMenuItem;
    MenuItemExit: TMenuItem;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ActionFitInfoExecute(Sender: TObject);
    procedure ActionInvertedYExecute(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionModelAsTableExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionPeriodogramExecute(Sender: TObject);
    procedure ActionPhasePlotExecute(Sender: TObject);
    procedure ActionPhasePlotSimpleExecute(Sender: TObject);
    procedure ActionPolyFitExecute(Sender: TObject);
    procedure ActionRawDataExecute(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FFileName: string;
    FPeriodogramFirstRun: Boolean;
    FDFTThreadTerminated: Boolean;
    FFitFormula: string;
    procedure CloseFile;
    procedure OpenFile(const AFileName: string);
    procedure PlotData;
    procedure PlotFolded;
    procedure PlotFoldedSimple;
    procedure PlotFoldedProc;
    procedure CalculateModelPhasePlot;
    procedure CalcAndPlotFoldedProc;
    procedure SetAxisBoundaries(Xmin, Xmax, Ymin, Ymax: Double; ExpandX, ExpandY: Boolean);
    procedure Periodogram;
    procedure DoPolyFit;
    function DoDCDFT(params: Pointer; ProgressCaptionProc: TProgressCaptionProc): Integer;
    procedure DCDFTThreadOnTerminate(Sender: TObject);
    procedure DFTGlobalTerminate(Sender: TObject);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  Windows, math, typ, TAChartUtils, common, unitPhaseDialog, unitFitParamDialog,
  unitDFTparamDialog, unitDFTdialog, unitDFT, unitTableDialog, unitInfoDialog,
  dftThread, dataio;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ToolBar1.Images := ImageList1;
  CloseFile;
end;

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.Chart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Pg: TDoublePoint;
begin
  try
    if (Chart1LineSeriesData.Source <> nil) and (Chart1LineSeriesData.Source.Count > 0) then begin
      P := Chart1.ScreenToClient(Mouse.CursorPos);
      Pg := Chart1.ImageToGraph(P);
      StatusBar1.Panels[0].Text := Format(' %g'^I' %g ', [Pg.X, Pg.Y]);
    end
    else
      StatusBar1.Panels[0].Text := '';
  except
    on E: Exception do
      StatusBar1.Panels[0].Text := '';
  end;
end;

procedure TFormMain.ActionPeriodogramExecute(Sender: TObject);
begin
  Periodogram;
end;

procedure TFormMain.ActionRawDataExecute(Sender: TObject);
begin
  PlotData;
end;

procedure TFormMain.ActionPhasePlotExecute(Sender: TObject);
begin
  PlotFolded;
end;

procedure TFormMain.ActionPhasePlotSimpleExecute(Sender: TObject);
begin
  PlotFoldedSimple;
end;

procedure TFormMain.ActionPolyFitExecute(Sender: TObject);
begin
  DoPolyFit;
end;

procedure TFormMain.ActionOpenExecute(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFileDir(FFileName);
  OpenDialog1.FileName := ExtractFileName(FFileName);
  if OpenDialog1.Execute then begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

procedure TFormMain.ActionInvertedYExecute(Sender: TObject);
begin
  Chart1.AxisList[0].Inverted := not Chart1.AxisList[0].Inverted;
end;

procedure TFormMain.ActionFitInfoExecute(Sender: TObject);
begin
  ShowInfo(FFitFormula, 'Approximation');
end;

procedure TFormMain.ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
begin
  if AAction = ActionInvertedY then begin
    (AAction as TAction).Checked := Chart1.AxisList[0].Inverted;
  end
  else
  if AAction = ActionRawData then begin
    (AAction as TAction).Enabled := ListChartSourceData.Count > 0;
    //(AAction as TAction).Checked := Chart1LineSeriesData.Source = ListChartSourceData;
  end
  else
  if AAction = ActionPhasePlot then begin
    (AAction as TAction).Enabled := ListChartSourceData.Count > 0;
    //(AAction as TAction).Checked := Chart1LineSeriesData.Source = ListChartSourceFoldedData;
  end
  else
  if AAction = ActionPhasePlotSimple then begin
    (AAction as TAction).Enabled := ListChartSourceData.Count > 0;
    //(AAction as TAction).Checked := Chart1LineSeriesData.Source = ListChartSourceFoldedData;
  end
  else
  if AAction = ActionPeriodogram then begin
    (AAction as TAction).Enabled := ListChartSourceData.Count > 0;
  end
  else
  if AAction = ActionPolyFit then begin
    (AAction as TAction).Enabled := ListChartSourceData.Count > 0;
  end
  else
  if AAction = ActionModelAsTable then begin
    (AAction as TAction).Enabled := ListChartSourceModel.Count > 0;
  end
  else
  if AAction = ActionFitInfo then begin
    (AAction as TAction).Enabled := FFitFormula <> '';
  end;
end;

procedure TFormMain.ActionModelAsTableExecute(Sender: TObject);
var
  X, Y: TFloatArray;
  Item: PChartDataItem;
  I: Integer;
begin
  if ListChartSourceModel.Count > 0 then begin
    SetLength(X, ListChartSourceModel.Count);
    SetLength(Y, ListChartSourceModel.Count);
    for I := 0 to ListChartSourceModel.Count - 1 do begin
      Item := ListChartSourceModel.Item[I];
      X[I] := Item^.X;
      Y[I] := Item^.Y;
    end;
    ShowTable(X, Y, 'Time', 'Model');
  end;
end;

procedure TFormMain.CloseFile;
begin
  FFileName := '';
  FFitFormula := '';
  FPeriodogramFirstRun := True;
  Chart1LineSeriesData.Source := nil;
  Chart1LineSeriesModel.Source := nil;
  ListChartSourceData.Clear;
  ListChartSourceFoldedData.Clear;
  ListChartSourceModel.Clear;
  ListChartSourceFoldedModel.Clear;
  SetAxisBoundaries(-1, 1, -1, 1, True, True);
  if Assigned(FormPhaseDialog) then begin
    FormPhaseDialog.CurrentEpoch := NaN;
    FormPhaseDialog.CurrentPeriod := NaN;
  end;
  unitDFTparamDialog.SetCurrentFrequencyMin(NaN);
  unitDFTparamDialog.SetCurrentFrequencyMax(NaN);
  unitDFTparamDialog.SetCurrentFrequencyResolution(NaN);
  unitDFTparamDialog.SetCurrentTrendDegree(0);
  unitDFTparamDialog.SetCurrentTrigPolyDegree(1);
  unitFitParamDialog.SetCurrentPeriod(NaN);
  unitFitParamDialog.SetCurrentTrendDegree(0);
  unitFitParamDialog.SetCurrentTrigPolyDegree(1);

  CloseDFTdialog;
end;

procedure TFormMain.OpenFile(const AFileName: string);
var
  X, Y: TFloatArray;
  I: Integer;
begin
  CloseFile;
  try
     ReadData(AFileName, X, Y);
  except
    on E: Exception do begin
      ShowMessage(E.Message);
      Exit;
    end;
  end;
  if Length(X) < 1 then begin
    ShowMessage('No data read');
    Exit;
  end;
  FFileName := AFileName;
  FormPhaseDialog.CurrentEpoch := (MaxValue(X) + MinValue(X)) / 2.0;
  for I := 0 to Length(X) - 1 do begin
    ListChartSourceData.Add(X[I], Y[I]);
  end;
  PlotData;
end;

procedure TFormMain.PlotData;
var
  SourceExtent: TDoubleRect;
begin
  if ListChartSourceData.Count > 0 then begin
    StatusBar1.Panels[0].Text := '';
    StatusBar1.Panels[1].Text := '';
    Chart1LineSeriesData.Source := nil;
    Chart1LineSeriesModel.Source := nil;
    SourceExtent := ListChartSourceData.Extent;
    SetAxisBoundaries(SourceExtent.coords[1], SourceExtent.coords[3], SourceExtent.coords[2], SourceExtent.coords[4], True, True);
    Chart1LineSeriesData.Source := ListChartSourceData;
    if ListChartSourceModel.Count > 0 then
      Chart1LineSeriesModel.Source := ListChartSourceModel;
  end;
end;

procedure TFormMain.PlotFolded;
begin
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  PhasePlot(@CalcAndPlotFoldedProc);
end;

procedure TFormMain.PlotFoldedSimple;
begin
  if ListChartSourceFoldedData.Count > 0 then begin
    if (ListChartSourceModel.Count > 0) and (ListChartSourceFoldedModel.Count < 1) then
      CalculateModelPhasePlot;
    PlotFoldedProc;
  end
  else begin
    PlotFolded;
  end;
end;

procedure TFormMain.PlotFoldedProc;
var
  SourceExtent: TDoubleRect;
begin
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  Chart1LineSeriesData.Source := nil;
  Chart1LineSeriesModel.Source := nil;
  SourceExtent := ListChartSourceFoldedData.Extent;
  SetAxisBoundaries(-1.0, 1.0, SourceExtent.coords[2], SourceExtent.coords[4], False, True);
  Chart1LineSeriesData.Source := ListChartSourceFoldedData;
  if ListChartSourceFoldedModel.Count > 0 then
    Chart1LineSeriesModel.Source := ListChartSourceFoldedModel;
  StatusBar1.Panels[1].Text := ' P= ' + FloatToStr(FormPhaseDialog.CurrentPeriod) + ^I' E= ' + FloatToStr(FormPhaseDialog.CurrentEpoch) + ' ';
end;

procedure TFormMain.CalculateModelPhasePlot;
var
  I: Integer;
  Item: PChartDataItem;
  Period, Epoch, X, Y, Phase: Double;
begin
  Period := FormPhaseDialog.CurrentPeriod;
  Epoch := FormPhaseDialog.CurrentEpoch;
  ListChartSourceFoldedModel.Clear;
  if IsNaN(Period) or IsNaN(Epoch) then
    Exit;
  for I := 0 to ListChartSourceModel.Count - 1 do begin
    Item := ListChartSourceModel.Item[I];
    X := Item^.X;
    Y := Item^.Y;
    Phase := CalculatePhase(X, Period, Epoch);
    ListChartSourceFoldedModel.Add(Phase, Y);
    ListChartSourceFoldedModel.Add(Phase - 1.0, Y);
  end;
  ListChartSourceFoldedModel.Sort;
end;

procedure TFormMain.CalcAndPlotFoldedProc;
var
  I: Integer;
  Item: PChartDataItem;
  Period, Epoch, X, Y, Phase: Double;
begin
  if (ListChartSourceData.Count > 0) then begin
    Period := FormPhaseDialog.CurrentPeriod;
    Epoch := FormPhaseDialog.CurrentEpoch;
    ListChartSourceFoldedData.Clear;
    ListChartSourceFoldedModel.Clear;
    for I := 0 to ListChartSourceData.Count - 1 do begin
      Item := ListChartSourceData.Item[I];
      X := Item^.X;
      Y := Item^.Y;
      Phase := CalculatePhase(X, Period, Epoch);
      ListChartSourceFoldedData.Add(Phase, Y);
      ListChartSourceFoldedData.Add(Phase - 1.0, Y);
    end;
    CalculateModelPhasePlot;
    PlotFoldedProc;
  end;
end;

procedure TFormMain.SetAxisBoundaries(Xmin, Xmax, Ymin, Ymax: Double; ExpandX, ExpandY: Boolean);
var
  MinV, MaxV, Extent: ArbFloat;
begin
  MinV := Xmin;
  MaxV := Xmax;
  Extent := MaxV - MinV;
  if ExpandX then begin
    MaxV := MaxV + Extent * 0.02;
    MinV := MinV - Extent * 0.02;
  end;
  Chart1.Extent.XMin := MinV;
  Chart1.Extent.XMax := MaxV;
  Chart1.Extent.UseXMin := True;
  Chart1.Extent.UseXMax := True;
  Chart1.AxisList[1].Marks.Range.Min := MinV;
  Chart1.AxisList[1].Marks.Range.Max := MaxV;
  Chart1.AxisList[1].Marks.Range.UseMin := True;
  Chart1.AxisList[1].Marks.Range.UseMax := True;

  MinV := Ymin;
  MaxV := Ymax;
  Extent := MaxV - MinV;
  if ExpandY then begin
    MaxV := MaxV + Extent * 0.02;
    MinV := MinV - Extent * 0.02;
  end;
  Chart1.Extent.YMin := MinV;
  Chart1.Extent.YMax := MaxV;
  Chart1.Extent.UseYMin := True;
  Chart1.Extent.UseYMax := True;
  Chart1.AxisList[0].Marks.Range.Min := MinV;
  Chart1.AxisList[0].Marks.Range.Max := MaxV;
  Chart1.AxisList[0].Marks.Range.UseMin := True;
  Chart1.AxisList[0].Marks.Range.UseMax := True;
end;

procedure TFormMain.DoPolyFit;
var
  X: TFloatArray;
  Y: TFloatArray;
  Frequency: Double;
  TrendDegree: Integer;
  TrigPolyDegree: Integer;
  trend, trig_fit: TFloatArray;
  meanTime: ArbFloat;
  fitXmin, fitXmax, fitXstep: ArbFloat;
  Xfit, Yfit: TFloatArray;
  Formula: string;
  Item: PChartDataItem;
  I: Integer;
begin
  FFitFormula := '';
  if ListChartSourceData.Count > 0 then begin
    if not GetFitParams(Frequency, TrendDegree, TrigPolyDegree) then
      Exit;
    Chart1LineSeriesModel.Source := nil;
    SetLength(X, ListChartSourceData.Count);
    SetLength(Y, ListChartSourceData.Count);
    for I := 0 to ListChartSourceData.Count - 1 do begin
      Item := ListChartSourceData.Item[I];
      X[I] := Item^.X;
      Y[I] := Item^.Y;
    end;
    meanTime := Mean(X);
    for I := 0 to Length(X) - 1 do begin
      X[I] := X[I] - meanTime;
    end;
    fitXmin := MinValue(X);
    fitXmax := MaxValue(X);
    fitXstep := (fitXmax - fitXmin) / (ListChartSourceData.Count * 3);
    try
      PolyFit(X, Y, Frequency, TrendDegree, TrigPolyDegree, fitXmin, fitXmax, fitXstep, Xfit, Yfit, FFitFormula);
      FFitFormula := 'timeZeroPoint is ' + FloatToStr(meanTime) + ^M^J^M^J + 'f(t: real): real {' + ^M^J + FFitFormula + '}';
    except
      on E: Exception do begin
        ShowMessage('Error: '^M^J + E.Message);
        Exit;
      end;
    end;
    ListChartSourceModel.Clear;
    ListChartSourceFoldedModel.Clear;
    for I := 0 to Length(Xfit) - 1 do begin
      ListChartSourceModel.Add(Xfit[I] + meanTime, Yfit[I]);
    end;
    if Chart1LineSeriesData.Source = ListChartSourceData then
      Chart1LineSeriesModel.Source := ListChartSourceModel
    else
    if Chart1LineSeriesData.Source = ListChartSourceFoldedData then begin
      CalculateModelPhasePlot;
      Chart1LineSeriesModel.Source := ListChartSourceFoldedModel;
    end
    else
      Chart1LineSeriesModel.Source := nil;
  end;
end;

procedure TFormMain.Periodogram;
var
  frequencies, periods, amp, power: TFloatArray;
  params: TDCDFTparameters;
  Item: PChartDataItem;
  I: Integer;
begin
  if ListChartSourceData.Count > 0 then begin
    SetLength(params.X, ListChartSourceData.Count);
    SetLength(params.Y, ListChartSourceData.Count);
    for I := 0 to ListChartSourceData.Count - 1 do begin
      Item := ListChartSourceData.Item[I];
      params.X[I] := Item^.X;
      params.Y[I] := Item^.Y;
    end;
    if FPeriodogramFirstRun then begin
      unitdftparamdialog.SetCurrentFrequencyResolution(0.05 / (MaxValue(params.X) - MinValue(params.X)));
      FPeriodogramFirstRun := False;
    end;
    if not GetDFTparams(params.FrequencyMin, params.FrequencyMax, params.FrequencyResolution, params.TrendDegree, params.TrigPolyDegree) then
      Exit;
    CloseDFTdialog;
    params.Error := '';
    try
      DoLongOp.DoLongOperation(@DoDCDFT, @params, @DFTGlobalTerminate, 'Periodogram');
    except
      on E: Exception do begin
        ShowMessage(E.Message);
        Exit;
      end;
    end;
    if params.Error <> '' then begin
      ShowMessage('Error:'^M^J + params.Error);
      Exit;
    end;
    //ShowMessage('Done!');
    PlotDFTresult(params.frequencies, params.periods, params.power);
  end;
end;

procedure TFormMain.DCDFTThreadOnTerminate(Sender: TObject);
begin
  FDFTThreadTerminated := True;
end;

procedure TFormMain.DFTGlobalTerminate(Sender: TObject);
begin
  unitDFT.SetGlobalTerminateAllThreads(True);
end;

function TFormMain.DoDCDFT(params: Pointer; ProgressCaptionProc: TProgressCaptionProc): Integer;
var
  DCDFTThread: TDFTThread;
begin
  Result := 0;
  FDFTThreadTerminated := False;
  DCDFTThread := TDFTThread.Create(PDCDFTparameters(params), @DCDFTThreadOnTerminate);
  if Assigned(DCDFTThread.FatalException) then
    raise DCDFTThread.FatalException;
  DCDFTThread.Start;
  while not FDFTThreadTerminated do begin
    Application.ProcessMessages;
    Sleep(1);
  end;
end;


end.


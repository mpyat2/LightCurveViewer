unit unitMain;

{$mode objfpc}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, IniFiles, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, TAGraph, TASources, TASeries, TACustomSource, TATools,
  DoLongOp, common;

type

  { TFormMain }

  TFormMain = class(TForm)
    ActionSaveVisible: TAction;
    ActionObservations: TAction;
    ActionAbout: TAction;
    ActionModelInfo: TAction;
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
    MenuItemSaveVisible: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemPeriodogram: TMenuItem;
    MenuItemApproximationInfo: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemObservations: TMenuItem;
    SaveDialog1: TSaveDialog;
    Separator1: TMenuItem;
    MenuItemPolyFit: TMenuItem;
    MenuAnalyses: TMenuItem;
    MenuItemRawData: TMenuItem;
    MenuItemPhasePlot: TMenuItem;
    MenuItemInvertedY: TMenuItem;
    MenuView: TMenuItem;
    MenuItemExit: TMenuItem;
    OpenDialog1: TOpenDialog;
    Separator2: TMenuItem;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionInvertedYExecute(Sender: TObject);
    procedure ActionModelInfoExecute(Sender: TObject);
    procedure ActionObservationsExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionPeriodogramExecute(Sender: TObject);
    procedure ActionPhasePlotExecute(Sender: TObject);
    procedure ActionPhasePlotSimpleExecute(Sender: TObject);
    procedure ActionPolyFitExecute(Sender: TObject);
    procedure ActionRawDataExecute(Sender: TObject);
    procedure ActionSaveVisibleExecute(Sender: TObject);
    procedure Chart1MouseEnter(Sender: TObject);
    procedure Chart1MouseLeave(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FFileName: string;
    FPeriodogramFirstRun: Boolean;
    FDFTThreadTerminated: Boolean;
    FFitFormula: string;
    FFitInfo: string;
    FFitAtPoints: array[0..2] of TFloatArray;
    procedure UpdateTitle;
    procedure CloseFile;
    procedure OpenFile(const AFileName: string);
    procedure SaveFileAs(const AFileName: string; const X, Y: TFloatArray);
    procedure SaveDataSettings;
    procedure LoadDataSettings;
    procedure SaveChartSettings(const Ini: TIniFile; const Section: string);
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
  Windows, math, TAChartUtils, unitPhaseDialog, unitFitParamDialog,
  unitDFTparamDialog, unitDFTdialog, unitDFT, unitTableDialog,
  unitModelInfoDialog, dftThread, dataio, unitAbout;

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

procedure TFormMain.Chart1MouseEnter(Sender: TObject);
begin
  //
end;

procedure TFormMain.Chart1MouseLeave(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := '';
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

procedure TFormMain.ActionSaveVisibleExecute(Sender: TObject);

  function ValInRange(V: Double; Range1, Range2: Double): Boolean;
  begin
    Result := (V >= Min(Range1, Range2)) and (V <= Max(Range1, Range2));
  end;

var
  X1, Y1: TFloatArray;
  ItemX, ItemY: Double;
  Item: PChartDataItem;
  N, I: Integer;
begin
  if ListChartSourceData.Count > 0 then begin
    SetLength(X1, ListChartSourceData.Count);
    SetLength(Y1, ListChartSourceData.Count);
    N := 0;
    for I := 0 to ListChartSourceData.Count - 1 do begin
      Item := ListChartSourceData.Item[I];
      ItemX := Item^.X;
      ItemY := Item^.Y;
      if ValInRange(ItemX, Chart1.CurrentExtent.a.X, Chart1.CurrentExtent.b.X) and
         ValInRange(ItemY, Chart1.CurrentExtent.a.Y, Chart1.CurrentExtent.b.Y) then
      begin
        X1[N] := ItemX;
        Y1[N] := ItemY;
        Inc(N);
      end;
    end;
    SetLength(X1, N);
    SetLength(Y1, N);
    //ShowTable(X1, Y1, 'X', 'Y');
    SaveDialog1.InitialDir := SaveDialog1.InitialDir;
    SaveDialog1.FileName := '';
    if SaveDialog1.Execute then begin
      SaveFileAs(SaveDialog1.FileName, X1, Y1);
    end;
  end;
end;

procedure TFormMain.ActionInvertedYExecute(Sender: TObject);
begin
  Chart1.AxisList[0].Inverted := not Chart1.AxisList[0].Inverted;
  SaveDataSettings;
end;

procedure TFormMain.ActionAboutExecute(Sender: TObject);
begin
  About;
end;

procedure TFormMain.ActionModelInfoExecute(Sender: TObject);
var
  X1, Y1: TFloatArray;
  Item: PChartDataItem;
  I: Integer;
begin
  if ListChartSourceModel.Count > 0 then begin
    SetLength(X1, ListChartSourceModel.Count);
    SetLength(Y1, ListChartSourceModel.Count);
    for I := 0 to ListChartSourceModel.Count - 1 do begin
      Item := ListChartSourceModel.Item[I];
      X1[I] := Item^.X;
      Y1[I] := Item^.Y;
    end;
    ShowModelInfo(FFitInfo, FFitFormula,
                  X1, Y1,
                  FFitAtPoints[0], FFitAtPoints[1], FFitAtPoints[2]);
  end;
end;

procedure TFormMain.ActionObservationsExecute(Sender: TObject);
var
  X1, Y1: TFloatArray;
  Item: PChartDataItem;
  I: Integer;
begin
  if ListChartSourceData.Count > 0 then begin
    SetLength(X1, ListChartSourceData.Count);
    SetLength(Y1, ListChartSourceData.Count);
    for I := 0 to ListChartSourceData.Count - 1 do begin
      Item := ListChartSourceData.Item[I];
      X1[I] := Item^.X;
      Y1[I] := Item^.Y;
    end;
    ShowTable(X1, Y1, 'X', 'Y');
  end;
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
  if AAction = ActionModelInfo then begin
    (AAction as TAction).Enabled := ListChartSourceModel.Count > 0;
  end
  else
  if AAction = ActionObservations then begin
    (AAction as TAction).Enabled := ListChartSourceData.Count > 0;
  end
  else
  if AAction = ActionSaveVisible then begin
    (AAction as TAction).Enabled := ListChartSourceData.Count > 0;
  end;
end;

procedure TFormMain.UpdateTitle;
begin
  Caption := 'LCV';
  if FFileName <> '' then
    Caption := ExtractFileName(FFileName) + ' - ' + Caption;
end;

procedure TFormMain.CloseFile;
var
  tempDArray: TDouble3Array = (NaN, NaN, NaN);
  tempIArray: TInt3Array = (0, 0, 0);
begin
  FFileName := '';
  UpdateTitle;
  FFitFormula := '';
  FFitInfo := '';
  FFitAtPoints[0] := nil;
  FFitAtPoints[1] := nil;
  FFitAtPoints[2] := nil;
  FPeriodogramFirstRun := True;
  Chart1LineSeriesData.Source := nil;
  Chart1LineSeriesModel.Source := nil;
  ListChartSourceData.Clear;
  ListChartSourceFoldedData.Clear;
  ListChartSourceModel.Clear;
  ListChartSourceFoldedModel.Clear;
  SetAxisBoundaries(-1, 1, -1, 1, True, True);
  unitPhaseDialog.SetCurrentEpoch(NaN);
  unitPhaseDialog.SetCurrentPeriod(NaN);
  unitDFTparamDialog.SetCurrentFrequencyMin(NaN);
  unitDFTparamDialog.SetCurrentFrequencyMax(NaN);
  unitDFTparamDialog.SetCurrentFrequencyResolution(NaN);
  unitDFTparamDialog.SetCurrentTrendDegree(0);
  unitDFTparamDialog.SetCurrentTrigPolyDegree(1);
  unitFitParamDialog.SetCurrentTrendDegree(1);
  unitFitParamDialog.SetCurrentPeriods(tempDArray);
  unitFitParamDialog.SetCurrentTrigPolyDegrees(tempIArray);
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
    ShowMessage('No data read. Aborted.');
    Exit;
  end;
  if Length(X) < 2 then begin
    ShowMessage('Only one data point read. Aborted.');
    Exit;
  end;

  FFileName := AFileName;
  UpdateTitle;
  unitPhaseDialog.SetCurrentEpoch((MaxValue(X) + MinValue(X)) / 2.0);
  for I := 0 to Length(X) - 1 do begin
    ListChartSourceData.Add(X[I], Y[I]);
  end;
  LoadDataSettings;
  PlotData;
end;

procedure TFormMain.SaveFileAs(const AFileName: string; const X, Y: TFloatArray);
var
  PropsFileName: string;
  Ini: TIniFile;
begin
  try
    PropsFileName := AFileName + '.lcv.props';
    WriteData(AFileName, X, Y);
    if FileExists(PropsFileName) then begin
      if not SysUtils.DeleteFile(PropsFileName) then
        ShowMessage('Cannot delete ' + PropsFileName);
    end;
    Ini := TMemIniFile.Create(PropsFileName);
    try
      SaveChartSettings(Ini, 'SETTINGS');
    finally
      FreeAndNil(Ini);
    end;
  except
    on E: Exception do begin
      ShowMessage(E.Message);
      Exit;
    end;
  end;
end;

procedure TFormMain.LoadDataSettings;
var
  Ini: TMemIniFile;
begin
  try
    Ini := TMemIniFile.Create(FFileName + '.lcv.props');
    try
      unitFitParamDialog.LoadParameters(Ini, 'SETTINGS');
      unitDFTparamDialog.LoadParameters(Ini, 'SETTINGS');
      unitPhaseDialog.LoadParameters(Ini, 'SETTINGS');
      Chart1.AxisList[0].Inverted := Ini.ReadBool('SETTINGS', 'Yinverted', True);
    finally
      FreeAndNil(Ini);
    end;
  except
    on E: Exception do begin
      ShowMessage(E.Message);
    end;
  end;
end;

procedure TFormMain.SaveDataSettings;
var
  Ini: TMemIniFile;
begin
  try
    Ini := TMemIniFile.Create(FFileName + '.lcv.props');
    try
      Ini.EraseSection('SETTINGS');
      unitFitParamDialog.SaveParameters(Ini, 'SETTINGS');
      unitDFTparamDialog.SaveParameters(Ini, 'SETTINGS');
      unitPhaseDialog.SaveParameters(Ini, 'SETTINGS');
      SaveChartSettings(Ini, 'SETTINGS');
      Ini.UpdateFile;
    finally
      FreeAndNil(Ini);
    end;
  except
    on E: Exception do begin
      ShowMessage(E.Message);
    end;
  end;
end;

procedure TFormMain.SaveChartSettings(const Ini: TIniFile; const Section: string);
begin
  Ini.WriteBool(Section, 'Yinverted', Chart1.AxisList[0].Inverted);
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
  StatusBar1.Panels[1].Text := ' P= ' + FloatToStr(unitPhaseDialog.GetCurrentPeriod) + ^I' E= ' + FloatToStr(unitPhaseDialog.GetCurrentEpoch) + ' ';
end;

procedure TFormMain.CalculateModelPhasePlot;
var
  I: Integer;
  Item: PChartDataItem;
  Period, Epoch, X, Y, Phase: Double;
begin
  Period := unitPhaseDialog.GetCurrentPeriod;
  Epoch := unitPhaseDialog.GetCurrentEpoch;
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
  SaveDataSettings;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
  if (ListChartSourceData.Count > 0) then begin
    Period := unitPhaseDialog.GetCurrentPeriod;
    Epoch := unitPhaseDialog.GetCurrentEpoch;
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
  MinV, MaxV, Extent: Double;
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

  function FitStepFromFrequencies(Freq: TDouble3Array): Double;
  var
    I: Integer;
    F: Double;
  begin
    Result := NaN;
    F := NaN;
    for I := 0 to Length(Freq) - 1 do begin
      if IsNan(F) or (not IsNan(Freq[I]) and (Freq[I] > F)) then
        F := Freq[I];
    end;
    if not IsNan(F) and (F > 0) then
      Result := 1.0 / F * 0.01;
  end;

var
  X: TFloatArray;
  Y: TFloatArray;
  TrendDegree: Integer;
  TrigPolyDegrees: TInt3Array;
  Frequencies: TDouble3Array;
  NofParameters: Integer;
  n_points: Integer;
  OCsquared: Double;
  meanTime: Double;
  fitXmin, fitXmax, fitXstep: Double;
  nfit: Integer;
  Xfit, Yfit: TFloatArray;
  Item: PChartDataItem;
  I: Integer;
begin
  if ListChartSourceData.Count > 0 then begin
    if not GetFitParams(TrendDegree, TrigPolyDegrees, Frequencies) then
      Exit;
    SaveDataSettings;
    FFitFormula := '';
    FFitInfo := '';
    FFitAtPoints[0] := nil;
    FFitAtPoints[1] := nil;
    FFitAtPoints[2] := nil;
    Chart1LineSeriesModel.Source := nil;
    SetLength(X, ListChartSourceData.Count);
    SetLength(Y, ListChartSourceData.Count);
    for I := 0 to ListChartSourceData.Count - 1 do begin
      Item := ListChartSourceData.Item[I];
      X[I] := Item^.X;
      Y[I] := Item^.Y;
    end;
    meanTime := Mean(X);
    n_points := Length(X);
    for I := 0 to n_points - 1 do begin
      X[I] := X[I] - meanTime;
    end;

    fitXmin := MinValue(X);
    fitXmax := MaxValue(X);
    fitXstep := FitStepFromFrequencies(Frequencies);
    if IsNan(fitXstep) then
      fitXstep := (fitXmax - fitXmin) / (ListChartSourceData.Count * 3);
    nfit := Ceil((fitXmax - fitXmin) / fitXstep);
    if nfit > 100000 then begin
      nfit := 100000;
      fitXstep := (fitXmax - fitXmin) / nfit;
    end;

    try
      PolyFit(X, Y, TrendDegree, TrigPolyDegrees, Frequencies, fitXmin, fitXmax, fitXstep, Xfit, Yfit, FFitAtPoints[1], FFitFormula, FFitInfo);
    except
      on E: Exception do begin
        ShowMessage('Error: '^M^J + E.Message);
        Exit;
      end;
    end;
    SetLength(FFitAtPoints[0], n_points);
    for I := 0 to n_points - 1 do begin
      FFitAtPoints[0][I] := X[I] + meanTime;
    end;
    SetLength(FFitAtPoints[2], n_points);
    for I := 0 to n_points - 1 do begin
      FFitAtPoints[2][I] := Y[I];
    end;

    NofParameters := 1 + TrendDegree;
      for I := 0 to Length(TrigPolyDegrees) - 1 do
        NofParameters := NofParameters + TrigPolyDegrees[I] * 2;

    OCsquared := CalcResidualSquared(FFitAtPoints[2], FFitAtPoints[1]);

    FFitFormula := '# Python'^M^J^M^J +
                   'import numpy as np'^M^J +
                   'import math'^M^J +
                   'import matplotlib.pyplot as plt'^M^J^M^J +
                   'timeZeroPoint = ' + FloatToStrLocaleIndependent(meanTime) + ^M^J^M^J +
                   'def f(t):' + ^M^J + ' return \' + ^M^J +
                   FFitFormula + ^M^J^M^J +
                   't_min = ' + FloatToStrLocaleIndependent(meanTime + fitXmin) + ^M^J +
                   't_max = ' + FloatToStrLocaleIndependent(meanTime + fitXmin + nfit * fitXstep) + ^M^J +
                   'n = ' + IntToStr(nfit + 1) + ^M^J +
                   't = np.linspace(t_min, t_max, n)' + ^M^J +
                   'v = np.array(list(map(f, t)))' + ^M^J +
                   'plt.plot(t, v)' + ^M^J +
                   'plt.show()' + ^M^J;

    FFitInfo := FFitInfo + ^M^J + 'timeZeroPoint = ' + Trim(FloatToStrMod(meanTime)) + ^M^J^M^J;
    FFitInfo := FFitInfo + '(O - C)^2 = ' + Trim(FloatToStrMod(OCsquared)) + ^M^J^M^J;
    FFitInfo := FFitInfo + 'Number of data points = ' + IntToStr(n_points) + ^M^J;
    FFitInfo := FFitInfo + 'Number of parameters = ' + IntToStr(NofParameters) + ^M^J;
    FFitInfo := FFitInfo + 'sigma = ' + Trim(FloatToStrMod(Power(OCsquared * Double(NofParameters) / Double(n_points) / Double(n_points - NofParameters), 0.5))) + ^M^J;

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
  DialogCaption: string;
  t0: TDateTime;
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
      if not unitDftParamDialog.IsResolutionDefined then
        unitDftParamDialog.SetCurrentFrequencyResolution(0.05 / (MaxValue(params.X) - MinValue(params.X)));
      FPeriodogramFirstRun := False;
    end;
    if not GetDFTparams(params.FrequencyMin, params.FrequencyMax, params.FrequencyResolution, params.TrendDegree, params.TrigPolyDegree) then
      Exit;
    SaveDataSettings;
    CloseDFTdialog;
    params.Error := '';
    t0 := Now;
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
    DialogCaption := Format('Periodogram | Trend degree = %d; Trig. Polynomial Degree = %d | Time = %fs',
                            [params.TrendDegree, params.TrigPolyDegree, (Now - t0) * 24 * 60 * 60]);
    PlotDFTresult(DialogCaption, params.frequencies, params.periods, params.power);
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
  DCDFTThread := TDFTThread.Create(PDCDFTparameters(params), @DCDFTThreadOnTerminate, ProgressCaptionProc);
  if Assigned(DCDFTThread.FatalException) then
    raise DCDFTThread.FatalException;
  DCDFTThread.Start;
  while not FDFTThreadTerminated do begin
    Application.ProcessMessages;
    Sleep(1);
  end;
end;


end.


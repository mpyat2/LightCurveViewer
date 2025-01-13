unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ComCtrls, TAGraph, TASources, TASeries, TACustomSource, DoLongOp, unitdcdft;

type

  { TFormMain }

  TFormMain = class(TForm)
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
    ImageList1: TImageList;
    ListChartSourceFoldedData: TListChartSource;
    ListChartSourceData: TListChartSource;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
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
    procedure ActionInvertedYExecute(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionPeriodogramExecute(Sender: TObject);
    procedure ActionPhasePlotExecute(Sender: TObject);
    procedure ActionPhasePlotSimpleExecute(Sender: TObject);
    procedure ActionRawDataExecute(Sender: TObject);
    procedure Chart1Click(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FFileName: string;
    FPeriodogramFirstRun: Boolean;
    FDCDFTThreadOnTerminated: Boolean;
    procedure CloseFile;
    procedure OpenFile(const AFileName: string);
    procedure PlotData;
    procedure PlotFolded;
    procedure PlotFoldedSimple;
    procedure PlotFoldedProc;
    procedure CalcAndPlotFoldedProc(Period, Epoch: Double);
    procedure SetAxisBoundaries(Xmin, Xmax, Ymin, Ymax: Double; ExpandX, ExpandY: Boolean);
    procedure Periodogram;
    function DoDCDFT(params: Pointer; ProgressCaptionProc: TProgressCaptionProc): Integer;
    procedure DCDFTThreadOnTerminate(Sender: TObject);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  Windows, math, typ, TAChartUtils, unitphasedialog, unitdftparamdialog, unitdftdialog, dataio;

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

procedure TFormMain.Chart1Click(Sender: TObject);
begin
  //
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
      StatusBar1.Panels[0].Text := Format(' %f'^I^I' %f ', [Pg.X, Pg.Y]);
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

procedure TFormMain.ActionOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

procedure TFormMain.ActionInvertedYExecute(Sender: TObject);
begin
  Chart1.AxisList[0].Inverted := not Chart1.AxisList[0].Inverted;
end;

procedure TFormMain.ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
begin
  if AAction = ActionInvertedY then begin
    (AAction as TAction).Checked := Chart1.AxisList[0].Inverted;
  end
  else
  if AAction = ActionRawData then begin
    (AAction as TAction).Enabled := ListChartSourceData.Count > 0;
    (AAction as TAction).Checked := Chart1LineSeriesData.Source = ListChartSourceData;
  end
  else
  if AAction = ActionPhasePlot then begin
    (AAction as TAction).Enabled := ListChartSourceData.Count > 0;
    (AAction as TAction).Checked := Chart1LineSeriesData.Source = ListChartSourceFoldedData;
  end
  else
  if AAction = ActionPhasePlotSimple then begin
    (AAction as TAction).Enabled := ListChartSourceData.Count > 0;
    (AAction as TAction).Checked := Chart1LineSeriesData.Source = ListChartSourceFoldedData;
  end
  else
  if AAction = ActionPeriodogram then begin
    (AAction as TAction).Enabled := ListChartSourceData.Count > 0;
  end;
end;

procedure TFormMain.CloseFile;
begin
  FFileName := '';
  FPeriodogramFirstRun := True;
  Chart1LineSeriesData.Source := nil;
  ListChartSourceData.Clear;
  ListChartSourceFoldedData.Clear;
  SetAxisBoundaries(-1, 1, -1, 1, True, True);
  if Assigned(FormPhaseDialog) then begin
    FormPhaseDialog.CurrentEpoch := NaN;
    FormPhaseDialog.CurrentPeriod := NaN;
  end;
end;

procedure TFormMain.OpenFile(const AFileName: string);
var
  X, Y: TFloatArray;
  I: Integer;
begin
  CloseFile;
  try
     ReadCSV(AFileName, X, Y);
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
    SourceExtent := ListChartSourceData.Extent;
    SetAxisBoundaries(SourceExtent.coords[1], SourceExtent.coords[3], SourceExtent.coords[2], SourceExtent.coords[4], True, True);
    Chart1LineSeriesData.Source := ListChartSourceData;
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
  SourceExtent := ListChartSourceFoldedData.Extent;
  SetAxisBoundaries(-1.0, 1.0, SourceExtent.coords[2], SourceExtent.coords[4], False, True);
  Chart1LineSeriesData.Source := ListChartSourceFoldedData;
  StatusBar1.Panels[1].Text := ' P= ' + FloatToStr(FormPhaseDialog.CurrentPeriod) + ^I' E= ' + FloatToStr(FormPhaseDialog.CurrentEpoch) + ' ';
end;

procedure TFormMain.CalcAndPlotFoldedProc(Period, Epoch: Double);
var
  I: Integer;
  Item: PChartDataItem;
  X, Y, Phase: Double;
begin
  if (ListChartSourceData.Count > 0) then begin
    ListChartSourceFoldedData.Clear;
    for I := 0 to ListChartSourceData.Count - 1 do begin
      Item := ListChartSourceData.Item[I];
      X := Item^.X;
      Y := Item^.Y;
      Phase := CalculatePhase(X, Period, Epoch);
      ListChartSourceFoldedData.Add(Phase, Y);
      ListChartSourceFoldedData.Add(Phase - 1.0, Y);
    end;
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

procedure TFormMain.Periodogram;
var
  I: Integer;
  Item: PChartDataItem;
  MinX, MaxX: Double;
  frequencies, periods, amp, power: TFloatArray;
  params: TDCDFTparameters;
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
      MinX := MinValue(params.X);
      MaxX := MaxValue(params.X);
      SetCurrentFrequencyResolution(0.05 / (MaxX - MinX));
      FPeriodogramFirstRun := False;
    end;
    if not GetDFTparams(params.FrequencyMin, params.FrequencyMax, params.FrequencyResolution) then
      Exit;
    FormDFTDialog.Hide;
    params.Error := '';
    try
      DoLongOp.DoLongOperation(@DoDCDFT, @params);
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
    FormDFTDialog.PlotData(params.frequencies, params.periods, params.amp, params.power);
  end;
end;

type
  TDCDFTThread = class(TThread)
  private
    FParamsPtr: PDCDFTparameters;
  protected
    procedure Execute; override;
  public
    constructor Create(AParamsPtr: PDCDFTparameters);
  end;

constructor TDCDFTThread.Create(AParamsPtr: PDCDFTparameters);
begin
  inherited Create(True);
  FParamsPtr := AParamsPtr;
end;

procedure TDCDFTThread.Execute;
begin
  try
    dcdft_proc(FParamsPtr^.X, FParamsPtr^.Y,
               FParamsPtr^.FrequencyMin, FParamsPtr^.FrequencyMax, FParamsPtr^.FrequencyResolution,
               True, 0,
               FParamsPtr^.frequencies, FParamsPtr^.periods, FParamsPtr^.amp, FParamsPtr^.power);
  except
    on E: Exception do begin
      FParamsPtr^.Error := E.Message;
      if FParamsPtr^.Error = '' then
        FParamsPtr^.Error := 'Unknown Error';
    end
    else
      FParamsPtr^.Error := 'Unknown Error';
  end;
end;

procedure TFormMain.DCDFTThreadOnTerminate(Sender: TObject);
begin
  FDCDFTThreadOnTerminated := True;
end;

function TFormMain.DoDCDFT(params: Pointer; ProgressCaptionProc: TProgressCaptionProc): Integer;
var
  DCDFTThread: TDCDFTThread;
begin
  FDCDFTThreadOnTerminated := False;
  DCDFTThread := TDCDFTThread.Create(PDCDFTparameters(params));
  DCDFTThread.OnTerminate := @DCDFTThreadOnTerminate;
  DCDFTThread.FreeOnTerminate := True;
  DCDFTThread.Start;
  while not FDCDFTThreadOnTerminated do begin
    Application.ProcessMessages;
    Sleep(0);
  end;
  Result := 0;
end;


end.


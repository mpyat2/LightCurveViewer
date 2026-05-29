unit unitMain;

{$mode objfpc}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, IniFiles, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, ExtDlgs, ExtCtrls, TAGraph, TASources, TASeries,
  TACustomSource, TATools, TAChartUtils, TACustomSeries, Types, lcvconsts,
  lcvtypes, unitDFT;

type

  { TFormMain }

  TFormMain = class(TForm)
    ActionCustomHColor: TAction;
    ActionYearByYearColor: TAction;
    ActionDayByDayColor: TAction;
    ActionDetrendAlgebraic: TAction;
    ActionStatistics: TAction;
    ActionMagShift: TAction;
    ActionOptions: TAction;
    ActionDetrend: TAction;
    ActionShowErrors: TAction;
    ActionCycleByCycleColor: TAction;
    ActionLogicalExtent: TAction;
    ActionUserManualLocal: TAction;
    ActionChartProperties: TAction;
    ActionUserManual: TAction;
    ActionSaveChartImageAs: TAction;
    ActionStop: TAction;
    ActionCopyChartImage: TAction;
    ActionShowModel: TAction;
    ActionShowData: TAction;
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
    ActionList: TActionList;
    Chart: TChart;
    ChartSeriesModelDownLimit: TLineSeries;
    ChartSeriesModelUpLimit: TLineSeries;
    ChartSeriesModel: TLineSeries;
    ChartSeriesData: TLineSeries;
    ChartToolset: TChartToolset;
    ChartToolsetDataPointClickTool1: TDataPointClickTool;
    ChartToolsetDataPointClickTool2: TDataPointClickTool;
    ChartToolsetPanDragTool1: TPanDragTool;
    ChartToolsetZoomDragTool1: TZoomDragTool;
    ChartToolsetZoomMouseWheelTool1: TZoomMouseWheelTool;
    ImageList: TImageList;
    LCSrcFoldedData: TListChartSource;
    LCSrcData: TListChartSource;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuItemCustomHColor: TMenuItem;
    MenuItemDayByDayColor: TMenuItem;
    MenuItemYearByYearColor: TMenuItem;
    MenuItemDetrendAlgebraic: TMenuItem;
    MenuItemStatistics: TMenuItem;
    MenuItemMagShift: TMenuItem;
    MenuItemOptions: TMenuItem;
    MenuTools: TMenuItem;
    MenuItemDetrend: TMenuItem;
    MenuItemShowErrors: TMenuItem;
    MenuItemPhasePlotNew: TMenuItem;
    MenuItemModelInfo: TMenuItem;
    MenuItemCycleByCycleColor: TMenuItem;
    MenuItemShowModel: TMenuItem;
    MenuItemShowData: TMenuItem;
    MenuItemChartExtent: TMenuItem;
    MenuItemExtent: TMenuItem;
    MenuItemUserManualLocal: TMenuItem;
    MenuItemChartProperties: TMenuItem;
    MenuItemUserManual: TMenuItem;
    MenuItemSavePNG: TMenuItem;
    MenuItemCopyChart: TMenuItem;
    MenuItemSaveVisible: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemPeriodogram: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemObservations: TMenuItem;
    PanelCalculatingMessage: TPanel;
    PopupMenuChart: TPopupMenu;
    SaveDialog: TSaveDialog;
    SavePictureDialog: TSavePictureDialog;
    Separator1: TMenuItem;
    MenuItemPolyFit: TMenuItem;
    MenuAnalyses: TMenuItem;
    MenuItemRawData: TMenuItem;
    MenuItemPhasePlot: TMenuItem;
    MenuItemInvertedY: TMenuItem;
    MenuView: TMenuItem;
    MenuItemExit: TMenuItem;
    OpenDialog: TOpenDialog;
    Separator10: TMenuItem;
    Separator11: TMenuItem;
    Separator12: TMenuItem;
    Separator13: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    Separator7: TMenuItem;
    Separator8: TMenuItem;
    Separator9: TMenuItem;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    ToolButtonStop: TToolButton;
    ToolButtonDiv3: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonDiv1: TToolButton;
    ToolButtonRawData: TToolButton;
    ToolButtonPhasePlot: TToolButton;
    ToolButtonPolyFit: TToolButton;
    ToolButtonDiv2: TToolButton;
    ToolButtonPhasePlotNew: TToolButton;
    ToolButtonPeriodogram: TToolButton;
    UDFSrcModel: TUserDefinedChartSource;
    UDFSrcModelUpLimit: TUserDefinedChartSource;
    UDFSrcModelDownLimit: TUserDefinedChartSource;
    UDFSrcModelFolded: TUserDefinedChartSource;
    UDFSrcModelFoldedUpLimit: TUserDefinedChartSource;
    UDFSrcModelFoldedDownLimit: TUserDefinedChartSource;
    procedure ActionChartPropertiesExecute(Sender: TObject);
    procedure ActionCopyChartImageExecute(Sender: TObject);
    procedure ActionCustomHColorExecute(Sender: TObject);
    procedure ActionCycleByCycleColorExecute(Sender: TObject);
    procedure ActionDayByDayColorExecute(Sender: TObject);
    procedure ActionYearByYearColorExecute(Sender: TObject);
    procedure ActionDetrendAlgebraicExecute(Sender: TObject);
    procedure ActionDetrendExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionInvertedYExecute(Sender: TObject);
    procedure ActionLogicalExtentExecute(Sender: TObject);
    procedure ActionMagShiftExecute(Sender: TObject);
    procedure ActionModelInfoExecute(Sender: TObject);
    procedure ActionObservationsExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionOptionsExecute(Sender: TObject);
    procedure ActionPeriodogramExecute(Sender: TObject);
    procedure ActionPhasePlotExecute(Sender: TObject);
    procedure ActionPhasePlotSimpleExecute(Sender: TObject);
    procedure ActionPolyFitExecute(Sender: TObject);
    procedure ActionRawDataExecute(Sender: TObject);
    procedure ActionSaveChartImageAsExecute(Sender: TObject);
    procedure ActionSaveVisibleExecute(Sender: TObject);
    procedure ActionShowDataExecute(Sender: TObject);
    procedure ActionShowErrorsExecute(Sender: TObject);
    procedure ActionShowModelExecute(Sender: TObject);
    procedure ActionStatisticsExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionUserManualExecute(Sender: TObject);
    procedure ActionUserManualLocalExecute(Sender: TObject);
    procedure ChartMouseEnter(Sender: TObject);
    procedure ChartMouseLeave(Sender: TObject);
    procedure ChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ChartToolsetDataPointClickTool1PointClick(ATool: TChartTool; APoint: TPoint);
    procedure ChartToolsetDataPointClickTool2PointClick(ATool: TChartTool; APoint: TPoint);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UDFSrcModelFoldedGetChartDataItem(ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
    procedure UDFSrcModelGetChartDataItem(ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
  private
    FFileNameParamStr: string;
    FCalculationInProgress: Boolean;
    FFileName: string;
    FObjectName: string;
    FChartSubtitle: string;
    FChartXtitle: string;
    FChartYtitle: string;
    FPeriodogramFirstRun: Boolean;
    FFitFormula: string;
    FFitInfo: string;
    FFitAtPoints: TFitColumnArray;
    FModelData: TFitColumnArray;
    FModelFolded: TFitColumnArray;
    FFoldedDataColorRegions: TFoldedRegions;
    FSimpleColorRegions: TSimpleRegions;
    FChartColorMode: TChartColorMode;
    function GetLCSrcDataCount: Integer;
    function GetLCSrcDataItem(N: Integer): PChartDataItem;
    procedure UpdateColorActions;
    procedure ShowDataPoint(Tool: TDatapointClickTool; AddToFloatTextForm: Boolean);
    procedure UpdateTitle;
    procedure ChartSeriesModelToNil;
    procedure ChartSeriesModelToModel;
    procedure ChartSeriesModelToModelFolded;
    procedure ClearUDFSrcModel;
    procedure ClearUDFSrcModelFolded;
    procedure ClearFitAtPoints;
    procedure ClearModelData;
    procedure ClearModelFolded;
    procedure CloseFile;
    procedure OpenFile(const AFileName: string);
    procedure AsyncOpenFile(Data: PtrInt);
    procedure Detrend(AlgebraicOnly: Boolean);
    procedure SaveFileAs(const AFileName: string; const X, Y, Errors: TDoubleArray);
    procedure SaveDataSettingsAs(AFileName: string);
    procedure LoadDataSettings;
    procedure SaveChartSettings(const Ini: TIniFile; const Section: string);
    procedure LoadChartSettings(const Ini: TIniFile; const Section: string);
    procedure PlotData;
    procedure PlotFolded;
    procedure PlotFoldedSimple;
    procedure PlotFoldedProc;
    procedure MagShift;
    procedure MagShiftProc;
    procedure SetPhaseColor(Data: TListChartSource); overload;
    procedure SetPhaseColor; overload;
    procedure SetSeasonalColor(Mode: Boolean);
    procedure SetCustomHColor;
    function GetCustomHColorArray: TBooleanArray;
    procedure RestoreCustomHColor(const A: TBooleanArray);
    procedure ShowColorLegend(CycleColors: Boolean);
    procedure HideColorLegend;
    procedure CalculateModelPhasePlot;
    procedure CalcFoldedData(Period, Epoch: Double);
    procedure CalcAndPlotFoldedProc;
    procedure SetAxisBoundaries(Xmin, Xmax, Ymin, Ymax: Double);
    procedure Periodogram;
    procedure DoPolyFit;
    procedure DoDCDFT(DCDFTparameters: TDCDFTparameters; NofThreads: Integer);
    procedure ShowPeriodogram(DCDFTparameters: TDCDFTparameters);
    procedure DCDFTThreadOnTerminate(Sender: TObject);
    procedure DFTGlobalTerminate;
    procedure ProgressCaptionProc(const Msg: string);
    procedure LongOpStart;
    procedure LongOpStop;
  public
    property LCSrcDataCount: Integer read GetLCSrcDataCount;
    property CalculationInProgress: Boolean read FCalculationInProgress;
    procedure SaveDataSettings;
    procedure DoPolyFitProc(TrendDegree: Integer; const TrigPolyDegrees: THarmonicIntArray; const Frequencies: THarmonicDblArray);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  lclintf, math, DateUtils, guiutils, unitPhaseDialog, unitMagShiftDialog,
  unitFitParamDialog, unitDFTparamDialog, unitDFTdialog, unitTableDialog,
  unitModelInfoDialog, colorLegend, floattextform, unitFormChartprops,
  unitGetExtent, unitOptionsDialog, unitFormStat, unitAbout, dftThread, dataio,
  sortutils, formatutils, miscutils, fitproc, settings;

const
  XLIST_PHS_COLOR = 0; // index of phase color values in chart source XList (both raw and folded chart sources)
  XLIST_FLD_PTIDX = 1; // folded chart source only: XList index to store raw point indices

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  FFoldedDataColorRegions := TFoldedRegions.Create;
  FSimpleColorRegions := TSimpleRegions.Create;
  CloseFile;
  try
    LoadFormPosition(Self);
    Chart.AxisList[0].Grid.Visible := GetGlobalBoolParameter('ShowGrid', True);
    Chart.AxisList[1].Grid.Visible := Chart.AxisList[0].Grid.Visible;
  except
    // nothing
  end;
  for I := 1 to ParamCount do begin
    S := ParamStr(I);
    if (Length(S) > 0) and (S[1] <> '-') then begin
      FFileNameParamStr := ParamStr(I);
      Application.QueueAsyncCall(@AsyncOpenFile, 0);
      Break;
    end;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFoldedDataColorRegions);
  FreeAndNil(FSimpleColorRegions);
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
    SaveFormPosition(Self);
    HideColorLegend;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not FCalculationInProgress;
end;

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  CloseFile;
  Close;
end;

procedure TFormMain.ActionOptionsExecute(Sender: TObject);
begin
  if ProgramOptions then begin
    Chart.AxisList[0].Grid.Visible := GetGlobalBoolParameter('ShowGrid', True);
    Chart.AxisList[1].Grid.Visible := Chart.AxisList[0].Grid.Visible;
  end;
end;

procedure TFormMain.ChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Pg: TDoublePoint;
begin
  try
    if (ChartSeriesData.Source <> nil) and (ChartSeriesData.Source.Count > 0) then begin
      P := Chart.ScreenToClient(Mouse.CursorPos);
      // To suppress a Chart exception when a file is loaded from commend-line.
      if Chart.ScalingValid then begin
        Pg := Chart.ImageToGraph(P);
        StatusBar.Panels[0].Text := Format(' %g'^I' %g ', [Pg.X, Pg.Y]);
      end
      else
        StatusBar.Panels[0].Text := '';
    end
    else
      StatusBar.Panels[0].Text := '';
  except
    on E: Exception do
      StatusBar.Panels[0].Text := '';
  end;
end;

procedure TFormMain.ChartToolsetDataPointClickTool1PointClick(ATool: TChartTool; APoint: TPoint);
begin
  ShowDataPoint(ATool as TDatapointClickTool, False);
end;

procedure TFormMain.ChartToolsetDataPointClickTool2PointClick(ATool: TChartTool; APoint: TPoint);
begin
  ShowDataPoint(ATool as TDatapointClickTool, True);
end;

procedure TFormMain.UpdateColorActions;
begin
  ActionList.UpdateAction(ActionCycleByCycleColor); // Ubuntu bug (check state not always updated)? (GNOME)
  ActionList.UpdateAction(ActionDayByDayColor); // Ubuntu bug (check state not always updated)? (GNOME)
  ActionList.UpdateAction(ActionYearByYearColor); // Ubuntu bug (check state not always updated)? (GNOME)
  ActionList.UpdateAction(ActionCustomHColor); // Ubuntu bug (check state not always updated)? (GNOME)
end;

procedure TFormMain.ShowDataPoint(Tool: TDatapointClickTool; AddToFloatTextForm: Boolean);

  function FormatPointInfo(X, Y, Error, Phase: Double; Sep: string): string;
  begin
    if IsNaN(Phase) then
      Result := Format('X = %g%sY = %g%sError = %g',             [X, Sep, Y, Sep, Error])
    else
      Result := Format('X = %g%sY = %g%sError = %g%sPhase = %g', [X, Sep, Y, Sep, Error, Sep, Phase]);
  end;

var
  Series: TLineSeries;
  Source: TListChartSource;
  Item, OriginalItem: PChartDataItem;
  S1: String;
  X, X0, Y, Error: Double;
begin
  if Tool.Series = ChartSeriesData then begin
    Series := Tool.Series as TLineSeries;
    if not (Series.Source is TListChartSource) then
      raise Exception.Create('Invalid chart source');
    Source := Series.Source as TListChartSource;
    Item := Source.Item[Tool.PointIndex];
    X := Item^.X;
    Y := Item^.Y;
    Error := Item^.YList[0];
    S1 := 'Unknown series';
    if Series.Source = LCSrcData then begin
      S1 := FormatPointInfo(X, Y, Error, NaN, ^M^J);
    end
    else
    if Series.Source = LCSrcFoldedData then begin
      OriginalItem := GetLCSrcDataItem(Trunc(Item^.XList[XLIST_FLD_PTIDX]));
      if OriginalItem = nil then
        raise Exception.Create('Folded/Original data mismatch');
      X0 := OriginalItem^.X;
      S1 := FormatPointInfo(X0, Y, Error, X, ^M^J);
    end;
    if not AddToFloatTextForm then begin
      if Series.ListSource.Item[Tool.PointIndex]^.Text = S1 then
        Series.ListSource.SetText(Tool.PointIndex, '')
      else
        Series.ListSource.SetText(Tool.PointIndex, S1);
    end else begin
      Series.ListSource.SetText(Tool.PointIndex, S1);
      floattextform.AddText(StringReplace(S1, ^M^J, ^I, [rfReplaceAll]));
    end;
  end;
end;

procedure TFormMain.ChartMouseEnter(Sender: TObject);
begin
  //
end;

procedure TFormMain.ChartMouseLeave(Sender: TObject);
begin
  StatusBar.Panels[0].Text := '';
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

procedure TFormMain.ActionMagShiftExecute(Sender: TObject);
begin
  MagShift;
end;

procedure TFormMain.ActionPolyFitExecute(Sender: TObject);
begin
  DoPolyFit;
end;

procedure TFormMain.ActionOpenExecute(Sender: TObject);
var
  TempFileName: string;
begin
  TempFileName := OpenDialog.FileName;
  if TempFileName <> '' then begin
    OpenDialog.InitialDir := ExtractFileDir(TempFileName);
    OpenDialog.FileName := ExtractFileName(TempFileName);
  end;
  if OpenDialog.Execute then begin
    OpenFile(OpenDialog.FileName);
  end;
end;

procedure TFormMain.ActionSaveVisibleExecute(Sender: TObject);
var
  X1, Y1, Errors: TDoubleArray;
  ItemX, ItemY, Error: Double;
  Item: PChartDataItem;
  N, I: Integer;
  ErrorExist: Boolean;
begin
  if (LCSrcData.Count > 0) and (ChartSeriesData.Source = LCSrcData) then begin
    SetLength(X1, LCSrcData.Count);
    SetLength(Y1, LCSrcData.Count);
    SetLength(Errors, LCSrcData.Count);
    ErrorExist := False;
    N := 0;
    for I := 0 to LCSrcData.Count - 1 do begin
      Item := LCSrcData.Item[I];
      ItemX := Item^.X;
      ItemY := Item^.Y;
      Error := Item^.YList[0];
      if ValInRange(ItemX, Chart.CurrentExtent.a.X, Chart.CurrentExtent.b.X) and
         ValInRange(ItemY, Chart.CurrentExtent.a.Y, Chart.CurrentExtent.b.Y) then
      begin
        X1[N] := ItemX;
        Y1[N] := ItemY;
        Errors[N] := Error;
        Inc(N);
        if Error <> 0.0 then ErrorExist := True;
      end;
    end;
    SetLength(X1, N);
    SetLength(Y1, N);
    if ErrorExist then
      SetLength(Errors, N)
    else
      Errors := nil;
    if SaveDialog.InitialDir = '' then
      SaveDialog.InitialDir := OpenDialog.InitialDir;
    SaveDialog.FileName := '';
    if SaveDialog.Execute then begin
      SaveFileAs(SaveDialog.FileName, X1, Y1, Errors);
    end;
  end;
end;

procedure TFormMain.ActionShowDataExecute(Sender: TObject);
begin
  ChartSeriesData.Active := not ChartSeriesData.Active;
  ActionList.UpdateAction(Sender as TAction); // Ubuntu bug (check state not always updated)? (GNOME)
end;

procedure TFormMain.ActionShowErrorsExecute(Sender: TObject);
begin
  ChartSeriesData.YErrorBars.Visible := not ChartSeriesData.YErrorBars.Visible;
  ActionList.UpdateAction(Sender as TAction); // Ubuntu bug (check state not always updated)? (GNOME)
end;

procedure TFormMain.ActionShowModelExecute(Sender: TObject);
begin
  ChartSeriesModel.Active := not ChartSeriesModel.Active;
  ChartSeriesModelUpLimit.Active := ChartSeriesModel.Active;
  ChartSeriesModelDownLimit.Active := ChartSeriesModel.Active;
  ActionList.UpdateAction(Sender as TAction); // Ubuntu bug (check state not always updated)? (GNOME)
end;

procedure TFormMain.ActionStatisticsExecute(Sender: TObject);
var
  Item: PChartDataItem;
  X, Y: TDoubleArray;
  I: Integer;
begin
  if LCSrcData.Count > 0 then begin
    SetLength(X, LCSrcData.Count);
    SetLength(Y, LCSrcData.Count);
    for I := 0 to LCSrcData.Count - 1 do begin
      Item := LCSrcData.Item[I];
      X[I] := Item^.X;
      Y[I] := Item^.Y;
    end;
    ShowStatistics(X, Y);
  end;
end;

procedure TFormMain.ActionStopExecute(Sender: TObject);
begin
  DFTGlobalTerminate;
end;

procedure TFormMain.ActionInvertedYExecute(Sender: TObject);
begin
  Chart.AxisList[0].Inverted := not Chart.AxisList[0].Inverted;
  ActionList.UpdateAction(Sender as TAction); // Ubuntu bug (check state not always updated)? (GNOME)
  SaveDataSettings;
end;

procedure TFormMain.ActionCycleByCycleColorExecute(Sender: TObject);
var
  CurrentChartColorMode: TChartColorMode;
begin
  CurrentChartColorMode := FChartColorMode;
  // Reset other actions
  FChartColorMode := TChartColorMode.None;
  ChartSeriesData.ColorEach := ceNone;
  HideColorLegend;
  UpdateColorActions;
  if CurrentChartColorMode <> TChartColorMode.Phase then begin
    // Colorize both LCSrcData and LCSrcFoldedData
    FChartColorMode := TChartColorMode.Phase;
    SetPhaseColor;
    ChartSeriesData.ColorEach := cePoint;
    ShowColorLegend(True);
  end;
  ActionList.UpdateAction(Sender as TAction); // Ubuntu bug (check state not always updated)? (GNOME)
end;

procedure TFormMain.ActionDayByDayColorExecute(Sender: TObject);
var
  CurrentChartColorMode: TChartColorMode;
begin
  CurrentChartColorMode := FChartColorMode;
  // Reset other actions
  FChartColorMode := TChartColorMode.None;
  ChartSeriesData.ColorEach := ceNone;
  HideColorLegend;
  UpdateColorActions;
  if CurrentChartColorMode <> TChartColorMode.Day then begin
    // Set colors to both LCSrcData and LCSrcFoldedData
    FChartColorMode := TChartColorMode.Day;
    SetSeasonalColor(False);
    ChartSeriesData.ColorEach := cePoint;
    ShowColorLegend(False);
  end;
  ActionList.UpdateAction(Sender as TAction); // Ubuntu bug (check state not always updated)? (GNOME)
end;

procedure TFormMain.ActionYearByYearColorExecute(Sender: TObject);
var
  CurrentChartColorMode: TChartColorMode;
begin
  CurrentChartColorMode := FChartColorMode;
  // Reset other actions
  FChartColorMode := TChartColorMode.None;
  ChartSeriesData.ColorEach := ceNone;
  HideColorLegend;
  UpdateColorActions;
  if CurrentChartColorMode <> TChartColorMode.Year then begin
    // Set colors to both LCSrcData and LCSrcFoldedData
    FChartColorMode := TChartColorMode.Year;
    SetSeasonalColor(True);
    ChartSeriesData.ColorEach := cePoint;
    ShowColorLegend(False);
  end;
  ActionList.UpdateAction(Sender as TAction); // Ubuntu bug (check state not always updated)? (GNOME)
end;

procedure TFormMain.ActionCustomHColorExecute(Sender: TObject);
var
  CurrentChartColorMode: TChartColorMode;
begin
  CurrentChartColorMode := FChartColorMode;
  // Reset other actions
  FChartColorMode := TChartColorMode.None;
  ChartSeriesData.ColorEach := ceNone;
  HideColorLegend;
  UpdateColorActions;
  if CurrentChartColorMode <> TChartColorMode.Custom then begin
    // Set colors to both LCSrcData and LCSrcFoldedData
    FChartColorMode := TChartColorMode.Custom;
    SetCustomHColor;
    ChartSeriesData.ColorEach := cePoint;
    // Color legend?
  end;
  ActionList.UpdateAction(Sender as TAction); // Ubuntu bug (check state not always updated)? (GNOME)
end;

procedure TFormMain.ActionLogicalExtentExecute(Sender: TObject);
var
  Extent: TDoubleRect;
begin
  Extent := Chart.LogicalExtent;
  if GetExtent(Extent) then begin
    Chart.LogicalExtent := Extent;
  end;
end;

procedure TFormMain.ActionUserManualExecute(Sender: TObject);
begin
{$IFDEF Linux}
  try
    OpenURLasync(defRemoteManual);
  except
    on Ex: Exception do
      ShowMessage(Ex.Message);
  end;
{$ELSE}
  if not OpenURL(defRemoteManual) then
    ShowMessage('Cannot open ' + defRemoteManual);
{$ENDIF}
end;

procedure TFormMain.ActionUserManualLocalExecute(Sender: TObject);
var
  DocName: string;
begin
  DocName := ExtractFilePath(ParamStr(0)) + defLocalManual;
  if not OpenDocument(DocName) then
    ShowMessage('Cannot open ' + DocName);
end;

procedure TFormMain.ActionAboutExecute(Sender: TObject);
begin
  About;
end;

procedure TFormMain.ActionModelInfoExecute(Sender: TObject);
begin
  if Length(FModelData[FitColumnType.x]) > 0 then begin
    ShowModelInfo(FFitInfo, FFitFormula, FModelData, FFitAtPoints);
  end;
end;

procedure TFormMain.ActionDetrendAlgebraicExecute(Sender: TObject);
begin
  if Length(FFitAtPoints[FitColumnType.x]) > 0 then begin
    Detrend(True);
  end;
end;

procedure TFormMain.ActionDetrendExecute(Sender: TObject);
begin
  if Length(FFitAtPoints[FitColumnType.x]) > 0 then begin
    Detrend(False);
  end;
end;

procedure TFormMain.ActionObservationsExecute(Sender: TObject);
var
  X1, Y1, Errors: TDoubleArray;
  Period, Epoch: Double;
  DialogTitle: string;
  Item: PChartDataItem;
  I: Integer;
begin
  if LCSrcData.Count > 0 then begin
    SetLength(X1, LCSrcData.Count);
    SetLength(Y1, LCSrcData.Count);
    SetLength(Errors, LCSrcData.Count);
    for I := 0 to LCSrcData.Count - 1 do begin
      Item := LCSrcData.Item[I];
      X1[I] := Item^.X;
      Y1[I] := Item^.Y;
      Errors[I] := Item^.YList[0];
    end;
    Period := unitPhaseDialog.GetCurrentPeriod;
    Epoch := unitPhaseDialog.GetCurrentEpoch;
    DialogTitle := 'Observations';
    if (not IsNan(Period)) and (not IsNan(Epoch)) then
      DialogTitle := DialogTitle + ' [Period ' + FloatToStr(Period) + ', Epoch ' + FloatToStr(Epoch) + ']';
    ShowObservations(X1, Y1, Errors, Period, Epoch, DialogTitle);
  end;
end;

procedure TFormMain.ActionCopyChartImageExecute(Sender: TObject);
begin
  Chart.CopyToClipboard(TBitmap);
end;

procedure TFormMain.ActionSaveChartImageAsExecute(Sender: TObject);
begin
  if SavePictureDialog.InitialDir = '' then
    SavePictureDialog.InitialDir := OpenDialog.InitialDir;
  SavePictureDialog.FileName := '';
  if SavePictureDialog.Execute then begin
    try
      Chart.SaveToFile(TPortableNetworkGraphic, SavePictureDialog.FileName);
    except
      on E: Exception do begin
        ShowMessage(E.Message);
        Exit;
      end;
    end;
  end;
end;

procedure TFormMain.ActionChartPropertiesExecute(Sender: TObject);
begin
  if ChartProperties(Chart, FObjectName, FChartXtitle, FChartYtitle) then begin
    UpdateTitle;
    SaveDataSettings;
  end;
end;

procedure TFormMain.ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  if (AAction = ActionOpen) or
     (AAction = ActionExit) or
     (AAction = ActionOptions) or
     (AAction = ActionAbout) or
     (AAction = ActionUserManual) or
     (AAction = ActionUserManualLocal)
  then begin
    (AAction as TAction).Enabled := not FCalculationInProgress;
  end
  else
  if AAction = ActionInvertedY then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
    (AAction as TAction).Checked := Chart.AxisList[0].Inverted;
  end
  else
  if AAction = ActionCycleByCycleColor then begin
    (AAction as TAction).Enabled := (LCSrcFoldedData.Count > 0) and not FCalculationInProgress;
    (AAction as TAction).Checked := FChartColorMode = TChartColorMode.Phase;
  end
  else
  if AAction = ActionDayByDayColor then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
    (AAction as TAction).Checked := FChartColorMode = TChartColorMode.Day;
  end
  else
  if AAction = ActionYearByYearColor then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
    (AAction as TAction).Checked := FChartColorMode = TChartColorMode.Year;
  end
  else
  if AAction = ActionCustomHColor then begin
    // Selection is not defined in the phase plot mode (see also ActionSaveVisible)
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and (ChartSeriesData.Source = LCSrcData) and not FCalculationInProgress;
    (AAction as TAction).Checked := FChartColorMode = TChartColorMode.Custom;
  end
  else
  if AAction = ActionLogicalExtent then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionSaveChartImageAs then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionCopyChartImage then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionChartProperties then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionRawData then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionPhasePlot then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionPhasePlotSimple then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionMagShift then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionStatistics then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionPeriodogram then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionPolyFit then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionModelInfo then begin
    (AAction as TAction).Enabled := (Length(FModelData[FitColumnType.x]) > 0) and not FCalculationInProgress;
  end
  else
  if (AAction = ActionDetrend) or (AAction = ActionDetrendAlgebraic) then begin
    (AAction as TAction).Enabled := (Length(FFitAtPoints[FitColumnType.x]) > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionObservations then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
  end
  else
  if AAction = ActionSaveVisible then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and (ChartSeriesData.Source = LCSrcData) and not FCalculationInProgress;
  end
  else
  if AAction = ActionShowData then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
    (AAction as TAction).Checked := ChartSeriesData.Active;
  end
  else
  if AAction = ActionShowErrors then begin
    (AAction as TAction).Enabled := (LCSrcData.Count > 0) and not FCalculationInProgress;
    (AAction as TAction).Checked := ChartSeriesData.YErrorBars.Visible;
  end
  else
  if AAction = ActionShowModel then begin
    (AAction as TAction).Enabled := (UDFSrcModel.Count > 0) and not FCalculationInProgress;
    (AAction as TAction).Checked := ChartSeriesModel.Active;
  end
  else
  if AAction = ActionStop then begin
    (AAction as TAction).Enabled := FCalculationInProgress;
  end;
end;

function TFormMain.GetLCSrcDataCount: Integer;
begin
  Result := LCSrcData.Count;
end;

function TFormMain.GetLCSrcDataItem(N: Integer): PChartDataItem;
begin
  if (N < 0) or (N > LCSrcData.Count - 1) then
    Result := nil
  else
    Result := LCSrcData.Item[N];
end;

procedure TFormMain.UpdateTitle;
const
  defCaption = 'LCV';
begin
  if FFileName <> '' then begin
    Caption := ExtractFileName(FFileName) + ' - ' + defCaption;
  end
  else begin
    Caption := defCaption;
  end;

  if FObjectName <> '' then begin
    if FChartSubtitle = '' then
      Chart.Title.Text.Text := FObjectName + ^M^J + ' '
    else
      Chart.Title.Text.Text := FObjectName + ^M^J + FChartSubtitle;
  end
  else begin
    Chart.Title.Text.Text := defCaption;
  end;

  if FChartYtitle <> '' then
    Chart.AxisList[0].Title.Caption := FChartYtitle
  else
    Chart.AxisList[0].Title.Caption := ' ';
  Chart.AxisList[0].Title.Visible := True; //FChartYtitle <> '';

  if ChartSeriesData.Source = LCSrcFoldedData then begin
    Chart.AxisList[1].Title.Caption := 'Phase';
  end
  else
  if FChartXtitle <> '' then begin
    Chart.AxisList[1].Title.Caption := FChartXtitle;
  end
  else begin
    Chart.AxisList[1].Title.Caption := ' ';
  end;
  Chart.AxisList[1].Title.Visible := True; //FChartXtitle <> '';
end;

procedure TFormMain.UDFSrcModelGetChartDataItem(
  ASource: TUserDefinedChartSource;
  AIndex: Integer;
  var AItem: TChartDataItem);
begin
  if (AIndex >= 0) and (AIndex < Length(FModelData[FitColumnType.x])) and (AIndex < Length(FModelData[FitColumnType.yFit])) then begin
    if ASource = UDFSrcModel then begin
      AItem.X := FModelData[FitColumnType.x][AIndex];
      AItem.Y := FModelData[FitColumnType.yFit][AIndex];
    end
    else
    if ASource = UDFSrcModelUpLimit then begin
      if (AIndex < Length(FModelData[FitColumnType.yErrors])) then begin
        AItem.X := FModelData[FitColumnType.x][AIndex];
        AItem.Y := FModelData[FitColumnType.yFit][AIndex] + FModelData[FitColumnType.yErrors][AIndex];
      end;
    end
    else
    if ASource = UDFSrcModelDownLimit then begin
      if (AIndex < Length(FModelData[FitColumnType.yErrors])) then begin
        AItem.X := FModelData[FitColumnType.x][AIndex];
        AItem.Y := FModelData[FitColumnType.yFit][AIndex] - FModelData[FitColumnType.yErrors][AIndex];
      end;
    end;
  end;
end;

procedure TFormMain.UDFSrcModelFoldedGetChartDataItem(
  ASource: TUserDefinedChartSource;
  AIndex: Integer;
  var AItem: TChartDataItem);
begin
  if (AIndex >= 0) and (AIndex < Length(FModelFolded[FitColumnType.x])) and (AIndex < Length(FModelFolded[FitColumnType.yFit])) then begin
    if ASource = UDFSrcModelFolded then begin
      AItem.X := FModelFolded[FitColumnType.x][AIndex];
      AItem.Y := FModelFolded[FitColumnType.yFit][AIndex];
    end
    else
    if ASource = UDFSrcModelFoldedUpLimit then begin
      if (AIndex < Length(FModelFolded[FitColumnType.yErrors])) then begin
        AItem.X := FModelFolded[FitColumnType.x][AIndex];
        AItem.Y := FModelFolded[FitColumnType.yFit][AIndex] + FModelFolded[FitColumnType.yErrors][AIndex];
      end;
    end
    else
    if ASource = UDFSrcModelFoldedDownLimit then begin
      if (AIndex < Length(FModelFolded[FitColumnType.yErrors])) then begin
        AItem.X := FModelFolded[FitColumnType.x][AIndex];
        AItem.Y := FModelFolded[FitColumnType.yFit][AIndex] - FModelFolded[FitColumnType.yErrors][AIndex];
      end;
    end;
  end;
end;

procedure TFormMain.ChartSeriesModelToNil;
begin
  ChartSeriesModel.Source := nil;
  ChartSeriesModelUpLimit.Source := nil;
  ChartSeriesModelDownLimit.Source := nil;
end;

procedure TFormMain.ChartSeriesModelToModel;
begin
  ChartSeriesModel.Source := UDFSrcModel;
  ChartSeriesModelUpLimit.Source := UDFSrcModelUpLimit;
  ChartSeriesModelDownLimit.Source := UDFSrcModelDownLimit;
end;

procedure TFormMain.ChartSeriesModelToModelFolded;
begin
  ChartSeriesModel.Source := UDFSrcModelFolded;
  ChartSeriesModelUpLimit.Source := UDFSrcModelFoldedUpLimit;
  ChartSeriesModelDownLimit.Source := UDFSrcModelFoldedDownLimit;;
end;

procedure TFormMain.ClearUDFSrcModel;
begin
  UDFSrcModel.PointsNumber := 0;
  UDFSrcModel.Reset;
  UDFSrcModelUpLimit.PointsNumber := 0;
  UDFSrcModelUpLimit.Reset;
  UDFSrcModelDownLimit.PointsNumber := 0;
  UDFSrcModelDownLimit.Reset;
end;

procedure TFormMain.ClearUDFSrcModelFolded;
begin
  UDFSrcModelFolded.PointsNumber := 0;
  UDFSrcModelFolded.Reset;
  UDFSrcModelFoldedUpLimit.PointsNumber := 0;
  UDFSrcModelFoldedUpLimit.Reset;
  UDFSrcModelFoldedDownLimit.PointsNumber := 0;
  UDFSrcModelFoldedDownLimit.Reset;
end;

procedure TFormMain.ClearFitAtPoints;
var
  FitColumn: FitColumnType;
begin
  for FitColumn := Low(FitColumnType) to High(FitColumnType) do begin
    FFitAtPoints[FitColumn] := nil;
  end;
end;

procedure TFormMain.ClearModelData;
var
  FitColumn: FitColumnType;
begin
  for FitColumn := Low(FitColumnType) to High(FitColumnType) do begin
    FModelData[FitColumn] := nil;
  end;
end;

procedure TFormMain.ClearModelFolded;
var
  FitColumn: FitColumnType;
begin
  for FitColumn := Low(FitColumnType) to High(FitColumnType) do begin
    FModelFolded[FitColumn] := nil;
  end;
end;

procedure TFormMain.CloseFile;
var
  tempDArray: THarmonicDblArray = (NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN);
  tempIArray: THarmonicIntArray = (0, 0, 0, 0, 0, 0, 0, 0, 0);
begin
  FFileName := '';
  FObjectName := '';
  FChartSubtitle := '';
  FChartXtitle := '';
  FChartYtitle := '';
  UpdateTitle;
  FFitFormula := '';
  FFitInfo := '';
  FPeriodogramFirstRun := True;
  // Data
  ChartSeriesData.Source := nil;
  LCSrcData.Clear;
  LCSrcFoldedData.Clear;
  FFoldedDataColorRegions.Clear;
  FSimpleColorRegions.Clear;
  // Model (virtual sources)
  ChartSeriesModelToNil;
  ClearUDFSrcModel;
  ClearUDFSrcModelFolded;
  ClearModelData;
  ClearModelFolded;
  //
  ClearFitAtPoints;
  //
  ChartSeriesData.ColorEach := ceNone;
  HideColorLegend;
  FChartColorMode := TChartColorMode.None;
  UpdateColorActions;
  ChartSeriesData.Active := True;
  ActionList.UpdateAction(ActionShowData); // Ubuntu bug (check state not always updated)? (GNOME)
  ChartSeriesData.YErrorBars.Visible := False;
  ActionList.UpdateAction(ActionShowErrors); // Ubuntu bug (check state not always updated)? (GNOME)
  ChartSeriesModel.Active := True;
  ChartSeriesModelUpLimit.Active := ChartSeriesModel.Active;
  ChartSeriesModelDownLimit.Active := ChartSeriesModel.Active;
  ActionList.UpdateAction(ActionShowModel); // Ubuntu bug (check state not always updated)? (GNOME)
  //
  SetAxisBoundaries(-1.0, 1.0, -1.0, 1.0);
  //
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
  unitMagShiftDialog.SetCurrentMagShift(NaN);
  //CloseDFTdialogs;
end;

procedure TFormMain.OpenFile(const AFileName: string);
var
  X, Y, Errors: TDoubleArray;
  TempObjectName: string;
  I, ItemIndex: Integer;
  Item: PChartDataItem;
begin
  try
    // Data sorted by X after read
    ReadData(AFileName, X, Y, Errors, TempObjectName);
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

  CloseFile;

  FFileName := AFileName;
  FObjectName := TempObjectName;
  unitPhaseDialog.SetCurrentEpoch((MaxValue(X) + MinValue(X)) / 2.0);

  for I := 0 to Length(X) - 1 do begin
    ItemIndex := LCSrcData.Add(X[I], Y[I]); // Currently, ItemIndex = I
    Item := LCSrcData.Item[ItemIndex];
    Item^.YList[0] := Errors[I];
    Item^.Color := clTAColor;
    Item^.XList[XLIST_PHS_COLOR] := Item^.Color; // To be used as 'Phase Colors'
  end;

  LoadDataSettings;
  UpdateTitle;
  PlotData;
end;

procedure TFormMain.AsyncOpenFile(Data: PtrInt);
begin
  OpenFile(ExpandFileName(FFileNameParamStr));
end;

procedure TFormMain.Detrend(AlgebraicOnly: Boolean);
var
  X, Xprev: Double;
  Xarray, Yarray: TDoubleArray;
  I: Integer;
begin
  // Before closing the active file: store data to temporary arrays
  SetLength(Xarray, Length(FFitAtPoints[FitColumnType.x]));
  SetLength(Yarray, Length(FFitAtPoints[FitColumnType.x]));

  Xprev := 0;
  for I := 0 to Length(FFitAtPoints[FitColumnType.x]) - 1 do begin
    X := FFitAtPoints[FitColumnType.x][I];

    // Check -- just in case
    if (I > 0) and (X < Xprev) then
      CalcError('Internal error: model data must be sorted');

    Xarray[I] := X;
    if AlgebraicOnly then
      Yarray[I] := FFitAtPoints[FitColumnType.yObserved][I] - FFitAtPoints[FitColumnType.yFitAlgebraic][I]
    else
      Yarray[I] := FFitAtPoints[FitColumnType.yObserved][I] - FFitAtPoints[FitColumnType.yFit][I];

    Xprev := X;
  end;

  // Like in OpenFile

  CloseFile;

  FFileName := '#Residuals';
  FObjectName := 'Residuals';
  unitPhaseDialog.SetCurrentEpoch((MaxValue(Xarray) + MinValue(Xarray)) / 2.0);

  for I := 0 to Length(Xarray) - 1 do begin
    LCSrcData.Add(Xarray[I], Yarray[I]);
  end;

  LoadDataSettings; // Initialization only!
  UpdateTitle;
  PlotData;
end;

procedure TFormMain.SaveFileAs(const AFileName: string; const X, Y, Errors: TDoubleArray);
var
  PropsFileName: string;
begin
  try
    PropsFileName := AFileName + '.lcv.props';
    WriteData(AFileName, X, Y, Errors);
    if FileExists(PropsFileName) then begin
      if not SysUtils.DeleteFile(PropsFileName) then
        ShowMessage('Cannot delete ' + PropsFileName);
    end;
    SaveDataSettingsAs(AFileName);
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
  if FFileName = '' then
    Exit;
  try
    if (Copy(FFileName, 1, 1) <> '#') then
      Ini := TMemIniFile.Create(FFileName + '.lcv.props')
    else
      Ini := TMemIniFile.Create('');
    try
      unitFitParamDialog.LoadParameters(Ini, 'SETTINGS');
      unitDFTparamDialog.LoadParameters(Ini, 'SETTINGS');
      unitPhaseDialog.LoadParameters(Ini, 'SETTINGS');
      LoadChartSettings(Ini, 'SETTINGS');
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
begin
  SaveDataSettingsAs(FFileName);
end;

procedure TFormMain.SaveDataSettingsAs(AFileName: string);
var
  Ini: TMemIniFile;
begin
  if (AFileName = '') or (Copy(AFileName, 1, 1) = '#') then
    Exit;
  try
    Ini := TMemIniFile.Create(AFileName + '.lcv.props');
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
  Ini.WriteBool(Section, 'Yinverted', Chart.AxisList[0].Inverted);
  Ini.WriteInteger(Section, 'DataColor', ChartSeriesData.Pointer.Brush.Color);
  Ini.WriteInteger(Section, 'ModelColor', ChartSeriesModel.LinePen.Color);
  Ini.WriteInteger(Section, 'ModelUpLimitColor', ChartSeriesModelUpLimit.LinePen.Color);
  Ini.WriteInteger(Section, 'ModelDownLimitColor', ChartSeriesModelDownLimit.LinePen.Color);
  Ini.WriteString(Section, 'XTitle', FChartXtitle);
  Ini.WriteString(Section, 'YTitle', FChartYtitle);
  Ini.WriteString(Section, 'Object', FObjectName);
end;

procedure TFormMain.LoadChartSettings(const Ini: TIniFile; const Section: string);
begin
  Chart.AxisList[0].Inverted := Ini.ReadBool('SETTINGS', 'Yinverted', True);
  ActionList.UpdateAction(ActionInvertedY); // Ubuntu bug (check state not always updated)? (GNOME)
  ChartSeriesData.ColorEach := ceNone;
  HideColorLegend;
  FChartColorMode := TChartColorMode.None;
  UpdateColorActions;
  ChartSeriesData.Pointer.Brush.Color := TColor(Ini.ReadInteger('SETTINGS', 'DataColor', clPurple));
  ChartSeriesData.Pointer.Pen.Color := ChartSeriesData.Pointer.Brush.Color;
  ChartSeriesModel.LinePen.Color := TColor(Ini.ReadInteger('SETTINGS', 'ModelColor', clLime));
  ChartSeriesModelUpLimit.LinePen.Color := TColor(Ini.ReadInteger('SETTINGS', 'ModelUpLimitColor', clRed));
  ChartSeriesModelUpLimit.LinePen.Color := TColor(Ini.ReadInteger('SETTINGS', 'ModelDownLimitColor', clRed));
  FChartXtitle := Ini.ReadString(Section, 'XTitle', '');
  FChartYtitle := Ini.ReadString(Section, 'YTitle', '');
  FObjectName := Ini.ReadString(Section, 'Object', FObjectName);
end;

procedure TFormMain.PlotData;
var
  SourceExtent: TDoubleRect;
begin
  if LCSrcData.Count > 0 then begin
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
    FChartSubtitle := '';
    ChartSeriesData.Source := nil;
    ChartSeriesModelToNil;
    UpdateTitle;
    SourceExtent := LCSrcData.Extent;
    SetAxisBoundaries(SourceExtent.coords[1], SourceExtent.coords[3], SourceExtent.coords[2], SourceExtent.coords[4]);
    ChartSeriesData.Source := LCSrcData;
    if UDFSrcModel.Count > 0 then begin
      ChartSeriesModelToModel;
    end;
    UpdateTitle;
  end;
end;

procedure TFormMain.PlotFolded;
begin
  PhasePlot(@CalcAndPlotFoldedProc);
end;

procedure TFormMain.PlotFoldedSimple;
begin
  if LCSrcFoldedData.Count > 0 then begin
    if (UDFSrcModel.Count > 0) and (UDFSrcModelFolded.Count < 1) then
      CalculateModelPhasePlot;
    PlotFoldedProc;
  end
  else begin
    PlotFolded;
  end;
end;

procedure TFormMain.PlotFoldedProc;
var
  LogicalExtent: TDoubleRect;
  SourceExtent: TDoubleRect;
  InPhasePlotMode: Boolean;
begin
  InPhasePlotMode := ChartSeriesData.Source = LCSrcFoldedData;
  HideColorLegend;
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';
  FChartSubtitle := '';
  UpdateTitle;
  LogicalExtent := Chart.LogicalExtent;
  ChartSeriesData.Source := nil;
  ChartSeriesModelToNil;
  if not InPhasePlotMode then begin
    SourceExtent := LCSrcFoldedData.Extent;
    SetAxisBoundaries(-1.0, 1.0, SourceExtent.coords[2], SourceExtent.coords[4]);
  end;
  ChartSeriesData.Source := LCSrcFoldedData;
  if UDFSrcModelFolded.Count > 0 then begin
    ChartSeriesModelToModelFolded;
  end;
  if InPhasePlotMode then begin
    Chart.LogicalExtent := LogicalExtent;
  end;
  StatusBar.Panels[1].Text := ' P= ' + FloatToStr(unitPhaseDialog.GetCurrentPeriod) + ^I' E= ' + FloatToStr(unitPhaseDialog.GetCurrentEpoch) + ' ';
  FChartSubtitle := 'Period ' + FloatToStr(unitPhaseDialog.GetCurrentPeriod) + ', Epoch ' + FloatToStr(unitPhaseDialog.GetCurrentEpoch) + ' ';
  UpdateTitle;
  if not (FChartColorMode in [TChartColorMode.None, TChartColorMode.Custom]) then
    ShowColorLegend(FChartColorMode = TChartColorMode.Phase);
end;

procedure TFormMain.MagShift;
begin
  MagnitudeShifter(@MagShiftProc);
end;

procedure TFormMain.MagShiftProc;

  procedure UpdateChartSource(Src: TListChartSource; Shift: Double);
  var
    Item: PChartDataItem;
    I, L: Integer;
  begin
    L := Src.Count;
    if L > 0 then begin
      Src.BeginUpdate;
      try
        for I := 0 to L - 1 do begin
          Item := Src.Item[I];
          Item^.Text := '';
          Item^.Y := Item^.Y + Shift;
        end;
      finally
        Src.EndUpdate;
      end;
    end;
  end;

var
  Shift: Double;
begin
  Shift := unitMagShiftDialog.GetCurrentMagShift;
  UpdateChartSource(LCSrcData, Shift);
  UpdateChartSource(LCSrcFoldedData, Shift);
end;

procedure TFormMain.SetPhaseColor(Data: TListChartSource); overload;
var
  Item: PChartDataItem;
  I, L: Integer;
begin
  L := Data.Count;
  for I := 0 to L - 1 do begin
    Item := Data.Item[I];
    Item^.Color := TColor(Trunc(Item^.XList[XLIST_PHS_COLOR]));
  end;
end;

procedure TFormMain.SetPhaseColor; overload;
begin
  SetPhaseColor(LCSrcData);
  SetPhaseColor(LCSrcFoldedData);
end;

procedure TFormMain.SetSeasonalColor(Mode: Boolean);
var
  Item, PrevItem, OriginalItem: PChartDataItem;
  IntervalN, PrevIntervalN, ColorIndex, I, L: Integer;
  IntervalName, PrevIntervalName: string;
  XTime: TDateTime;
  Year, Month, Day: Word;
  X1: Double;
begin
  // We assume that X is Julian Day.

  FSimpleColorRegions.Clear;

  L := LCSrcData.Count;

  if L < 1 then
    Exit;

  if Mode then
     FSimpleColorRegions.FInfo := 'Year'
  else
     FSimpleColorRegions.FInfo := 'JD';

  ColorIndex := 0;
  PrevIntervalN := 0;
  PrevIntervalName := '';
  X1 := LCSrcData.Item[0]^.X;
  for I := 0 to L - 1 do begin
    Item := LCSrcData.Item[I];
    if Mode then begin
      // Years
      try
        XTime := JulianDateToDateTime(Item^.X);
        DecodeDate(XTime, Year, Month, Day); // Works for A.D. only! Otherwise, returns 0
        IntervalN := Year;
        IntervalName := IntToStr(IntervalN);
      except
        on E: EConvertError do begin
           IntervalN := -1;
           IntervalName := 'Cannot convert JD to DateTime';
        end;
      end;
    end
    else begin
      // Days
      IntervalN := Floor(Item^.X);
      IntervalName := IntToStr(IntervalN);
    end;
    if I > 0 then begin
      PrevItem := LCSrcData.Item[I - 1];
      if (PrevItem^.X > Item^.X) then
        CalcError('Internal error: data must be sorted');
      if IntervalN <> PrevIntervalN then begin
        FSimpleColorRegions.AddRegion(X1, PrevItem^.X, CycleByCycleColors[ColorIndex], PrevIntervalName);
        X1 := Item^.X;
        Inc(ColorIndex);
        if ColorIndex > Length(CycleByCycleColors) - 1 then
          ColorIndex := 0;
      end;
    end;
    Item^.Color := CycleByCycleColors[ColorIndex];
    PrevIntervalN := IntervalN;
    PrevIntervalName := IntervalName;
  end;
  FSimpleColorRegions.AddRegion(X1, LCSrcData.Item[L - 1]^.X, CycleByCycleColors[ColorIndex], PrevIntervalName);

  L := LCSrcFoldedData.Count;

  if L > 0 then begin
    for I := 0 to L - 1 do begin
      Item := LCSrcFoldedData.Item[I];
      OriginalItem := GetLCSrcDataItem(Trunc(Item^.XList[XLIST_FLD_PTIDX]));
      if OriginalItem = nil then
        CalcError('Folded/Original data mismatch');
      Item^.Color := OriginalItem^.Color;
    end;
  end;
end;

procedure TFormMain.SetCustomHColor;
var
  Item, OriginalItem: PChartDataItem;
  ItemX, ItemY: Double;
  I: Integer;
begin
  if (LCSrcData.Count > 0) and (ChartSeriesData.Source = LCSrcData) then begin

    FSimpleColorRegions.Clear;
    for I := 0 to LCSrcData.Count - 1 do begin
      Item := LCSrcData.Item[I];
      ItemX := Item^.X;
      ItemY := Item^.Y;
       if ValInRange(ItemX, Chart.CurrentExtent.a.X, Chart.CurrentExtent.b.X) and
         ValInRange(ItemY, Chart.CurrentExtent.a.Y, Chart.CurrentExtent.b.Y) then
      begin
        Item^.Color := CycleByCycleColors[0];
      end
      else begin
        Item^.Color := clTAColor;
      end;
    end;

    for I := 0 to LCSrcFoldedData.Count - 1 do begin
      Item := LCSrcFoldedData.Item[I];
      OriginalItem := GetLCSrcDataItem(Trunc(Item^.XList[XLIST_FLD_PTIDX]));
      if OriginalItem = nil then
        CalcError('Folded/Original data mismatch');
      Item^.Color := OriginalItem^.Color;
    end;

  end;
end;

function TFormMain.GetCustomHColorArray: TBooleanArray;
var
  Item: PChartDataItem;
  I: Integer;
begin
  SetLength(Result, LCSrcData.Count);
  for I := 0 to LCSrcData.Count - 1 do begin
    Item := LCSrcData.Item[I];
    Result[I] := Item^.Color <> clTAColor;
  end;
end;

procedure TFormMain.RestoreCustomHColor(const A: TBooleanArray);
var
  Item, OriginalItem: PChartDataItem;
  I: Integer;
begin
  if Length(A) <> LCSrcData.Count then
    Exit;
  for I := 0 to LCSrcData.Count - 1 do begin
    Item := LCSrcData.Item[I];
    if A[I] then
      Item^.Color := CycleByCycleColors[0]
    else
      Item^.Color := clTAColor;
  end;
  for I := 0 to LCSrcFoldedData.Count - 1 do begin
    Item := LCSrcFoldedData.Item[I];
    OriginalItem := GetLCSrcDataItem(Trunc(Item^.XList[XLIST_FLD_PTIDX]));
    if OriginalItem = nil then
      CalcError('Folded/Original data mismatch');
    Item^.Color := OriginalItem^.Color;
  end;
end;

procedure TFormMain.ShowColorLegend(CycleColors: Boolean);
begin
  if CycleColors then
     colorLegend.ShowColorLegend(FFoldedDataColorRegions,
     'Period ' + FloatToStr(unitPhaseDialog.GetCurrentPeriod) + ', Epoch ' + FloatToStr(unitPhaseDialog.GetCurrentEpoch))
  else
     colorLegend.ShowColorLegend(FSimpleColorRegions, 'Legend');
end;

procedure TFormMain.HideColorLegend;
begin
  colorLegend.HideColorLegend;
end;

procedure TFormMain.CalculateModelPhasePlot;
var
  X, Y, E, Period, Epoch, Phase: Double;
  FitColumn: FitColumnType;
  L, I: Integer;
begin
  ClearUDFSrcModelFolded;
  Period := unitPhaseDialog.GetCurrentPeriod;
  Epoch := unitPhaseDialog.GetCurrentEpoch;
  for FitColumn := Low(FitColumnType) to High(FitColumnType) do begin
    FModelFolded[FitColumn] := nil;
  end;
  if IsNaN(Period) or IsNaN(Epoch) then
    Exit;
  L := Length(FModelData[FitColumnType.x]);
  SetLength(FModelFolded[FitColumnType.x], L * 2);
  SetLength(FModelFolded[FitColumnType.yFit], L * 2);
  SetLength(FModelFolded[FitColumnType.yErrors], L * 2);
  for I := 0 to L - 1 do begin
    X := FModelData[FitColumnType.x][I];
    Y := FModelData[FitColumnType.yFit][I];
    E := FModelData[FitColumnType.yErrors][I];
    Phase := CalculatePhase(X, Period, Epoch);
    FModelFolded[FitColumnType.x][I] := Phase;
    FModelFolded[FitColumnType.yFit][I] := Y;
    FModelFolded[FitColumnType.yErrors][I] := E;

    FModelFolded[FitColumnType.x][L + I] := Phase - 1;
    FModelFolded[FitColumnType.yFit][L + I] := Y;
    FModelFolded[FitColumnType.yErrors][L + I] := E;
  end;
  SortModelPoints(FModelFolded);
  // Virtual sources
  UDFSrcModelFolded.PointsNumber := L * 2;
  UDFSrcModelFolded.Reset;
  UDFSrcModelFoldedUpLimit.PointsNumber := L * 2;
  UDFSrcModelFoldedUpLimit.Reset;
  UDFSrcModelFoldedDownLimit.PointsNumber := L * 2;
  UDFSrcModelFoldedDownLimit.Reset;
  //
end;

procedure TFormMain.CalcFoldedData(Period, Epoch: Double);
var
  Item, PrevItem, FoldedItem: PChartDataItem;
  Phase: Double;
  L, N, I, ItemIndex: Integer;
  Cycle, PrevCycle: Int64;
  X1: Double;
begin
  FFoldedDataColorRegions.Clear;

  L := LCSrcData.Count;

  if L < 1 then
    Exit;

  N := 0;
  PrevCycle := 0;
  X1 := LCSrcData.Item[0]^.X;
  for I := 0 to L - 1 do begin
    Item := LCSrcData.Item[I];
    Phase := CalculatePhase(Item^.X, Period, Epoch);
    // Cycle from Phase -0.5 to +0.5
    Cycle := CalculateCycle(Item^.X + Period / 2, Period, Epoch);
    if I > 0 then begin
      PrevItem := LCSrcData.Item[I - 1];
      if (PrevItem^.X > Item^.X) then
        CalcError('Internal error: data must be sorted');
      if Cycle <> PrevCycle then begin
        FFoldedDataColorRegions.AddRegion(X1, PrevItem^.X, CycleByCycleColors[N], PrevCycle, Epoch + PrevCycle * Period, Period);
        X1 := Item^.X;
        Inc(N);
        if N > Length(CycleByCycleColors) - 1 then
          N := 0;
      end;
    end;

    ItemIndex := LCSrcFoldedData.Add(Phase      , Item^.Y, '');
    FoldedItem := LCSrcFoldedData.Item[ItemIndex];
    FoldedItem^.YList[0] := Item^.YList[0];
    FoldedItem^.XList[XLIST_PHS_COLOR] := CycleByCycleColors[N]; // color
    FoldedItem^.XList[XLIST_FLD_PTIDX] := I; // index of this value in the original data

    ItemIndex := LCSrcFoldedData.Add(Phase - 1.0, Item^.Y, '');
    FoldedItem := LCSrcFoldedData.Item[ItemIndex];
    FoldedItem^.YList[0] := Item^.YList[0];
    FoldedItem^.XList[XLIST_PHS_COLOR] := CycleByCycleColors[N]; // color
    FoldedItem^.XList[XLIST_FLD_PTIDX] := I; // index of this value in the original data

    Item^.XList[XLIST_PHS_COLOR] := CycleByCycleColors[N]; // set color to original data too!

    PrevCycle := Cycle;
  end;
  FFoldedDataColorRegions.AddRegion(X1, LCSrcData.Item[L - 1]^.X, CycleByCycleColors[N], PrevCycle, Epoch + PrevCycle * Period, Period);
end;

procedure TFormMain.CalcAndPlotFoldedProc;
var
  Period, Epoch: Double;
  I: Integer;
  SavedCustomHColor: TBooleanArray;
begin
  SaveDataSettings;
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';
  FChartSubtitle := '';
  UpdateTitle;
  if (LCSrcData.Count > 0) then begin
    Period := unitPhaseDialog.GetCurrentPeriod;
    Epoch := unitPhaseDialog.GetCurrentEpoch;
    // Folded data will be recteated
    LCSrcFoldedData.Clear;
    FFoldedDataColorRegions.Clear;
    // Model (virtual sources)
    ClearUDFSrcModelFolded;
    // Reset data colors
    if FChartColorMode = TChartColorMode.Custom then
      SavedCustomHColor := GetCustomHColorArray;
    for I := 0 to LCSrcData.Count - 1 do begin
      LCSrcData.Item[I]^.Color := clTAColor;
      LCSrcData.Item[I]^.XList[XLIST_PHS_COLOR] := clTAColor;
    end;
    //
    CalcFoldedData(Period, Epoch);
    // Set point colors for both LCSrcData and LCSrcFoldedData
    if FChartColorMode = TChartColorMode.Phase then
      SetPhaseColor
    else
    if FChartColorMode = TChartColorMode.Day then
      SetSeasonalColor(False)
    else
    if FChartColorMode = TChartColorMode.Year then
      SetSeasonalColor(True)
    else
    if FChartColorMode = TChartColorMode.Custom then begin
      RestoreCustomHColor(SavedCustomHColor);
    end;
    CalculateModelPhasePlot;
    PlotFoldedProc;
  end;
end;

procedure TFormMain.SetAxisBoundaries(Xmin, Xmax, Ymin, Ymax: Double);
var
  MinV, MaxV: Double;
begin
  MinV := Xmin;
  MaxV := Xmax;
  //Extent := MaxV - MinV;
  //if ExpandX then begin
  //  MaxV := MaxV + Extent * 0.02;
  //  MinV := MinV - Extent * 0.02;
  //end;
  Chart.Extent.XMin := MinV;
  Chart.Extent.XMax := MaxV;
  //Chart.Extent.UseXMin := True;
  //Chart.Extent.UseXMax := True;
  //Chart.AxisList[1].Marks.Range.Min := MinV;
  //Chart.AxisList[1].Marks.Range.Max := MaxV;
  //Chart.AxisList[1].Marks.Range.UseMin := True;
  //Chart.AxisList[1].Marks.Range.UseMax := True;

  MinV := Ymin;
  MaxV := Ymax;
  //Extent := MaxV - MinV;
  //if ExpandY then begin
  //  MaxV := MaxV + Extent * 0.02;
  //  MinV := MinV - Extent * 0.02;
  //end;
  Chart.Extent.YMin := MinV;
  Chart.Extent.YMax := MaxV;
  //Chart.Extent.UseYMin := True;
  //Chart.Extent.UseYMax := True;
  //Chart.AxisList[0].Marks.Range.Min := MinV;
  //Chart.AxisList[0].Marks.Range.Max := MaxV;
  //Chart.AxisList[0].Marks.Range.UseMin := True;
  //Chart.AxisList[0].Marks.Range.UseMax := True;
end;

procedure TFormMain.DoPolyFit;
var
  TrendDegree: Integer;
  TrigPolyDegrees: THarmonicIntArray;
  Frequencies: THarmonicDblArray;
begin
  if LCSrcData.Count > 0 then begin
    if not GetFitParams(TrendDegree, TrigPolyDegrees, Frequencies, False) then
      Exit;
    SaveDataSettings;
    DoPolyFitProc(TrendDegree, TrigPolyDegrees, Frequencies);
  end;
end;

procedure TFormMain.DoPolyFitProc(TrendDegree: Integer; const TrigPolyDegrees: THarmonicIntArray; const Frequencies: THarmonicDblArray);

const
  DEF_MIN_MODEL_INTERVALS = 500;
  DEF_MAX_MODEL_INTERVALS = 100000;

  function FitStepFromFrequencies(Freq: THarmonicDblArray): Double;
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
      //Result := 1.0 / F * 0.01 * 0.5;
      Result := 1.0 / F * 0.01 * 0.1;
  end;

var
  X: TDoubleArray;
  Y: TDoubleArray;
  NofParameters: Integer;
  n_points: Integer;
  OCsquared: Double;
  meanTime: Double;
  fitXmin, fitXmax, fitXstep, t_scale: Double;
  tempFrequencies: THarmonicDblArray;
  nfit: Integer;
  Item: PChartDataItem;
  I: Integer;
  FPUExceptionMask: TFPUExceptionMask;
  WCursorIntf: IUnknown;

begin
  WCursorIntf := TWaitCursor.Create as IUnknown; // will be freed automatically

  FFitFormula := '';
  FFitInfo := '';

  // Model uses virtual sources
  ChartSeriesModelToNil;
  //
  ClearUDFSrcModel;
  ClearUDFSrcModelFolded;
  //
  ClearModelData;
  ClearModelFolded;
  //
  ClearFitAtPoints;
  //

  SetLength(X, LCSrcData.Count);
  SetLength(Y, LCSrcData.Count);
  for I := 0 to LCSrcData.Count - 1 do begin
    Item := LCSrcData.Item[I];
    X[I] := Item^.X;
    Y[I] := Item^.Y;
  end;

  try
    // Under Linux, all exceptions get masked (at least sometimes)
    // For compatibility, we explicitly set the mask
    FPUExceptionMask := GetExceptionMask;
    SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
    try
      meanTime := Mean(X);
      n_points := Length(X);

      SetLength(FFitAtPoints[FitColumnType.x], n_points);
      for I := 0 to n_points - 1 do begin
        FFitAtPoints[FitColumnType.x][I] := X[I];
      end;

      // Centering and scaling time
      for I := 0 to n_points - 1 do begin
        X[I] := X[I] - meanTime;
      end;
      t_scale := Max(Abs(MinValue(X)), Abs(MaxValue(X)));
      for I := 0 to n_points - 1 do begin
        X[I] := X[I] / t_scale;
      end;
      // We also need to scale the input frequencies
      for I := 0 to Length(Frequencies) - 1 do begin
        tempFrequencies[I] := Frequencies[I] * t_scale;
      end;

      fitXmin := MinValue(X);
      fitXmax := MaxValue(X);

      nfit := GetGlobalIntParameter('ModelPoints', 0) - 1;
      if nfit <> -1 then begin
        if ((nfit + 1) > unitOptionsDialog.MAX_MODEL_POINTS) or ((nfit + 1) < unitOptionsDialog.MIN_MODEL_POINTS) then begin
          ShowMessage('Invalid number of model points in settings. The default value will be used.');
          nfit := -1;
        end;
      end;

      if nfit = -1 then begin
        fitXstep := FitStepFromFrequencies(tempFrequencies);
        if IsNan(fitXstep) then
          fitXstep := (fitXmax - fitXmin) / (Length(X) * 100);

        nfit := Ceil((fitXmax - fitXmin) / fitXstep);
        nfit := Ceil(nfit / 10) * 10;
        if nfit > DEF_MAX_MODEL_INTERVALS then begin
          nfit := DEF_MAX_MODEL_INTERVALS;
        end
        else
        if nfit < DEF_MIN_MODEL_INTERVALS then begin
          nfit := DEF_MIN_MODEL_INTERVALS;
        end;
      end;

      fitXstep := (fitXmax - fitXmin) / nfit;

      PolyFit(X, Y, TrendDegree, TrigPolyDegrees, tempFrequencies,
              fitXmin, fitXmax, fitXstep,
              FModelData[FitColumnType.x],
              FModelData[FitColumnType.yFit],
              FModelData[FitColumnType.yErrors],
              FFitAtPoints[FitColumnType.yFit],
              FFitAtPoints[FitColumnType.yErrors],
              FFitAtPoints[FitColumnType.yFitAlgebraic],
              t_scale,
              FFitFormula, FFitInfo);
      for I := 0 to Length(FModelData[FitColumnType.x]) - 1 do begin
        FModelData[FitColumnType.x][I] := FModelData[FitColumnType.x][I] * t_scale + meanTime;
      end;
    finally
      SetExceptionMask(FPUExceptionMask);
    end;
  except
    on E: Exception do begin
      ShowMessage('Error: '^M^J + '[' + E.ClassName + '] ' + E.Message);
      Exit;
    end;
  end;
  SetLength(FFitAtPoints[FitColumnType.yObserved], n_points);
  for I := 0 to n_points - 1 do begin
    FFitAtPoints[FitColumnType.yObserved][I] := Y[I];
  end;

  NofParameters := 1 + TrendDegree;
    for I := 0 to Length(TrigPolyDegrees) - 1 do
      NofParameters := NofParameters + TrigPolyDegrees[I] * 2;

  OCsquared := CalcResidualSquared(FFitAtPoints[FitColumnType.yObserved], FFitAtPoints[FitColumnType.yFit]);

  FFitFormula := '# Python'^M^J^M^J +
                 'import numpy as np'^M^J +
                 'import math'^M^J +
                 'import matplotlib.pyplot as plt'^M^J^M^J +
                 'def f(t):' + ^M^J +
                 ' timeZeroPoint = ' + FloatToStrLocaleIndependent(meanTime) + ^M^J +
                 ' return \' + ^M^J +
                 FFitFormula + ^M^J^M^J +
                 't_min = ' + FloatToStrLocaleIndependent(meanTime + fitXmin * t_scale) + ^M^J +
                 't_max = ' + FloatToStrLocaleIndependent(meanTime + fitXmin * t_scale + nfit * fitXstep * t_scale) + ^M^J +
                 'n = ' + IntToStr(nfit + 1) + ^M^J +
                 't = np.linspace(t_min, t_max, n)' + ^M^J +
                 'v = np.array(list(map(f, t)))' + ^M^J^M^J +
                 'fig, ax = plt.subplots()' + ^M^J;
  if Chart.AxisList[0].Inverted then
    FFitFormula := FFitFormula + 'ax.invert_yaxis()' + ^M^J;
  FFitFormula := FFitFormula +
                 'ax.plot(t, v)' + ^M^J^M^J +
                 'plt.show()' + ^M^J;

  FFitInfo := FFitInfo + ^M^J;
  FFitInfo := FFitInfo + 'timeZeroPoint = ' + Trim(FloatToStrMod(meanTime)) + ^M^J^M^J;

  FFitInfo := FFitInfo + 'Number of data points = ' + IntToStr(n_points) + ^M^J;
  FFitInfo := FFitInfo + 'Number of parameters = ' + IntToStr(NofParameters) + ^M^J;
  FFitInfo := FFitInfo + 'Σ(O-C)² = ' + Trim(FloatToStrMod(OCsquared)) + ^M^J;
  FFitInfo := FFitInfo + 'σ = ' + Trim(FloatToStrMod(Sqrt(OCsquared / (n_points - NofParameters)))) + ^M^J;
  FFitInfo := FFitInfo + 'R.M.S. accuracy of the fit σ[x_c] = ' + Trim(FloatToStrMod(Power(OCsquared * Double(NofParameters) / Double(n_points) / Double(n_points - NofParameters), 0.5))) + ^M^J;

  UDFSrcModel.PointsNumber := Length(FModelData[FitColumnType.x]);
  UDFSrcModel.Reset;
  UDFSrcModelUpLimit.PointsNumber := UDFSrcModel.PointsNumber;
  UDFSrcModelUpLimit.Reset;
  UDFSrcModelDownLimit.PointsNumber := UDFSrcModel.PointsNumber;
  UDFSrcModelDownLimit.Reset;
  if ChartSeriesData.Source = LCSrcData then begin
    ChartSeriesModelToModel;
  end
  else
  if ChartSeriesData.Source = LCSrcFoldedData then begin
    CalculateModelPhasePlot;
    ChartSeriesModelToModelFolded;
  end
  else begin
    ChartSeriesModelToNil;
  end;

  ChartSeriesModel.Active := True;
  ChartSeriesModelUpLimit.Active := ChartSeriesModel.Active;
  ChartSeriesModelDownLimit.Active := ChartSeriesModel.Active;
  ActionList.UpdateAction(ActionShowModel); // Ubuntu bug (check state not always updated)? (GNOME)
 end;

procedure TFormMain.Periodogram;
var
  DCDFTparameters: TDCDFTparameters;
  NofThreads: Integer;
  Item: PChartDataItem;
  Interval, Freq: Double;
  I: Integer;
begin
  if LCSrcData.Count > 0 then begin
    SetLength(DCDFTparameters.X, LCSrcData.Count);
    SetLength(DCDFTparameters.Y, LCSrcData.Count);
    for I := 0 to LCSrcData.Count - 1 do begin
      Item := LCSrcData.Item[I];
      DCDFTparameters.X[I] := Item^.X;
      DCDFTparameters.Y[I] := Item^.Y;
    end;
    if FPeriodogramFirstRun then begin
      if not unitDftParamDialog.IsResolutionDefined then begin
        unitDftParamDialog.SetCurrentFrequencyResolution(
          GetRecommendedFrequencyResolution(MinValue(DCDFTparameters.X),
                                            MaxValue(DCDFTparameters.X),
                                            unitDftParamDialog.GetCurrentTrigPolyDegree));
        unitDftParamDialog.SetCurrentFrequencyMin(0.0);
        Interval := GetMedianInterval(DCDFTparameters.X);
        if not IsNan(Interval) and (Interval > 0) then begin
          Freq := 1.0/Interval/2.0; // Approximate Nyquist
          if Freq > 50.0 then
            Freq := 50.0;
          unitDftParamDialog.SetCurrentFrequencyMax(Freq);
        end;
      end;
      FPeriodogramFirstRun := False;
    end;
    if not GetDFTparams(DCDFTparameters.X,
                        DCDFTparameters.FrequencyMin, DCDFTparameters.FrequencyMax,
                        DCDFTparameters.FrequencyResolution,
                        DCDFTparameters.TrendDegree,
                        DCDFTparameters.TrigPolyDegree)
    then
      Exit;
    SaveDataSettings;
    //CloseDFTdialogs;
    NofThreads := GetGlobalIntParameter('Threads', GetLogicalCpuCount);
    PanelCalculatingMessage.Caption := 'Calculating (' + IntToStr(NofThreads) + ' threads)...';
    DCDFTparameters.Error := '';
    DCDFTparameters.StartTime := Now;
    LongOpStart;
    try
      DoDCDFT(DCDFTparameters, NofThreads);
    except
      on E: Exception do begin
        ShowMessage(E.Message);
        LongOpStop;
        Exit;
      end;
    end;
  end;
end;

procedure TFormMain.DCDFTThreadOnTerminate(Sender: TObject);
begin
  LongOpStop;
  Assert(Sender is TDFTThread);
  if TDFTThread(Sender).Params.Error <> '' then begin
    ShowMessage('Error:'^M^J + TDFTThread(Sender).Params.Error);
    Exit;
  end;
  ShowPeriodogram(TDFTThread(Sender).Params);
end;

procedure TFormMain.ShowPeriodogram(DCDFTparameters: TDCDFTparameters);
var
  DialogCaption: string;
begin
  DialogCaption := Format('Periodogram | Trend degree = %d; Trig. Polynomial Degree = %d | CalcTime = %fs | %s',
                          [DCDFTparameters.TrendDegree,
                           DCDFTparameters.TrigPolyDegree,
                           (Now - DCDFTparameters.StartTime) * 24 * 60 * 60,
                           ExtractFileName(FFileName)]);
  PlotDFTresult(DialogCaption, DCDFTparameters.frequencies, DCDFTparameters.power, @CalcAndPlotFoldedProc);
end;

procedure TFormMain.DFTGlobalTerminate;
begin
  unitDFT.SetGlobalTerminateAllThreads(True);
end;

procedure TFormMain.ProgressCaptionProc(const Msg: string);
begin
  StatusBar.Panels[2].Text := Msg;
end;

procedure TFormMain.LongOpStart;
var
  I: Integer;
begin
  FCalculationInProgress := True;
  Chart.Enabled := False;
  StatusBar.Panels[2].Text := 'Calculating...';
  for I := 0 to ActionList.ActionCount - 1 do begin
    ActionList.UpdateAction(ActionList.Actions[I]);
  end;
  PanelCalculatingMessage.Visible := True;
end;

procedure TFormMain.LongOpStop;
begin
  FCalculationInProgress := False;
  Chart.Enabled := True;
  StatusBar.Panels[2].Text := '';
  PanelCalculatingMessage.Visible := False;
  if Self.WindowState = wsMinimized then begin
    //Self.WindowState := wsNormal;
    Application.Restore;
  end;
end;

procedure TFormMain.DoDCDFT(DCDFTparameters: TDCDFTparameters; NofThreads: Integer);
var
  DCDFTThread: TDFTThread;
begin
  DCDFTThread := TDFTThread.Create(DCDFTparameters, NofThreads, @DCDFTThreadOnTerminate, @ProgressCaptionProc);
  if Assigned(DCDFTThread.FatalException) then
    raise DCDFTThread.FatalException;
  DCDFTThread.Start;
end;


end.


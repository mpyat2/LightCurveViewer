unit unitMain;

{$mode objfpc}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, IniFiles, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, ExtDlgs, StdCtrls, ExtCtrls, TAGraph, TASources, TASeries,
  TACustomSource, TATools, TAChartUtils, TACustomSeries, Types, lcvconsts,
  lcvtypes, unitDFT;

type

  { TFormMain }

  TFormMain = class(TForm)
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
    MenuItemPhasePlotNew: TMenuItem;
    MenuItemModelInfo: TMenuItem;
    MenuItemDayByDayColor: TMenuItem;
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
    procedure ActionCycleByCycleColorExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionInvertedYExecute(Sender: TObject);
    procedure ActionLogicalExtentExecute(Sender: TObject);
    procedure ActionModelInfoExecute(Sender: TObject);
    procedure ActionObservationsExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionPeriodogramExecute(Sender: TObject);
    procedure ActionPhasePlotExecute(Sender: TObject);
    procedure ActionPhasePlotSimpleExecute(Sender: TObject);
    procedure ActionPolyFitExecute(Sender: TObject);
    procedure ActionRawDataExecute(Sender: TObject);
    procedure ActionSaveChartImageAsExecute(Sender: TObject);
    procedure ActionSaveVisibleExecute(Sender: TObject);
    procedure ActionShowDataExecute(Sender: TObject);
    procedure ActionShowModelExecute(Sender: TObject);
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
    procedure SaveFileAs(const AFileName: string; const X, Y: TDoubleArray);
    procedure SaveDataSettings;
    procedure LoadDataSettings;
    procedure SaveChartSettings(const Ini: TIniFile; const Section: string);
    procedure LoadChartSettings(const Ini: TIniFile; const Section: string);
    procedure PlotData;
    procedure PlotFolded;
    procedure PlotFoldedSimple;
    procedure PlotFoldedProc;
    procedure CalculateModelPhasePlot;
    procedure CalcFoldedData(Period, Epoch: Double; out AX, AY: TDoubleArray; out Colors: TColorArray);
    procedure CalcAndPlotFoldedProc;
    procedure SetAxisBoundaries(Xmin, Xmax, Ymin, Ymax: Double);
    procedure Periodogram;
    procedure DoPolyFit;
    procedure DoDCDFT(DCDFTparameters: TDCDFTparameters);
    procedure ShowPeriodogram(DCDFTparameters: TDCDFTparameters);
    procedure DCDFTThreadOnTerminate(Sender: TObject);
    procedure DFTGlobalTerminate;
    procedure ProgressCaptionProc(const Msg: string);
    procedure LongOpStart;
    procedure LongOpStop;
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  lclintf, math, guiutils, unitPhaseDialog,
  unitFitParamDialog, unitDFTparamDialog, unitDFTdialog, unitTableDialog,
  unitModelInfoDialog, floattextform, unitFormChartprops, unitGetExtent,
  unitAbout, dftThread, dataio, sortutils, formatutils, miscutils, fitproc,
  settings;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ToolBar.Images := ImageList;
  CloseFile;
  try
    LoadFormPosition(Self);
  except
    // nothing
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
    SaveFormPosition(Self);
  except
    // nothing;
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

procedure TFormMain.ChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Pg: TDoublePoint;
begin
  try
    if (ChartSeriesData.Source <> nil) and (ChartSeriesData.Source.Count > 0) then begin
      P := Chart.ScreenToClient(Mouse.CursorPos);
      Pg := Chart.ImageToGraph(P);
      StatusBar.Panels[0].Text := Format(' %g'^I' %g ', [Pg.X, Pg.Y]);
    end
    else
      StatusBar.Panels[0].Text := '';
  except
    on E: Exception do
      StatusBar.Panels[0].Text := '';
  end;
end;

procedure TFormMain.ChartToolsetDataPointClickTool1PointClick(ATool: TChartTool; APoint: TPoint);
var
  Tool: TDatapointClickTool;
  Series: TLineSeries;
  S, S1: String;
  X, Y: Double;
begin
  Tool := ATool as TDatapointClickTool;
  if Tool.Series = ChartSeriesData then begin
    Series := Tool.Series as TLineSeries;
    X := Series.GetXValue(Tool.PointIndex);
    Y := Series.GetYValue(Tool.PointIndex);
    S1 := Format('x = %g'#13#10'y = %g', [X, Y]);
    S := Series.ListSource.Item[Tool.PointIndex]^.Text;
    if S = S1 then
      Series.ListSource.SetText(Tool.PointIndex, '')
    else
      Series.ListSource.SetText(Tool.PointIndex, S1);
  end;
end;

procedure TFormMain.ChartToolsetDataPointClickTool2PointClick(ATool: TChartTool; APoint: TPoint);
var
  Tool: TDatapointClickTool;
  Series: TLineSeries;
  S1: String;
  X, Y: Double;
begin
  Tool := ATool as TDatapointClickTool;
  if Tool.Series = ChartSeriesData then begin
    Series := Tool.Series as TLineSeries;
    X := Series.GetXValue(Tool.PointIndex);
    Y := Series.GetYValue(Tool.PointIndex);
    S1 := Format('x = %g'#13#10'y = %g', [X, Y]);
    Series.ListSource.SetText(Tool.PointIndex, S1);
    floattextform.AddText('x = '^I + FloatToStr(X) + ^I + 'y = '^I + FloatToStr(Y));
    //S := Series.ListSource.Item[Tool.PointIndex]^.Text;
    //if S = S1 then
    //  Series.ListSource.SetText(Tool.PointIndex, '')
    //else
    //  Series.ListSource.SetText(Tool.PointIndex, S1);
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

procedure TFormMain.ActionPolyFitExecute(Sender: TObject);
begin
  DoPolyFit;
end;

procedure TFormMain.ActionOpenExecute(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFileDir(FFileName);
  OpenDialog.FileName := ExtractFileName(FFileName);
  if OpenDialog.Execute then begin
    OpenFile(OpenDialog.FileName);
  end;
end;

procedure TFormMain.ActionSaveVisibleExecute(Sender: TObject);

  function ValInRange(V: Double; Range1, Range2: Double): Boolean;
  begin
    Result := (V >= Min(Range1, Range2)) and (V <= Max(Range1, Range2));
  end;

var
  X1, Y1: TDoubleArray;
  ItemX, ItemY: Double;
  Item: PChartDataItem;
  N, I: Integer;
begin
  if (LCSrcData.Count > 0) and (ChartSeriesData.Source = LCSrcData) then begin
    SetLength(X1, LCSrcData.Count);
    SetLength(Y1, LCSrcData.Count);
    N := 0;
    for I := 0 to LCSrcData.Count - 1 do begin
      Item := LCSrcData.Item[I];
      ItemX := Item^.X;
      ItemY := Item^.Y;
      if ValInRange(ItemX, Chart.CurrentExtent.a.X, Chart.CurrentExtent.b.X) and
         ValInRange(ItemY, Chart.CurrentExtent.a.Y, Chart.CurrentExtent.b.Y) then
      begin
        X1[N] := ItemX;
        Y1[N] := ItemY;
        Inc(N);
      end;
    end;
    SetLength(X1, N);
    SetLength(Y1, N);
    if SaveDialog.InitialDir = '' then
      SaveDialog.InitialDir := OpenDialog.InitialDir;
    SaveDialog.FileName := '';
    if SaveDialog.Execute then begin
      SaveFileAs(SaveDialog.FileName, X1, Y1);
    end;
  end;
end;

procedure TFormMain.ActionShowDataExecute(Sender: TObject);
begin
  ChartSeriesData.Active := not ChartSeriesData.Active;
end;

procedure TFormMain.ActionShowModelExecute(Sender: TObject);
begin
  ChartSeriesModel.Active := not ChartSeriesModel.Active;
  ChartSeriesModelUpLimit.Active := ChartSeriesModel.Active;
  ChartSeriesModelDownLimit.Active := ChartSeriesModel.Active;
end;

procedure TFormMain.ActionStopExecute(Sender: TObject);
begin
  DFTGlobalTerminate;
end;

procedure TFormMain.ActionInvertedYExecute(Sender: TObject);
begin
  Chart.AxisList[0].Inverted := not Chart.AxisList[0].Inverted;
  SaveDataSettings;
end;

procedure TFormMain.ActionCycleByCycleColorExecute(Sender: TObject);
begin
  if ChartSeriesData.ColorEach = ceNone then
    ChartSeriesData.ColorEach := cePoint
  else
    ChartSeriesData.ColorEach := ceNone;
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
  if not OpenURL(defRemoteManual) then
    ShowMessage('Cannot open ' + defRemoteManual);
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

procedure TFormMain.ActionObservationsExecute(Sender: TObject);
var
  X1, Y1: TDoubleArray;
  Item: PChartDataItem;
  I: Integer;
begin
  if LCSrcData.Count > 0 then begin
    SetLength(X1, LCSrcData.Count);
    SetLength(Y1, LCSrcData.Count);
    for I := 0 to LCSrcData.Count - 1 do begin
      Item := LCSrcData.Item[I];
      X1[I] := Item^.X;
      Y1[I] := Item^.Y;
    end;
    ShowTable(X1, Y1, 'X', 'Y');
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
    (AAction as TAction).Enabled := (ChartSeriesData.Source = LCSrcFoldedData) and not FCalculationInProgress;
    (AAction as TAction).Checked := ChartSeriesData.ColorEach <> ceNone;
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
  if AAction = ActionShowModel then begin
    (AAction as TAction).Enabled := (UDFSrcModel.Count > 0) and not FCalculationInProgress;
    (AAction as TAction).Checked := ChartSeriesModel.Active;
  end
  else
  if AAction = ActionStop then begin
    (AAction as TAction).Enabled := FCalculationInProgress;
  end;
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

  Chart.AxisList[0].Title.Caption := FChartYtitle;
  Chart.AxisList[0].Title.Visible := FChartYtitle <> '';

  if FChartXtitle <> '' then begin
    if ChartSeriesData.Source = LCSrcFoldedData then begin
      Chart.AxisList[1].Title.Caption := 'Phase (' + FChartXtitle + ')';
    end
    else begin
      Chart.AxisList[1].Title.Caption := FChartXtitle;
    end;
  end
  else begin
    Chart.AxisList[1].Title.Caption := '';
  end;
  Chart.AxisList[1].Title.Visible := FChartXtitle <> '';
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
  tempDArray: TDouble5Array = (NaN, NaN, NaN, NaN, NaN);
  tempIArray: TInt5Array = (0, 0, 0, 0, 0);
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
  ChartSeriesData.Active := True;
  ChartSeriesModel.Active := True;
  ChartSeriesModelUpLimit.Active := ChartSeriesModel.Active;
  ChartSeriesModelDownLimit.Active := ChartSeriesModel.Active;
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
  //CloseDFTdialogs;
end;

procedure TFormMain.OpenFile(const AFileName: string);
var
  X, Y: TDoubleArray;
  //Xprev: Double;
  TempObjectName: string;
  I: Integer;
  //N: Integer;
begin
  CloseFile;
  try
    ReadData(AFileName, X, Y, TempObjectName);
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
  FObjectName := TempObjectName;
  unitPhaseDialog.SetCurrentEpoch((MaxValue(X) + MinValue(X)) / 2.0);

  //// Assume X is a Julian Day: use the color scheme
  //N := 0;
  //Xprev := NaN;
  //for I := 0 to Length(X) - 1 do begin
  //  if (I > 0) and (Int(X[I]) <> Int(Xprev)) then begin
  //    Inc(N);
  //    if N > Length(CycleByCycleColors) - 1 then
  //      N := 0;
  //  end;
  //  LCSrcData.Add(X[I], Y[I], '', CycleByCycleColors[N]);
  //  Xprev := X[I];
  //end;

  for I := 0 to Length(X) - 1 do begin
    LCSrcData.Add(X[I], Y[I]);
  end;

  LoadDataSettings;
  UpdateTitle;
  PlotData;
end;

procedure TFormMain.SaveFileAs(const AFileName: string; const X, Y: TDoubleArray);
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
      Ini.UpdateFile;
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
  ChartSeriesData.ColorEach := ceNone;
  ChartSeriesData.Pointer.Brush.Color := TColor(Ini.ReadInteger('SETTINGS', 'DataColor', clPurple));
  //ChartSeriesData.Pointer.Pen.Color := ChartSeriesData.Pointer.Brush.Color;
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
    ChartSeriesData.ColorEach := ceNone;
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
  SourceExtent: TDoubleRect;
begin
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';
  FChartSubtitle := '';
  UpdateTitle;
  ChartSeriesData.Source := nil;
  ChartSeriesModelToNil;
  SourceExtent := LCSrcFoldedData.Extent;
  SetAxisBoundaries(-1.0, 1.0, SourceExtent.coords[2], SourceExtent.coords[4]);
  ChartSeriesData.Source := LCSrcFoldedData;
  if UDFSrcModelFolded.Count > 0 then begin
    ChartSeriesModelToModelFolded;
  end;
  StatusBar.Panels[1].Text := ' P= ' + FloatToStr(unitPhaseDialog.GetCurrentPeriod) + ^I' E= ' + FloatToStr(unitPhaseDialog.GetCurrentEpoch) + ' ';
  FChartSubtitle := 'Period ' + FloatToStr(unitPhaseDialog.GetCurrentPeriod) + ', Epoch ' + FloatToStr(unitPhaseDialog.GetCurrentEpoch) + ' ';
  UpdateTitle;
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

procedure TFormMain.CalcFoldedData(Period, Epoch: Double; out AX, AY: TDoubleArray; out Colors: TColorArray);
var
  Xdata, Ydata: TDoubleArray;
  Item: PChartDataItem;
  Phase: Double;
  L, N, I: Integer;
  Cycle, PrevCycle: Int64;
begin
  L := LCSrcData.Count;
  SetLength(Xdata, L);
  SetLength(Ydata, L);
  for I := 0 to L - 1 do begin
    Item := LCSrcData.Item[I];
    Xdata[I] := Item^.X;
    Ydata[I] := Item^.Y;
  end;
  SortDataPoints(Xdata, Ydata); // ensure the data is sorted

  SetLength(AX, L);
  SetLength(AY, L);
  SetLength(Colors, L);

  N := 0;
  PrevCycle := 0;
  for I := 0 to L - 1 do begin
    Phase := CalculatePhase(Xdata[I], Period, Epoch);
    AX[I] := Phase;
    AY[I] := Ydata[I];
    Cycle := CalculateCycle(Xdata[I], Period, Epoch);
    if (I > 0) and (Cycle <> PrevCycle) then begin
      Inc(N);
      if N > Length(CycleByCycleColors) - 1 then
        N := 0;
    end;
    Colors[I] := CycleByCycleColors[N];
    PrevCycle := Cycle;
  end;
end;

procedure TFormMain.CalcAndPlotFoldedProc;
var
  Xpoints, Ypoints: TDoubleArray;
  Colors: TColorArray;
  Period, Epoch: Double;
  I: Integer;
begin
  SaveDataSettings;
  StatusBar.Panels[0].Text := '';
  StatusBar.Panels[1].Text := '';
  FChartSubtitle := '';
  UpdateTitle;
  if (LCSrcData.Count > 0) then begin
    Period := unitPhaseDialog.GetCurrentPeriod;
    Epoch := unitPhaseDialog.GetCurrentEpoch;
    LCSrcFoldedData.Clear;
    // Model (virtual sources)
    ClearUDFSrcModelFolded;
    //
    CalcFoldedData(Period, Epoch, Xpoints, Ypoints, Colors);
    for I := 0 to Length(Xpoints) - 1 do begin
      LCSrcFoldedData.Add(Xpoints[I], Ypoints[I], '', Colors[I]);
      LCSrcFoldedData.Add(Xpoints[I] - 1.0, Ypoints[I], '', Colors[I]);
    end;
    //LCSrcFoldedData.Sort;
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

  function FitStepFromFrequencies(Freq: TDouble5Array): Double;
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
      Result := 1.0 / F * 0.01 * 0.5;
  end;

var
  X: TDoubleArray;
  Y: TDoubleArray;
  TrendDegree: Integer;
  TrigPolyDegrees: TInt5Array;
  Frequencies: TDouble5Array;
  NofParameters: Integer;
  n_points: Integer;
  OCsquared: Double;
  meanTime: Double;
  fitXmin, fitXmax, fitXstep: Double;
  nfit: Integer;
  Item: PChartDataItem;
  I: Integer;
  FPUExceptionMask: TFPUExceptionMask;
  Intf: IUnknown;
begin
  if LCSrcData.Count > 0 then begin
    if not GetFitParams(TrendDegree, TrigPolyDegrees, Frequencies) then
      Exit;
    SaveDataSettings;

    Intf := TWaitCursor.Create as IUnknown; // will be freed automatically

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
        for I := 0 to n_points - 1 do begin
          X[I] := X[I] - meanTime;
        end;

        fitXmin := MinValue(X);
        fitXmax := MaxValue(X);
        fitXstep := FitStepFromFrequencies(Frequencies);
        if IsNan(fitXstep) then
          fitXstep := (fitXmax - fitXmin) / (Length(X) * 3);
        nfit := Ceil((fitXmax - fitXmin) / fitXstep);
        nfit := Ceil(nfit / 10) * 10;
        if nfit > 100000 then begin
          nfit := 100000;
          fitXstep := (fitXmax - fitXmin) / nfit;
        end;
        PolyFit(X, Y, TrendDegree, TrigPolyDegrees, Frequencies,
                fitXmin, fitXmax, fitXstep,
                FModelData[FitColumnType.x],
                FModelData[FitColumnType.yFit],
                FModelData[FitColumnType.yErrors],
                FFitAtPoints[FitColumnType.yFit],
                FFitAtPoints[FitColumnType.yErrors],
                FFitFormula, FFitInfo);
        for I := 0 to Length(FModelData[FitColumnType.x]) - 1 do begin
          FModelData[FitColumnType.x][I] := FModelData[FitColumnType.x][I] + meanTime;
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
    SetLength(FFitAtPoints[FitColumnType.x], n_points);
    for I := 0 to n_points - 1 do begin
      FFitAtPoints[FitColumnType.x][I] := X[I] + meanTime;
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
    FFitInfo := FFitInfo + 'Sum((O-C)^2) = ' + Trim(FloatToStrMod(OCsquared)) + ^M^J^M^J;
    FFitInfo := FFitInfo + 'Number of data points = ' + IntToStr(n_points) + ^M^J;
    FFitInfo := FFitInfo + 'Number of parameters = ' + IntToStr(NofParameters) + ^M^J;
    FFitInfo := FFitInfo + 'sigma[x_c] = ' + Trim(FloatToStrMod(Power(OCsquared * Double(NofParameters) / Double(n_points) / Double(n_points - NofParameters), 0.5))) + ^M^J;


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
  end;
end;

procedure TFormMain.Periodogram;
var
  DCDFTparameters: TDCDFTparameters;
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
    DCDFTparameters.Error := '';
    DCDFTparameters.StartTime := Now;
    LongOpStart;
    try
      DoDCDFT(DCDFTparameters);
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
  PlotDFTresult(DialogCaption, DCDFTparameters.frequencies, DCDFTparameters.power);
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

procedure TFormMain.DoDCDFT(DCDFTparameters: TDCDFTparameters);
var
  DCDFTThread: TDFTThread;
begin
  DCDFTThread := TDFTThread.Create(DCDFTparameters, @DCDFTThreadOnTerminate, @ProgressCaptionProc);
  if Assigned(DCDFTThread.FatalException) then
    raise DCDFTThread.FatalException;
  DCDFTThread.Start;
end;


end.


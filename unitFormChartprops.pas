unit unitFormChartprops;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  TAGraph, TASeries, TAChartAxis;

type

  { TFormChartProperties }

  TFormChartProperties = class(TForm)
    ButtonDataColor: TButton;
    ButtonModelColor: TButton;
    ButtonModelUpDownColor: TButton;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ColorDialog: TColorDialog;
    EditXtitle: TEdit;
    EditObjectName: TEdit;
    EditYtitle: TEdit;
    LabelXtitle: TLabel;
    LabelDataColor: TLabel;
    LabelModelColor: TLabel;
    LabelModelUpDownColor: TLabel;
    LabelObjectName: TLabel;
    LabelYtitle: TLabel;
    PanelDataColor: TPanel;
    PanelModelColor: TPanel;
    PanelModelUpDownColor: TPanel;
    PanelOptions: TPanel;
    PanelButtons: TPanel;
    procedure ButtonDataColorClick(Sender: TObject);
    procedure ButtonModelColorClick(Sender: TObject);
    procedure ButtonModelUpDownColorClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FChart: TChart;
    FChartSeriesData: TLineSeries;
    FChartSeriesModel: TLineSeries;
    FChartSeriesModelUpLimit: TLineSeries;
    FChartSeriesModelDownLimit: TLineSeries;
    FXAxis: TChartAxis;
    FYAxis: TChartAxis;
    FObjectName: string;
    procedure InitProps;
  public

  end;

function ChartProperties(Chart: TChart; var ObjectName: string): Boolean;

implementation

{$R *.lfm}

uses
  guiutils;

function ChartProperties(Chart: TChart; var ObjectName: string): Boolean;
var
  F: TFormChartProperties;
begin
  F := TFormChartProperties.Create(Application);
  try
    F.FChart := Chart;
    F.FObjectName := ObjectName;
    F.InitProps;
    Result := F.ShowModal = mrOk;
    if Result then
      ObjectName := F.FObjectName;
  finally
    FreeAndNil(F);
  end;
end;

{ TFormChartProperties }

procedure TFormChartProperties.FormCreate(Sender: TObject);
begin
  RearrangeButtons(ButtonOK, ButtonCancel);
end;

procedure TFormChartProperties.InitProps;
var
  I: Integer;
begin
  FChartSeriesData := nil;
  FChartSeriesModel := nil;
  FChartSeriesModelUpLimit := nil;
  FChartSeriesModelDownLimit := nil;
  for I := 0 to FChart.Series.Count - 1 do begin
    if FChart.Series[I].Name = 'ChartSeriesData' then
      FChartSeriesData := FChart.Series[I] as TLineSeries
    else
    if FChart.Series[I].Name = 'ChartSeriesModel' then
      FChartSeriesModel := FChart.Series[I] as TLineSeries
    else
    if FChart.Series[I].Name = 'ChartSeriesModelUpLimit' then
      FChartSeriesModelUpLimit := FChart.Series[I] as TLineSeries
    else
    if FChart.Series[I].Name = 'ChartSeriesModelDownLimit' then
      FChartSeriesModelDownLimit := FChart.Series[I] as TLineSeries;
  end;
  ButtonDataColor.Enabled := FChartSeriesData <> nil;
  ButtonModelColor.Enabled := FChartSeriesModel <> nil;
  ButtonModelUpDownColor.Enabled := (FChartSeriesModelUpLimit <> nil) and (FChartSeriesModelDownLimit <> nil);
  if FChartSeriesData <> nil then begin
    PanelDataColor.Color := FChartSeriesData.Pointer.Brush.Color;
  end;
  if FChartSeriesModel <> nil then begin
    PanelModelColor.Color := FChartSeriesModel.LinePen.Color;
  end;
  if (FChartSeriesModelUpLimit <> nil) and (FChartSeriesModelDownLimit <> nil) then begin
    PanelModelUpDownColor.Color := FChartSeriesModelUpLimit.LinePen.Color;
  end;
  FXAxis := FChart.AxisList[1];
  FYAxis := FChart.AxisList[0];
  EditXTitle.Text := FXAxis.Title.Caption;
  EditYTitle.Text := FYAxis.Title.Caption;
  EditObjectName.Text := FObjectName;
end;

procedure TFormChartProperties.ButtonDataColorClick(Sender: TObject);
begin
  ColorDialog.Color := PanelDataColor.Color;
  if ColorDialog.Execute then begin
    PanelDataColor.Color := ColorDialog.Color;
  end;
end;

procedure TFormChartProperties.ButtonModelColorClick(Sender: TObject);
begin
  ColorDialog.Color := PanelModelColor.Color;
  if ColorDialog.Execute then begin
    PanelModelColor.Color := ColorDialog.Color;
  end;
end;

procedure TFormChartProperties.ButtonModelUpDownColorClick(Sender: TObject);
begin
  ColorDialog.Color := PanelModelUpDownColor.Color;
  if ColorDialog.Execute then begin
    PanelModelUpDownColor.Color := ColorDialog.Color;
  end;
end;

procedure TFormChartProperties.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrNone;
  FChartSeriesData.Pointer.Brush.Color := PanelDataColor.Color;
  FChartSeriesData.Pointer.Pen.Color := FChartSeriesData.Pointer.Brush.Color;
  FChartSeriesModel.LinePen.Color := PanelModelColor.Color;
  FChartSeriesModelUpLimit.LinePen.Color := PanelModelUpDownColor.Color;
  FChartSeriesModelDownLimit.LinePen.Color := FChartSeriesModelUpLimit.LinePen.Color;
  FXAxis.Title.Caption := EditXTitle.Text;
  FYAxis.Title.Caption := EditYTitle.Text;
  FXAxis.Title.Visible := FXAxis.Title.Caption <> '';
  FYAxis.Title.Visible := FYAxis.Title.Caption <> '';
  FObjectName := EditObjectName.Text;
  ModalResult := mrOk;
end;

end.


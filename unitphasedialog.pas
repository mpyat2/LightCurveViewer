unit unitphasedialog;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, math;

type
  TApplyPhasePlotParams = procedure of object;

type

  { TFormPhaseDialog }

  TFormPhaseDialog = class(TForm)
    ButtonApply: TButton;
    ButtonOK: TButton;
    ButtonCamcel: TButton;
    EditPeriod: TEdit;
    EditEpoch: TEdit;
    LabelPeriod: TLabel;
    LabelEpoch: TLabel;
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetEpoch: Double;
    function GetPeriod: Double;
    procedure SetEpoch(AValue: Double);
    procedure SetPeriod(AValue: Double);
  private
    FApplyPhasePlotParams: TApplyPhasePlotParams;
    FParamOk: Boolean;
    FCurrentEpoch: Double;
    FCurrentPeriod: Double;
  public
    property CurrentEpoch: Double read FCurrentEpoch write FCurrentEpoch;
    property CurrentPeriod: Double read FCurrentPeriod write FCurrentPeriod;
  end;

var
  FormPhaseDialog: TFormPhaseDialog;

function CalculatePhase(T, Period, Epoch: Double): Double; inline;

procedure PhasePlot(ApplyPhasePlotParamsProc: TApplyPhasePlotParams);

implementation

{$R *.lfm}

function CalculatePhase(T, Period, Epoch: Double): Double; inline;
var
  N: Int64;
begin
  T := T - Epoch;
  N := Floor64(T / Period);
  Result := (T - Period * N) / Period;
end;

procedure PhasePlot(ApplyPhasePlotParamsProc: TApplyPhasePlotParams);
begin
  FormPhaseDialog.FApplyPhasePlotParams := ApplyPhasePlotParamsProc;
  FormPhaseDialog.SetEpoch(FormPhaseDialog.CurrentEpoch);
  FormPhaseDialog.SetPeriod(FormPhaseDialog.CurrentPeriod);
  FormPhaseDialog.ShowModal;
end;

{ TFormPhaseDialog }

procedure TFormPhaseDialog.FormCreate(Sender: TObject);
begin
  FCurrentEpoch := math.NaN;
  FCurrentPeriod := math.NaN;
end;

procedure TFormPhaseDialog.ButtonOKClick(Sender: TObject);
var
  V: Double;
begin
  ModalResult := mrNone;
  ButtonApplyClick(Sender);
  if FParamOk then
    ModalResult := mrOk;
end;

procedure TFormPhaseDialog.ButtonApplyClick(Sender: TObject);
var
  LPeriod, LEpoch: Double;
begin
  ModalResult := mrNone;
  FParamOk := False;
  LPeriod := GetPeriod;
  if IsNaN(LPeriod) then begin
    ShowMessage('Invalid Period');
    EditPeriod.SetFocus;
    EditPeriod.SelectAll;
    Exit;
  end;
  LEpoch := GetEpoch;
  if IsNaN(LEpoch) then begin
    ShowMessage('Invalid Epoch');
    EditEpoch.SetFocus;
    EditEpoch.SelectAll;
    Exit;
  end;
  CurrentPeriod := LPeriod;
  CurrentEpoch := LEpoch;
  FApplyPhasePlotParams;
  FParamOk := True;
end;

procedure TFormPhaseDialog.FormShow(Sender: TObject);
begin
  EditPeriod.SetFocus;
  EditPeriod.SelectAll;
end;

function TFormPhaseDialog.GetEpoch: Double;
var
  S: string;
  V: Double;
begin
  S := Trim(EditEpoch.Text);
  if not TryStrToFloat(S, V) then V := NaN;
  Result := V;
end;

function TFormPhaseDialog.GetPeriod: Double;
var
  S: string;
  V: Double;
begin
  S := Trim(EditPeriod.Text);
  if not TryStrToFloat(S, V) or (V <= 0) then V := NaN;
  Result := V;
end;

procedure TFormPhaseDialog.SetEpoch(AValue: Double);
begin
  if not IsNaN(AValue) then
    EditEpoch.Text := FloatToStr(AValue)
  else
    EditEpoch.Text := '';
end;

procedure TFormPhaseDialog.SetPeriod(AValue: Double);
begin
  if not IsNaN(AValue) then
    EditPeriod.Text := FloatToStr(AValue)
  else
    EditPeriod.Text := '';
end;

end.


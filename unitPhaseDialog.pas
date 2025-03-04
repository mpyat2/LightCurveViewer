unit unitPhaseDialog;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles,
  math;

type
  TApplyPhasePlotParams = procedure of object;

type

  { TFormPhaseDialog }

  TFormPhaseDialog = class(TForm)
    ButtonApply: TButton;
    ButtonOK: TButton;
    ButtonCancel: TButton;
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
  end;

function CalculateCycle(T, Period, Epoch: Double): Int64; inline;

function CalculatePhase(T, Period, Epoch: Double): Double; inline;

procedure PhasePlot(ApplyPhasePlotParamsProc: TApplyPhasePlotParams);

procedure SaveParameters(const Ini: TCustomIniFile; const Section: string);

procedure LoadParameters(const Ini: TCustomIniFile; const Section: string);

procedure SetCurrentEpoch(E: Double);

procedure SetCurrentPeriod(P: Double);

function GetCurrentEpoch: Double;

function GetCurrentPeriod: Double;

implementation

{$R *.lfm}

uses
  guiutils;

var
  CurrentEpoch: Double = NaN;
  CurrentPeriod: Double = NaN;

procedure SetCurrentEpoch(E: Double);
begin
  CurrentEpoch := E;
end;

procedure SetCurrentPeriod(P: Double);
begin
  CurrentPeriod := P;
end;

function GetCurrentEpoch: Double;
begin
  Result := CurrentEpoch;
end;

function GetCurrentPeriod: Double;
begin
  Result := CurrentPeriod;
end;

function CalculateCycle(T, Period, Epoch: Double): Int64; inline;
begin
  Result := Floor64((T - Epoch) / Period);
end;

function CalculatePhase(T, Period, Epoch: Double): Double; inline;
var
  Cycle: Int64;
begin
  Cycle := CalculateCycle(T, Period, Epoch);
  Result := (T - Epoch - Period * Cycle) / Period;
end;

procedure PhasePlot(ApplyPhasePlotParamsProc: TApplyPhasePlotParams);
var
  F: TFormPhaseDialog;
begin
  F := TFormPhaseDialog.Create(Application);
  try
    F.FApplyPhasePlotParams := ApplyPhasePlotParamsProc;
    F.SetEpoch(CurrentEpoch);
    F.SetPeriod(CurrentPeriod);
    F.ShowModal;
  finally
    FreeAndNil(F);
  end;
end;

procedure SaveParameters(const Ini: TCustomIniFile; const Section: string);
begin
  Ini.WriteFloat(Section, 'phaseplot.period', CurrentPeriod);
  Ini.WriteFloat(Section, 'phaseplot.epoch', CurrentEpoch);
end;

procedure LoadParameters(const Ini: TCustomIniFile; const Section: string);
begin
  CurrentPeriod := Ini.ReadFloat(Section, 'phaseplot.period', CurrentPeriod);
  CurrentEpoch := Ini.ReadFloat(Section, 'phaseplot.epoch', CurrentEpoch);
end;

{ TFormPhaseDialog }

procedure TFormPhaseDialog.FormCreate(Sender: TObject);
begin
  RearrangeButtons(ButtonOK, ButtonCancel, ButtonApply);
end;

procedure TFormPhaseDialog.ButtonOKClick(Sender: TObject);
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


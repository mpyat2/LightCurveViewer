unit unitDFTparamDialog;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles;

type

  { TFormDFTparams }

  TFormDFTparams = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    EditTrendDegree: TEdit;
    EditTrigPolyDegree: TEdit;
    EditFrequencyMin: TEdit;
    EditFrequencyMax: TEdit;
    EditFrequencyResolution: TEdit;
    LabelTrendDegree: TLabel;
    LabelTrigPolyDegree: TLabel;
    LabelFrequencyMin: TLabel;
    LabelFrequencyMax: TLabel;
    LabelFrequencyResolution: TLabel;
    procedure ButtonOkClick(Sender: TObject);
  private
    FFrequencyMin: Double;
    FFrequencyMax: Double;
    FFrequencyResolution: Double;
    FTrendDegree: Integer;
    FTrigPolyDegree: Integer;
  public

  end;

procedure SetCurrentFrequencyMin(AValue: Double);

procedure SetCurrentFrequencyMax(AValue: Double);

procedure SetCurrentFrequencyResolution(AValue: Double);

procedure SetCurrentTrendDegree(AValue: Integer);

procedure SetCurrentTrigPolyDegree(AValue: Integer);

function IsResolutionDefined: Boolean;

procedure SaveParameters(const Ini: TCustomIniFile; const Section: string);

procedure LoadParameters(const Ini: TCustomIniFile; const Section: string);

function GetDFTparams(out AFrequencyMin, AFrequencyMax, AFrequencyResolution: Double; out ATrendDegree, ATrigPolyDegree: Integer): Boolean;

implementation

{$R *.lfm}

uses
  math, common;

var
  CurrentFrequencyMin: Double = NaN;
  CurrentFrequencyMax: Double = NaN;
  CurrentFrequencyResolution: Double = NaN;
  CurrentTrendDegree: Integer = 0;
  CurrentTrigPolyDegree: Integer = 1;

procedure SetCurrentFrequencyMin(AValue: Double);
begin
  CurrentFrequencyMin := AValue;
end;

procedure SetCurrentFrequencyMax(AValue: Double);
begin
  CurrentFrequencyMax := AValue;
end;

procedure SetCurrentFrequencyResolution(AValue: Double);
begin
  CurrentFrequencyResolution := AValue;
end;

procedure SetCurrentTrendDegree(AValue: Integer);
begin
  CurrentTrendDegree := AValue;
end;

procedure SetCurrentTrigPolyDegree(AValue: Integer);
begin
  CurrentTrigPolyDegree := AValue;
end;

function IsResolutionDefined: Boolean;
begin
  Result := not IsNan(CurrentFrequencyResolution);
end;

procedure SaveParameters(const Ini: TCustomIniFile; const Section: string);
begin
  Ini.WriteFloat(Section, 'periodogram.freqmin', CurrentFrequencyMin);
  Ini.WriteFloat(Section, 'periodogram.freqmax', CurrentFrequencyMax);
  Ini.WriteFloat(Section, 'periodogram.resolution', CurrentFrequencyResolution);
  Ini.WriteInteger(Section, 'periodogram.trenddegree', CurrentTrendDegree);
  Ini.WriteInteger(Section, 'periodogram.trigpolydegree', CurrentTrigPolyDegree);
end;

procedure LoadParameters(const Ini: TCustomIniFile; const Section: string);
begin
  CurrentFrequencyMin := Ini.ReadFloat(Section, 'periodogram.freqmin', NaN);
  CurrentFrequencyMax := Ini.ReadFloat(Section, 'periodogram.freqmax', NaN);
  CurrentFrequencyResolution := Ini.ReadFloat(Section, 'periodogram.resolution', NaN);
  CurrentTrendDegree := Ini.ReadInteger(Section, 'periodogram.trenddegree', 0);
  CurrentTrigPolyDegree := Ini.ReadInteger(Section, 'periodogram.trigpolydegree', 1);
end;

function GetDFTparams(out AFrequencyMin, AFrequencyMax, AFrequencyResolution: Double; out ATrendDegree, ATrigPolyDegree: Integer): Boolean;
var
  F: TFormDFTparams;
begin
  Result := False;
  F := TFormDFTparams.Create(Application);
  try
    F.EditFrequencyMin.Text := '';
    F.EditFrequencyMax.Text := '';
    F.EditFrequencyresolution.Text := '';
    F.EditTrigPolyDegree.Text := '';
    if not IsNan(CurrentFrequencyMin) then F.EditFrequencyMin.Text := FloatToStr(CurrentFrequencyMin);
    if not IsNan(CurrentFrequencyMax) then F.EditFrequencyMax.Text := FloatToStr(CurrentFrequencyMax);
    if not IsNan(CurrentFrequencyResolution) then F.EditFrequencyresolution.Text := FloatToStr(CurrentFrequencyResolution);
    F.EditTrendDegree.Text := IntToStr(CurrentTrendDegree);
    F.EditTrigPolyDegree.Text := IntToStr(CurrentTrigPolyDegree);
    F.ShowModal;
    if F.ModalResult <> mrOK then
      Exit;
    CurrentFrequencyMin := F.FFrequencyMin;
    CurrentFrequencyMax := F.FFrequencyMax;
    CurrentFrequencyResolution := F.FFrequencyResolution;
    CurrentTrendDegree := F.FTrendDegree;
    CurrentTrigPolyDegree := F.FTrigPolyDegree;
    AFrequencyMin := CurrentFrequencyMin;
    AFrequencyMax := CurrentFrequencyMax;
    AFrequencyResolution := CurrentFrequencyResolution;
    ATrendDegree := CurrentTrendDegree;
    ATrigPolyDegree := CurrentTrigPolyDegree;
  finally
    FreeAndNil(F);
  end;
  Result := True;
end;

{ TFormDFTparams }

procedure TFormDFTparams.ButtonOkClick(Sender: TObject);
var
  MinF, MaxF, Resolution: Double;
  TrendDegree, TrigPolyDegree: Integer;
begin
  ModalResult := mrNone;
  if not GetFieldValue(EditFrequencyMin, 0, NaN, LabelFrequencyMin.Caption, MinF) then
    Exit;
  if not GetFieldValue(EditFrequencyMax, MinF, NaN, LabelFrequencyMax.Caption, MaxF) then
    Exit;
  if not GetFieldValue(EditFrequencyResolution, 0, NaN, LabelFrequencyResolution.Caption, Resolution) then
    Exit;
  if not GetFieldValue(EditTrendDegree, 0, 50, LabelTrendDegree.Caption, TrendDegree) then
    Exit;
  if not GetFieldValue(EditTrigPolyDegree, 1, 25, LabelTrigPolyDegree.Caption, TrigPolyDegree) then
    Exit;
  FFrequencyMin := MinF;
  FFrequencyMax := MaxF;
  FFrequencyResolution := Resolution;
  FTrendDegree := TrendDegree;
  FTrigPolyDegree := TrigPolyDegree;
  ModalResult := mrOK;
end;

end.


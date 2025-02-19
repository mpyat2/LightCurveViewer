unit unitFitParamDialog;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles,
  lcvtypes;

type

  { TFormFitparams }

  TFormFitparams = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    LabelTrendDegree: TLabel;
    LabelTrigPoly: TLabel;
    LabelPeriod1: TLabel;
    LabelPeriod2: TLabel;
    LabelPeriod3: TLabel;
    LabelTrigDeg1: TLabel;
    LabelTrigDeg2: TLabel;
    LabelTrigDeg3: TLabel;
    EditTrendDegree: TEdit;
    EditPeriod1: TEdit;
    EditPeriod2: TEdit;
    EditPeriod3: TEdit;
    EditTrigDeg1: TEdit;
    EditTrigDeg2: TEdit;
    EditTrigDeg3: TEdit;
    procedure ButtonOkClick(Sender: TObject);
  private
    FTrendDegree: Integer;
    FPeriods: TDouble3Array;
    FTrigPolyDegrees: TInt3Array;
  public

  end;

procedure SetCurrentTrendDegree(AValue: Integer);

procedure SetCurrentPeriods(const AValue: TDouble3Array);

procedure SetCurrentTrigPolyDegrees(const AValue: TInt3Array);

procedure SaveParameters(const Ini: TCustomIniFile; const Section: string);

procedure LoadParameters(const Ini: TCustomIniFile; const Section: string);

function GetFitParams(out ATrendDegree: Integer; out ATrigPolyDegrees: TInt3Array; out AFrequencies: TDouble3Array): Boolean;

implementation

{$R *.lfm}

uses
  math, guiutils;

var
  CurrentTrendDegree: Integer = 1;
  CurrentPeriods: TDouble3Array = (NaN, NaN, NaN);
  CurrentTrigPolyDegrees: TInt3Array = (0, 0, 0);

procedure SetCurrentTrendDegree(AValue: Integer);
begin
  CurrentTrendDegree := AValue;
end;

procedure SetCurrentPeriods(const AValue: TDouble3Array);
begin
  CurrentPeriods := AValue;
end;

procedure SetCurrentTrigPolyDegrees(const AValue: TInt3Array);
begin
  CurrentTrigPolyDegrees := AValue;
end;

procedure SaveParameters(const Ini: TCustomIniFile; const Section: string);
var
  I: Integer;
begin
  Ini.WriteInteger(Section, 'fit.trenddegree', CurrentTrendDegree);
  for I := 0 to Length(CurrentPeriods) - 1 do
    Ini.WriteFloat(Section, 'fit.period' + IntToStr(I + 1), CurrentPeriods[I]);
  for I := 0 to Length(CurrentTrigPolyDegrees) - 1 do
    Ini.WriteInteger(Section, 'fit.trigpolydegree' + IntToStr(I + 1), CurrentTrigPolyDegrees[I]);
end;

procedure LoadParameters(const Ini: TCustomIniFile; const Section: string);
var
  I: Integer;
begin
  CurrentTrendDegree := Ini.ReadInteger(Section, 'fit.trenddegree', 1);
  for I := 0 to Length(CurrentPeriods) - 1 do
    CurrentPeriods[I] := Ini.ReadFloat(Section, 'fit.period' + IntToStr(I + 1), NaN);
  for I := 0 to Length(CurrentTrigPolyDegrees) - 1 do
    CurrentTrigPolyDegrees[I] := Ini.ReadInteger(Section, 'fit.trigpolydegree' + IntToStr(I + 1), 0);
end;

function GetFitParams(out ATrendDegree: Integer; out ATrigPolyDegrees: TInt3Array; out AFrequencies: TDouble3Array): Boolean;
var
  Edit: TEdit;
  F: TFormFitParams;
  I: Integer;
begin
  Result := False;
  F := TFormFitparams.Create(Application);
  try
    F.EditTrendDegree.Text := IntToStr(CurrentTrendDegree);
    for I := 0 to Length(CurrentPeriods) - 1 do begin
      Edit := F.FindComponent('EditPeriod' + IntToStr(I+1)) as TEdit;
      if not IsNan(CurrentPeriods[I]) then
        Edit.Text := FloatToStr(CurrentPeriods[I])
      else
        Edit.Text := '';
      Edit := F.FindComponent('EditTrigDeg' + IntToStr(I+1)) as TEdit;
      Edit.Text := IntToStr(CurrentTrigPolyDegrees[I]);
    end;
    F.ShowModal;
    if F.ModalResult <> mrOK then
      Exit;
    CurrentTrendDegree := F.FTrendDegree;
    CurrentPeriods := F.FPeriods;
    CurrentTrigPolyDegrees := F.FTrigPolyDegrees;
    for I := 0 to Length(CurrentPeriods) - 1 do begin
      if CurrentTrigPolyDegrees[I] > 0 then
        AFrequencies[I] := 1.0 / CurrentPeriods[I]
      else
        AFrequencies[I] := NaN;
    end;
    ATrendDegree := CurrentTrendDegree;
    ATrigPolyDegrees := CurrentTrigPolyDegrees;
  finally
    FreeAndNil(F);
  end;
  Result := True;
end;

{ TFormFitparams }

procedure TFormFitparams.ButtonOkClick(Sender: TObject);
var
  Edit: TEdit;
  Lab: TLabel;
  Periods: TDouble3Array;
  TrigPolyDegrees: TInt3Array;
  TrendDegree: Integer;
  I: Integer;
begin
  ModalResult := mrNone;
  if not GetFieldValue(EditTrendDegree, 0, 50, LabelTrendDegree.Caption, TrendDegree) then
    Exit;
  for I := 0 to Length(FPeriods) - 1 do begin
    Edit := FindComponent('EditPeriod' + IntToStr(I+1)) as TEdit;
    Lab := FindComponent('LabelPeriod' + IntToStr(I+1)) as TLabel;
    if Trim(Edit.Text) <> '' then begin
      if not GetFieldValue(Edit, 0.000001, NaN, Lab.Caption, Periods[I]) then
        Exit;
      Edit := FindComponent('EditTrigDeg' + IntToStr(I+1)) as TEdit;
      Lab := FindComponent('LabelTrigDeg' + IntToStr(I+1)) as TLabel;
      if not GetFieldValue(Edit, 0, 25, Lab.Caption, TrigPolyDegrees[I]) then
        Exit;
    end
    else begin
      Periods[I] := NaN;
      TrigPolyDegrees[I] := 0;
    end;
  end;
  FTrendDegree := TrendDegree;
  FPeriods := Periods;
  FTrigPolyDegrees := TrigPolyDegrees;
  ModalResult := mrOK;
end;

end.


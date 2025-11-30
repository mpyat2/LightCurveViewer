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
    LabelPeriod4: TLabel;
    LabelPeriod5: TLabel;
    LabelPeriod6: TLabel;
    LabelPeriod7: TLabel;
    LabelPeriod8: TLabel;
    LabelPeriod9: TLabel;
    LabelTrigDeg1: TLabel;
    LabelTrigDeg2: TLabel;
    LabelTrigDeg3: TLabel;
    LabelTrigDeg4: TLabel;
    LabelTrigDeg5: TLabel;
    LabelTrigDeg6: TLabel;
    LabelTrigDeg7: TLabel;
    LabelTrigDeg8: TLabel;
    LabelTrigDeg9: TLabel;
    EditTrendDegree: TEdit;
    EditPeriod1: TEdit;
    EditPeriod2: TEdit;
    EditPeriod3: TEdit;
    EditPeriod4: TEdit;
    EditPeriod5: TEdit;
    EditPeriod6: TEdit;
    EditPeriod7: TEdit;
    EditPeriod8: TEdit;
    EditPeriod9: TEdit;
    EditTrigDeg1: TEdit;
    EditTrigDeg2: TEdit;
    EditTrigDeg3: TEdit;
    EditTrigDeg4: TEdit;
    EditTrigDeg5: TEdit;
    EditTrigDeg6: TEdit;
    EditTrigDeg7: TEdit;
    EditTrigDeg8: TEdit;
    EditTrigDeg9: TEdit;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTrendDegree: Integer;
    FPeriods: THarmonicDblArray;
    FTrigPolyDegrees: THarmonicIntArray;
  public

  end;

procedure SetCurrentTrendDegree(AValue: Integer);

procedure SetCurrentPeriods(const AValue: THarmonicDblArray);

procedure SetCurrentTrigPolyDegrees(const AValue: THarmonicIntArray);

procedure SaveParameters(const Ini: TCustomIniFile; const Section: string);

procedure LoadParameters(const Ini: TCustomIniFile; const Section: string);

function GetFitParams(var ATrendDegree: Integer;
                      var ATrigPolyDegrees: THarmonicIntArray;
                      var AFrequencies: THarmonicDblArray;
                      UseInitialValues: Boolean): Boolean;

implementation

{$R *.lfm}

uses
  math, guiutils;

var
  CurrentTrendDegree: Integer = 1;
  CurrentPeriods: THarmonicDblArray = (NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN);
  CurrentTrigPolyDegrees: THarmonicIntArray = (0, 0, 0, 0, 0, 0, 0, 0, 0);

procedure SetCurrentTrendDegree(AValue: Integer);
begin
  CurrentTrendDegree := AValue;
end;

procedure SetCurrentPeriods(const AValue: THarmonicDblArray);
begin
  CurrentPeriods := AValue;
end;

procedure SetCurrentTrigPolyDegrees(const AValue: THarmonicIntArray);
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

function GetFitParams(var ATrendDegree: Integer;
                      var ATrigPolyDegrees: THarmonicIntArray;
                      var AFrequencies: THarmonicDblArray;
                      UseInitialValues: Boolean): Boolean;
var
  Edit: TEdit;
  F: TFormFitParams;
  I: Integer;
begin
  Result := False;
  F := TFormFitparams.Create(Application);
  try
    if not UseInitialValues then
      F.EditTrendDegree.Text := IntToStr(CurrentTrendDegree)
    else
      F.EditTrendDegree.Text := IntToStr(ATrendDegree);

    for I := 0 to Length(CurrentPeriods) - 1 do begin
      Edit := F.FindComponent('EditPeriod' + IntToStr(I+1)) as TEdit;
      Assert(Edit <> nil);
      if not UseInitialValues then begin
        if not IsNan(CurrentPeriods[I]) then
          Edit.Text := FloatToStr(CurrentPeriods[I])
        else
          Edit.Text := '';
      end else begin
        if not IsNan(AFrequencies[I]) and (AFrequencies[I] > 0) then
          Edit.Text := FloatToStr(1.0 / AFrequencies[I])
        else
          Edit.Text := '';
      end;
      Edit := F.FindComponent('EditTrigDeg' + IntToStr(I+1)) as TEdit;
      Assert(Edit <> nil);
      if not UseInitialValues then
        Edit.Text := IntToStr(CurrentTrigPolyDegrees[I])
      else
        Edit.Text := IntToStr(ATrigPolyDegrees[I]);
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

procedure TFormFitparams.FormCreate(Sender: TObject);
begin
  RearrangeButtons(ButtonOK, ButtonCancel);
end;

procedure TFormFitparams.ButtonOkClick(Sender: TObject);
var
  Edit: TEdit;
  Lab: TLabel;
  Periods: THarmonicDblArray;
  TrigPolyDegrees: THarmonicIntArray;
  TrendDegree: Integer;
  I: Integer;
begin
  ModalResult := mrNone;
  if not GetFieldValue(EditTrendDegree, 0, 50, LabelTrendDegree.Caption, TrendDegree) then
    Exit;
  for I := 0 to Length(FPeriods) - 1 do begin
    Edit := FindComponent('EditPeriod' + IntToStr(I+1)) as TEdit;
    Assert(Edit <> nil);
    Lab := FindComponent('LabelPeriod' + IntToStr(I+1)) as TLabel;
    Assert(Lab <> nil);
    if Trim(Edit.Text) <> '' then begin
      if not GetFieldValue(Edit, 0.000001, NaN, Lab.Caption, Periods[I]) then
        Exit;
      Edit := FindComponent('EditTrigDeg' + IntToStr(I+1)) as TEdit;
      Assert(Edit <> nil);
      Lab := FindComponent('LabelTrigDeg' + IntToStr(I+1)) as TLabel;
      Assert(Lab <> nil);
      if not GetFieldValue(Edit, 0, 50, Lab.Caption, TrigPolyDegrees[I]) then
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


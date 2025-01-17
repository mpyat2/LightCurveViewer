unit unitfitparamdialog;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormDFTparams }

  TFormFitparams = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    EditTrendDegree: TEdit;
    EditTrigPolyDegree: TEdit;
    EditPeriod: TEdit;
    LabelTrendDegree: TLabel;
    LabelTrigPolyDegree: TLabel;
    LabelPeriod: TLabel;
    procedure ButtonOkClick(Sender: TObject);
  private
    FPeriod: Double;
    FTrendDegree: Integer;
    FTrigPolyDegree: Integer;
  public

  end;

//procedure SetCurrentPeriod(AValue: Double);

//procedure SetCurrentTrendDegree(AValue: Integer);

//procedure SetCurrentTrigPolyDegree(AValue: Integer);

function GetFitParams(out AFrequency: Double; out ATrendDegree, ATrigPolyDegree: Integer): Boolean;

implementation

{$R *.lfm}

uses
  math, common;

var
  CurrentPeriod: Double = NaN;
  CurrentTrendDegree: Integer = 1;
  CurrentTrigPolyDegree: Integer = 0;

procedure SetCurrentPeriod(AValue: Double);
begin
  CurrentPeriod := AValue;
end;

procedure SetCurrentTrendDegree(AValue: Integer);
begin
  CurrentTrendDegree := AValue;
end;

procedure SetCurrentTrigPolyDegree(AValue: Integer);
begin
  CurrentTrigPolyDegree := AValue;
end;

function GetFitParams(out AFrequency: Double; out ATrendDegree, ATrigPolyDegree: Integer): Boolean;
var
  F: TFormFitparams;
begin
  Result := False;
  F := TFormFitparams.Create(Application);
  try
    F.EditPeriod.Text := '';
    F.EditTrigPolyDegree.Text := '';
    if not IsNan(CurrentPeriod) then F.EditPeriod.Text := FloatToStr(CurrentPeriod);
    F.EditTrendDegree.Text := IntToStr(CurrentTrendDegree);
    F.EditTrigPolyDegree.Text := IntToStr(CurrentTrigPolyDegree);
    F.ShowModal;
    if F.ModalResult <> mrOK then
      Exit;
    CurrentPeriod := F.FPeriod;
    CurrentTrendDegree := F.FTrendDegree;
    CurrentTrigPolyDegree := F.FTrigPolyDegree;
    if CurrentTrigPolyDegree > 0 then
      AFrequency := 1.0 / CurrentPeriod
    else
      AFrequency := NaN;
    ATrendDegree := CurrentTrendDegree;
    ATrigPolyDegree := CurrentTrigPolyDegree;
  finally
    FreeAndNil(F);
  end;
  Result := True;
end;

{ TFormFitparams }

procedure TFormFitparams.ButtonOkClick(Sender: TObject);
var
  Period: Double;
  TrendDegree, TrigPolyDegree: Integer;
begin
  ModalResult := mrNone;
  if not GetFieldValue(EditTrendDegree, 0, 20, LabelTrendDegree.Caption, TrendDegree) then
    Exit;
  if not GetFieldValue(EditTrigPolyDegree, 0, 10, LabelTrigPolyDegree.Caption, TrigPolyDegree) then
    Exit;
  if (TrigPolyDegree > 0) then begin
    if not GetFieldValue(EditPeriod, 0.000001, NaN, LabelPeriod.Caption, Period) then
      Exit;
  end
  else
    Period := NaN;
  FPeriod := Period;
  FTrendDegree := TrendDegree;
  FTrigPolyDegree := TrigPolyDegree;
  ModalResult := mrOK;
end;

end.


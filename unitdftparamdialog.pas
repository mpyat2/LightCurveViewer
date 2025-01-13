unit unitdftparamdialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormDFTparams }

  TFormDFTparams = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    EditFrequencyMin: TEdit;
    EditFrequencyMax: TEdit;
    EditFrequencyResolution: TEdit;
    LabelFrequencyMin: TLabel;
    LabelFrequencyMax: TLabel;
    LabelFrequencyResolution: TLabel;
    procedure ButtonOkClick(Sender: TObject);
  private
    FFrequencyMin: Double;
    FFrequencyMax: Double;
    FFrequencyResolution: Double;
  public

  end;

procedure SetCurrentFrequencyMin(AValue: Double);

procedure SetCurrentFrequencyMax(AValue: Double);

procedure SetCurrentFrequencyResolution(AValue: Double);

function GetDFTparams(out AFrequencyMin, AFrequencyMax, AFrequencyResolution: Double): Boolean;

implementation

{$R *.lfm}

uses
  math;

var
  CurrentFrequencyMin: Double = NaN;
  CurrentFrequencyMax: Double = NaN;
  CurrentFrequencyResolution: Double = NaN;

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

function GetDFTparams(out AFrequencyMin, AFrequencyMax, AFrequencyResolution: Double): Boolean;
var
  F: TFormDFTparams;
begin
  Result := False;
  F := TFormDFTparams.Create(Application);
  try
    F.EditFrequencyMin.Text := '';
    F.EditFrequencyMax.Text := '';
    F.EditFrequencyresolution.Text := '';
    if not IsNan(CurrentFrequencyMin) then F.EditFrequencyMin.Text := FloatToStr(CurrentFrequencyMin);
    if not IsNan(CurrentFrequencyMax) then F.EditFrequencyMax.Text := FloatToStr(CurrentFrequencyMax);
    if not IsNan(CurrentFrequencyResolution) then F.EditFrequencyresolution.Text := FloatToStr(CurrentFrequencyResolution);
    F.ShowModal;
    if F.ModalResult <> mrOK then
      Exit;
    CurrentFrequencyMin := F.FFrequencyMin;
    CurrentFrequencyMax := F.FFrequencyMax;
    CurrentFrequencyResolution := F.FFrequencyResolution;
    AFrequencyMin := CurrentFrequencyMin;
    AFrequencyMax := CurrentFrequencyMax;
    AFrequencyResolution := CurrentFrequencyResolution;
  finally
    FreeAndNil(F);
  end;
  Result := True;
end;

{ TFormDFTparams }

function GetFieldValue(const Field: TEdit; Min, Max: Double; const FieldName: string; out V: Double): Boolean;
var
  S: string;
begin
  Result := False;
  S := Trim(Field.Text);
  if not TryStrToFloat(S, V) then begin
    Field.SetFocus;
    Field.SelectAll;
    ShowMessage(FieldName + ': Invalid value');
    Exit;
  end;
  if (not IsNaN(Min)) and (V < Min) then begin
    Field.SetFocus;
    Field.SelectAll;
    ShowMessage(FieldName + ': Value must be greater than or equal to ' + FloatToStr(Min));
    Exit;
  end;
  if (not IsNaN(Max)) and (V > Max) then begin
    Field.SetFocus;
    Field.SelectAll;
    ShowMessage(FieldName + ': Value must be less than or equal to ' + FloatToStr(Max));
    Exit;
  end;
  Result := True;
end;

procedure TFormDFTparams.ButtonOkClick(Sender: TObject);
var
  MinF, MaxF, Resolution: Double;
begin
  ModalResult := mrNone;
  if not GetFieldValue(EditFrequencyMin, 0, NaN, LabelFrequencyMin.Caption, MinF) then
    Exit;
  if not GetFieldValue(EditFrequencyMax, MinF, NaN, LabelFrequencyMax.Caption, MaxF) then
    Exit;
  if not GetFieldValue(EditFrequencyResolution, 0, NaN, LabelFrequencyResolution.Caption, Resolution) then
    Exit;
  FFrequencyMin := MinF;
  FFrequencyMax := MaxF;
  FFrequencyResolution := Resolution;
  ModalResult := mrOK;
end;

end.


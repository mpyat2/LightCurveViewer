unit unitMagShiftDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TApplyMagShiftParams = procedure of object;

type

  { TFormMagShift }

  TFormMagShift = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ButtonApply: TButton;
    EditMagShift: TEdit;
    LabelMagShift: TLabel;
    PanelControls: TPanel;
    PanelButtons: TPanel;
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetMagShift: Double;
    procedure SetMagShift(AValue: Double);
  private
    FApplyMagShiftParams: TApplyMagShiftParams;
    FParamOk: Boolean;
  public

  end;

procedure MagnitudeShifter(ApplyMagShiftParamsProc: TApplyMagShiftParams);

function GetCurrentMagShift: Double;

procedure SetCurrentMagShift(V: Double);

implementation

{$R *.lfm}

uses
  math, guiutils;

var
  CurrentMagShift: Double = NaN;

function GetCurrentMagShift: Double;
begin
  Result := CurrentMagShift;
end;

procedure SetCurrentMagShift(V: Double);
begin
  CurrentMagShift := V;
end;

procedure MagnitudeShifter(ApplyMagShiftParamsProc: TApplyMagShiftParams);
var
  F: TFormMagShift;
begin
  F := TFormMagShift.Create(Application);
  try
    F.FApplyMagShiftParams := ApplyMagShiftParamsProc;
    F.SetMagShift(CurrentMagShift);
    F.ShowModal;
  finally
    FreeAndNil(F);
  end;
end;

{ TFormMagShift }

procedure TFormMagShift.FormCreate(Sender: TObject);
begin
  RearrangeButtons(ButtonOK, ButtonCancel, ButtonApply);
end;

procedure TFormMagShift.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrNone;
  ButtonApplyClick(Sender);
  if FParamOk then
    ModalResult := mrOk;
end;

procedure TFormMagShift.ButtonApplyClick(Sender: TObject);
var
  LMagShift: Double;
begin
  ModalResult := mrNone;
  FParamOk := False;
  LMagShift := GetMagShift;
  if IsNaN(LMagShift) then
    Exit;
  CurrentMagShift := LMagShift;
  FApplyMagShiftParams;
  FParamOk := True;
end;

procedure TFormMagShift.FormShow(Sender: TObject);
begin
  EditMagShift.SetFocus;
  EditMagShift.SelectAll;
end;

function TFormMagShift.GetMagShift: Double;
var
  V: Double;
begin
  if GetFieldValue(EditMagShift, NaN, NaN, LabelMagShift.Caption, V) then
    Result := V
  else
    Result := NaN;
end;

procedure TFormMagShift.SetMagShift(AValue: Double);
begin
  if not IsNaN(AValue) then
    EditMagShift.Text := FloatToStr(AValue)
  else
    EditMagShift.Text := '';
end;

end.


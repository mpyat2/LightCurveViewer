unit unitOptionsDialog;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin;

const
  MIN_MODEL_POINTS = 101;
  MAX_MODEL_POINTS = 1000001;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    CheckBoxShowGrid: TCheckBox;
    EditNumberOfModelPoints: TEdit;
    GroupBoxChart: TGroupBox;
    GroupBoxGeneral: TGroupBox;
    LabelNofThreads: TLabel;
    LabelNofModelPoints: TLabel;
    PanelMain: TPanel;
    PanelBottom: TPanel;
    SpinEditNofProcessors: TSpinEdit;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

function ProgramOptions: Boolean;

implementation

{$R *.lfm}

uses
  miscutils, guiutils, settings;

function ProgramOptions: Boolean;
var
  F: TFormOptions;
begin
  Result := False;
  F := TFormOptions.Create(Application);
  try
    if F.ShowModal = mrOk then
      Result := True;
  finally
    FreeAndNil(F);
  end;
end;

{ TFormOptions }

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  RearrangeButtons(ButtonOK, ButtonCancel);
  EditNumberOfModelPoints.Text := IntToStr(GetGlobalIntParameter('ModelPoints', 0));
  LabelNofThreads.Caption := Format(LabelNofThreads.Caption, [GetLogicalCpuCount]);
  SpinEditNofProcessors.Value := GetGlobalIntParameter('Threads', GetLogicalCpuCount);
  CheckBoxShowGrid.Checked := GetGlobalBoolParameter('ShowGrid', True);
end;

procedure TFormOptions.ButtonOKClick(Sender: TObject);
var
  S: string;
  ModelPoints: Integer;
  Error: Word;
begin
  ModalResult := mrNone;
  S := Trim(EditNumberOfModelPoints.Text);
  Val(S, ModelPoints, Error);
  if (Error <> 0) or (ModelPoints <> 0) and ((ModelPoints < MIN_MODEL_POINTS) or (ModelPoints > MAX_MODEL_POINTS)) then begin
    ShowMessageFmt('Invalid number of model points. Value must be between %d and %d or 0.', [MIN_MODEL_POINTS, MAX_MODEL_POINTS]);
    EditNumberOfModelPoints.SetFocus;
    EditNumberOfModelPoints.SelectAll;
    Exit;
  end;
  SetGlobalIntParameter('ModelPoints', ModelPoints);
  SetGlobalIntParameter('Threads', SpinEditNofProcessors.Value);
  SetGlobalBoolParameter('ShowGrid', CheckBoxShowGrid.Checked);
  try
    UpdateParamFile;
  except
    on E: Exception do begin
      ShowMessage(E.Message);
    end;
  end;
  ModalResult := mrOk;
end;

end.


unit unitOptionsDialog;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    CheckBoxShowGrid: TCheckBox;
    GroupBoxChart: TGroupBox;
    GroupBoxGeneral: TGroupBox;
    LabelNofThreads: TLabel;
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
  LabelNofThreads.Caption := Format(LabelNofThreads.Caption, [GetLogicalCpuCount]);
  SpinEditNofProcessors.Value := GetGlobalIntParameter('Threads', GetLogicalCpuCount);
  CheckBoxShowGrid.Checked := GetGlobalBoolParameter('ShowGrid', True);
end;

procedure TFormOptions.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrNone;
  SetGlobalIntParameter('Threads', SpinEditNofProcessors.Value);
  SetGlobalBoolParameter('ShowGrid', CheckBoxShowGrid.Checked);
  ModalResult := mrOk;
end;

end.


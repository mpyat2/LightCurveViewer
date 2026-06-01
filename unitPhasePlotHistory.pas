unit unitPhasePlotHistory;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  lcvtypes;

type

  { TFormPhasePlotHistory }

  TFormPhasePlotHistory = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ListBoxPhasePlotHistory: TListBox;
    PanelButtons: TPanel;
    PanelList: TPanel;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxPhasePlotHistoryDblClick(Sender: TObject);
  private
    FSelectedPhasePlot: TPhaseHistoryItem;
    procedure InitForm;
  public

  end;

function SelectPreviousPhasePlot(out APhase: Double; out AEpoch: Double): Boolean;

procedure ClearPhasePlotHistory;

procedure AddToPhasePlotHistory(APhase, AEpoch: Double);

implementation

uses
  guiutils;

{$R *.lfm}

var
  PhaseHistory: TPhaseHistory;

function SelectPreviousPhasePlot(out APhase: Double; out AEpoch: Double): Boolean;
var
  F: TFormPhasePlotHistory;
begin
  Result := False;
  F := TFormPhasePlotHistory.Create(Application);
  try
    F.InitForm;
    if F.ShowModal = mrOK then begin
      if F.FSelectedPhasePlot = nil then
        raise Exception.Create('Internal error: Selected Phase Plot is not assigned.');
      APhase := F.FSelectedPhasePlot.FPhase;
      AEpoch := F.FSelectedPhasePlot.FEpoch;
      Result := True;
    end;
  finally
    FreeAndNil(F);
  end;
end;

procedure ClearPhasePlotHistory;
begin
  PhaseHistory.Clear;
end;

procedure AddToPhasePlotHistory(APhase, AEpoch: Double);
begin
  PhaseHistory.InsertItem(APhase, AEpoch);
end;

{ TFormPhasePlotHistory }

procedure TFormPhasePlotHistory.FormCreate(Sender: TObject);
begin
  RearrangeButtons(ButtonOK, ButtonCancel);
end;

procedure TFormPhasePlotHistory.ListBoxPhasePlotHistoryDblClick(Sender: TObject);
begin
  ButtonOKClick(Sender);
end;

procedure TFormPhasePlotHistory.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrNone;
  if ListBoxPhasePlotHistory.ItemIndex >= 0 then begin
    FSelectedPhasePlot := PhaseHistory.Get(ListBoxPhasePlotHistory.ItemIndex);
    ModalResult := mrOk;
  end;
end;

procedure TFormPhasePlotHistory.InitForm;
var
  I: Integer;
begin
  FSelectedPhasePlot := nil;
  for I := 0 to PhaseHistory.Count - 1 do begin
    ListBoxPhasePlotHistory.Items.Add(PhaseHistory.Get(I).GetDescription);
  end;
  if ListBoxPhasePlotHistory.Items.Count > 0 then
    ListBoxPhasePlotHistory.ItemIndex := 0;
end;

initialization
  PhaseHistory := TPhaseHistory.Create;
finalization
  FreeAndNil(PhaseHistory);
end.


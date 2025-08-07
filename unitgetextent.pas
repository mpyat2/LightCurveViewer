unit unitGetExtent;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  TAChartUtils;

type

  { TFormGetExtent }

  TFormGetExtent = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    EditLeft: TEdit;
    EditTop: TEdit;
    EditRight: TEdit;
    EditBottom: TEdit;
    LabelLeft: TLabel;
    LabelTop: TLabel;
    LabelRight: TLabel;
    LabelBottom: TLabel;
    PanelEdit: TPanel;
    PanelButtons: TPanel;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FX1, FX2, FY1, FY2: Double;
  public

  end;

  function GetExtent(var Extent: TDoubleRect): Boolean;

implementation

uses
  math, guiutils;

{$R *.lfm}

function GetExtent(var Extent: TDoubleRect): Boolean;
var
  F: TFormGetExtent;
begin
  Result := False;

  F := TFormGetExtent.Create(Application);
  try
    F.FX1 := Extent.a.X;
    F.FX2 := Extent.b.X;
    F.FY1 := Extent.a.Y;
    F.FY2 := Extent.b.Y;

    if F.ShowModal = mrOK then begin
      Extent.a.X := F.FX1;
      Extent.b.X := F.FX2;
      Extent.a.Y := F.FY1;
      Extent.b.Y := F.FY2;
      Result := True;
    end;
  finally
    FreeAndNil(F);
  end;
end;

{ TFormGetExtent }

procedure TFormGetExtent.FormCreate(Sender: TObject);
begin
  RearrangeButtons(ButtonOK, ButtonCancel);
end;

procedure TFormGetExtent.FormShow(Sender: TObject);
begin
  EditLeft.Text := FloatToStr(FX1);
  EditRight.Text := FloatToStr(FX2);
  EditTop.Text := FloatToStr(FY1);
  EditBottom.Text := FloatToStr(FY2);
end;

procedure TFormGetExtent.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrNone;

  if not GetFieldValue(EditLeft, NaN, NaN, 'Left', FX1) then Exit;
  if not GetFieldValue(EditRight, NaN, NaN, 'Right', FX2) then Exit;
  if not GetFieldValue(EditTop, NaN, NaN, 'Top', FY1) then Exit;
  if not GetFieldValue(EditBottom, NaN, NaN, 'Bottom', FY2) then Exit;

  if FX1 >= FX2 then begin
    ShowMessage('The Right value mut be greater than the Left one');
    Exit;
  end;

  if FY1 >= FY2 then begin
    ShowMessage('The Topt value mut be greater than the Bottom one');
    Exit;
  end;

  ModalResult := mrOK;
end;

end.


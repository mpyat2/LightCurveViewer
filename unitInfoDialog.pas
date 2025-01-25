unit unitInfoDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TFormInfo }

  TFormInfo = class(TForm)
    ButtonClose: TButton;
    MemoInfo: TMemo;
    PanelButtons: TPanel;
  private

  public

  end;

procedure ShowInfo(const Text: string; const Caption: string = '');

implementation

{$R *.lfm}

procedure ShowInfo(const Text: string; const Caption: string = '');
var
  F: TFormInfo;
begin
  F := TFormInfo.Create(Application);
  try
    F.MemoInfo.Text := Text;
    if Caption <> '' then
      F.Caption := Caption;
    F.ShowModal;
  finally
    FreeAndNil(F);
  end;
end;

end.


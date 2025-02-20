unit floattextform;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TFormFloatText }

  TFormFloatText = class(TForm)
    ButtonClose: TButton;
    ButtonClear: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

procedure AddText(const S: string);

implementation

{$R *.lfm}

var
  FormFloatText: TFormFloatText = nil;

procedure AddText(const S: string);
begin
  if FormFloatText = nil then begin
    FormFloatText := TFormFloatText.Create(Application);
  end;
  FormFloatText.Memo1.Lines.Add(S);
  FormFloatText.Show;
end;

{ TFormFloatText }

procedure TFormFloatText.ButtonClearClick(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TFormFloatText.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormFloatText.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FormFloatText := nil;
end;

end.


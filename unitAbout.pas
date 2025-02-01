unit unitAbout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

procedure About;

implementation

{$R *.lfm}

procedure About;
var
  F: TFormAbout;
begin
  F := TFormAbout.Create(Application);
  try
    F.ShowModal;
  finally
    FreeAndNil(F);
  end;
end;

{ TFormAbout }

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  Memo1.Text :=
  ^M^J +
  'Light Curve Viewer by Maksym Yu. Pyatnytskyy'^M^J^M^J +
  'This program implements some methods from:'^M^J^M^J +
  'Andronov, I. L., (Multi-) Frequency Variations of Stars. Some Methods and Results, ' +
  'Odessa Astronomical Publications, vol. 7, p. 49-54 (1994) [1994OAP.....7...49A]'^M^J^M^J +
  'Andronov, I. L., Advanced Time Series Analysis of Generally Irregularly Spaced Signals: ' +
  'Beyond the Oversimplified Methods, ' +
  'Knowledge Discovery in Big Data from Astronomy and Earth Observation, 1st Edition. ' +
  'Edited by Petr Skoda and Fathalrahman Adam. ISBN: 978-0-128-19154-5. Elsevier, 2020, p.191-224 [2020kdbd.book..191A]';
end;

{ TFormAbout }

{ TFormAbout }

end.


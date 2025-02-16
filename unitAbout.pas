unit unitAbout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    Button1: TButton;
    LabelVersion: TLabel;
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

uses
  Windows;

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

procedure GetVersionValues(const ModuleName: String; out V1, V2, V3, V4: Word);
var
  VerInfoSize:  DWORD;
  VerInfo:      Pointer;
  VerValueSize: DWORD;
  VerValue:     PVSFixedFileInfo;
  Dummy:        DWORD;
begin
  V1 := 0;
  V2 := 0;
  V3 := 0;
  V4 := 0;
  VerInfoSize := GetFileVersionInfoSize(PChar(ModuleName), Dummy);
  if VerInfoSize = 0 then Exit;
  GetMem(VerInfo, VerInfoSize);
  try
    if not GetFileVersionInfo(PChar(ModuleName), 0, VerInfoSize, VerInfo) then Exit;
    if not VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then Exit;
    with VerValue^ do begin
      V1 := dwFileVersionMS shr 16;
      V2 := dwFileVersionMS and $FFFF;
      V3 := dwFileVersionLS shr 16;
      V4 := dwFileVersionLS and $FFFF;
   end;
 finally
   FreeMem(VerInfo, VerInfoSize);
 end;
end;

function GetVersionString(const ModuleName: String): String;
var
  V1, V2, V3, V4: Word;
begin
  Result := '';
  GetVersionValues(ModuleName, V1, V2, V3, V4);
  Result := ExtractFileName(ModuleName) + ' Version ' + IntToStr(V1) + '.' + IntToStr(V2) + '.' + IntToStr(V3) + ' (Build ' + IntToStr(V4) + ')';
end;

{ TFormAbout }

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  LabelVersion.Caption := '  ' + GetVersionString(AnsiUpperCase(ParamStr(0))) +
                          ' FPC ' + {$I %FPCVERSION%} + ' ' + {$I %FPCTARGETOS%} +
                          ' ' + {$I %DATE%} + ' ' + {$I %TIME%};
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

end.


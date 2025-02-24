unit settings;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, IniFiles;

procedure LoadFormPosition(F: TForm);

procedure SaveFormPosition(F: TForm);

implementation

procedure LoadFormPosition(F: TForm);
var
  IniFile: TMemIniFile;
  IniName: string;
  Maximized: Boolean;
  Left, Top, Width, Height: Integer;
begin
  IniName := GetAppConfigFile(False);
  IniFile := TMemIniFile.Create(IniName);
  try
    F.Position := poDesigned;
    F.Left := 0;
    F.Top := 0;
    F.WindowState := wsNormal;

    Maximized := IniFile.ReadBool(F.Name, 'Maximized', False);
    Left := IniFile.ReadInteger(F.Name, 'Left', F.Left);
    Top := IniFile.ReadInteger(F.Name, 'Top', F.Top);
    Width := IniFile.ReadInteger(F.Name, 'Right', F.Width);
    Height := IniFile.ReadInteger(F.Name, 'Bottom', F.Height);

    if Left < 0 then
      Left := 0
    else
    if Left > Screen.Width - 24 then
      Left := Screen.Width - 24;
    if Top < 0 then
      Top := 0
    else
    if Top > Screen.Height - 24 then
      Top := Screen.Height - 24;

    if Width < 0 then
      Width := 0;
    if Height < 0 then
      Height := 0;

    F.SetBounds(Left, Top, Width, Height);

    if Maximized then begin
      F.WindowState := wsMaximized
    end;

    IniFile.UpdateFile;
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure SaveFormPosition(F: TForm);
var
  IniFile: TMemIniFile;
  IniName: string;
begin
  IniName := GetAppConfigFile(False);
  IniFile := TMemIniFile.Create(IniName);
  try
    IniFile.WriteBool(F.Name, 'Maximized', F.WindowState = wsMaximized);
    F.WindowState := wsNormal;
    IniFile.WriteInteger(F.Name, 'Left', F.Left);
    IniFile.WriteInteger(F.Name, 'Top', F.Top);
    IniFile.WriteInteger(F.Name, 'Width', F.Width);
    IniFile.WriteInteger(F.Name, 'Height', F.Height);
    IniFile.UpdateFile;
  finally
    FreeAndNil(IniFile);
  end;
end;

end.


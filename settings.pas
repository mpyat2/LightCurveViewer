unit settings;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, IniFiles;

procedure LoadFormPosition(F: TForm);

procedure SaveFormPosition(F: TForm);

function GetGlobalStringParameter(const Name: string; const Default: string): string;

function GetGlobalIntParameter(const Name: string; Default: Integer): Integer;

function GetGlobalBoolParameter(const Name: string; Default: Boolean): Boolean;

procedure SetGlobalStringParameter(const Name: string; const Value: string);

procedure SetGlobalIntParameter(const Name: string; Value: Integer);

procedure SetGlobalBoolParameter(const Name: string; Value: Boolean);

implementation

var
  GlobalIniFile: TMemIniFile = nil;

function GetGlobalStringParameter(const Name: string; const Default: string): string;
begin
  Result := GlobalIniFile.ReadString('ProgramSettings', Name, Default);
end;

function GetGlobalIntParameter(const Name: string; Default: Integer): Integer;
begin
  Result := GlobalIniFile.ReadInteger('ProgramSettings', Name, Default);
end;

function GetGlobalBoolParameter(const Name: string; Default: Boolean): Boolean;
begin
  Result := GlobalIniFile.ReadBool('ProgramSettings', Name, Default);
end;

procedure SetGlobalStringParameter(const Name: string; const Value: string);
begin
  GlobalIniFile.WriteString('ProgramSettings', Name, Value);
end;

procedure SetGlobalIntParameter(const Name: string; Value: Integer);
begin
  GlobalIniFile.WriteInteger('ProgramSettings', Name, Value);
end;

procedure SetGlobalBoolParameter(const Name: string; Value: Boolean);
begin
  GlobalIniFile.WriteBool('ProgramSettings', Name, Value);
end;

procedure LoadFormPosition(F: TForm);
var
  Maximized: Boolean;
  Left, Top, Width, Height: Integer;
begin
  F.Position := poDesigned;
  F.Left := 0;
  F.Top := 0;
  F.WindowState := wsNormal;

  Maximized := GlobalIniFile.ReadBool(F.Name, 'Maximized', False);
  Left := GlobalIniFile.ReadInteger(F.Name, 'Left', F.Left);
  Top := GlobalIniFile.ReadInteger(F.Name, 'Top', F.Top);
  Width := GlobalIniFile.ReadInteger(F.Name, 'Width', F.Width);
  Height := GlobalIniFile.ReadInteger(F.Name, 'Height', F.Height);

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

end;

procedure SaveFormPosition(F: TForm);
begin
  GlobalIniFile.WriteBool(F.Name, 'Maximized', F.WindowState = wsMaximized);
  F.WindowState := wsNormal;
  GlobalIniFile.WriteInteger(F.Name, 'Left', F.Left);
  GlobalIniFile.WriteInteger(F.Name, 'Top', F.Top);
  GlobalIniFile.WriteInteger(F.Name, 'Width', F.Width);
  GlobalIniFile.WriteInteger(F.Name, 'Height', F.Height);
  GlobalIniFile.UpdateFile;
end;

var
  IniName: string;

initialization
  IniName := GetAppConfigFile(False);
  GlobalIniFile := TMemIniFile.Create(IniName);

finalization
  FreeAndNil(GlobalIniFile);
end.


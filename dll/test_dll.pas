{$mode objfpc}{$H+}

program Test_DLL;

procedure sincos(var a: Double; var s: Double; var c: Double); cdecl; external 'sincos.dll' name 'sincos_';

var
  a, s, c: Double;
begin
  a := Pi / 4;
  sincos(a, s, c);
  WriteLn('sin(', a:0:3, ') = ', s:0:6);
  WriteLn('cos(', a:0:3, ') = ', c:0:6);
end.

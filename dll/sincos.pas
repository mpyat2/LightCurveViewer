{$mode objfpc}{$H+}

library sincos;

uses
  Math;

procedure sincos_(var a, s, c: Double); cdecl; export;
begin
  Math.sincos(a, s, c);
end;

exports
  sincos_;

begin
end.

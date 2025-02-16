unit formatutils;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils;

function StringToFloatLocaleIndependent(const S: string; out V: Double): Boolean;

function FloatToStrLocaleIndependent(V: Double): string;

function FloatToStrMod(V: Double): string;

implementation

function StringToFloatLocaleIndependent(const S: string; out V: Double): Boolean;
var
  Code: Integer;
begin
  Val(S, V, Code);
  Result := Code = 0;
end;

function FloatToStrLocaleIndependent(V: Double): string;
var
  F: TFormatSettings;
begin
  GetLocaleFormatSettings(0409, F);
  if Abs(V) > 0.0000001 then
    Result := FloatToStrF(V, ffFixed, 0, 15, F)
  else
    Result := FloatToStr(V, F);
end;

function FloatToStrMod(V: Double): string;
begin
  if Abs(V) > 0.0000001 then
    Result := FloatToStrF(V, ffFixed, 0, 15)
  else
    Result := FloatToStr(V);
  if V >= 0 then
    Result := ' ' + Result;
end;

end.


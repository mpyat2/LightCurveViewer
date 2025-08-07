{$mode objfpc}{$H+}

program Test_DLL;

procedure sincos(var a: Double; var s: Double; var c: Double); cdecl; external 'sincosF77.dll' name 'sincos_';
procedure sincos_a(var n: LongInt; var a: Double; var s: Double; var c: Double); cdecl; external 'sincosF77.dll' name 'sincos_array_';
procedure sincos_a2(var n: LongInt; var a: Double; var cs: Double); cdecl; external 'sincosF77.dll' name 'sincos_a2_';

var
  a, s, c: Double;
  
var
  i: Integer;
  N: Integer;
  AA, SS, CC: array[0..4] of Double;
  CS: array[0..9] of Double;

begin
  WriteLn('sincos test');
  a := Pi / 4;
  sincos(a, s, c);
  WriteLn('sin(', a:0:3, ') = ', s:0:6);
  WriteLn('cos(', a:0:3, ') = ', c:0:6);
  WriteLn;
  WriteLn;
  
  WriteLn('sincos_a test');
  N := Length(AA);
  for i := 0 to N - 1 do
    AA[i] := Pi * i / N;

  sincos_a(N, AA[0], SS[0], CC[0]);

  for i := 0 to N - 1 do
    WriteLn('AA[', i, '] = ', AA[i]:0:3, '  sin = ', SS[i]:0:6, '  cos = ', CC[i]:0:6);
  WriteLn;
  WriteLn;

  WriteLn('sincos_a2 test');

  sincos_a2(N, AA[0], CS[0]);

  for i := 0 to N - 1 do
    WriteLn('AA[', i, '] = ', AA[i]:0:3, '  sin = ', CS[i * 2 + 1]:0:6, '  cos = ', CS[i * 2]:0:6);

end.

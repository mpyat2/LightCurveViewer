unit dataio;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, typ, common;

procedure ReadData(const AFileName: string; out X: TFloatArray; out Y: TFloatArray);

implementation

function StringToFloat(const S: string; out F: ArbFloat): Boolean;
var
  Code: Integer;
begin
  Val(S, F, Code);
  Result := Code = 0;
end;

// Reads a text file assuming it contains columns of the floating-point values.
// Only the first two columns are taken into account.
// The empty lines and lines starting with '#' are ignored.
// The lines containing ASCII 127 character are ignored.
// The lines contaning less than 2 floating-point values in the first two
// columns are ignored.
// The columns are assumed to be separated by tabs or spaces.
procedure ReadData(const AFileName: string; out X: TFloatArray; out Y: TFloatArray);
var
  Lines, Line: TStrings;
  S: string;
  FX, FY: ArbFloat;
  I, N: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    SetLength(X, Lines.Count);
    SetLength(Y, Lines.Count);
    Line := TStringList.Create;
    try
      Line.Delimiter := #127;
      Line.QuoteChar := '"';
      N := 0;
      for I := 0 to Lines.Count - 1 do begin
        S := Trim(Lines[I]);
        if (S = '') or (S[1] = '#') or (Pos(#127, S) > 0) then
          Break;
        // DelimitedText treats repeating spaces (including tabs) as one delimiter.
        // To workaround this, use a spercial character.
        Line.DelimitedText := StringReplace(S, ^I, #127, [rfReplaceAll]);
        if Line.Count > 1 then begin
          if StringToFloat(Line[0], FX) and StringToFloat(Line[1], FY) then begin
            X[N] := FX;
            Y[N] := FY;
            Inc(N);
          end;
        end;
      end;
      SetLength(X, N);
      SetLength(Y, N);
    finally
      FreeAndNil(Line);
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

end.


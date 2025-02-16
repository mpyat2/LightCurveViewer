unit dataio;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, lcvtypes;

procedure ReadData(const AFileName: string; out X: TFloatArray; out Y: TFloatArray);

procedure WriteData(const AFileName: string; const X: TFloatArray; const Y: TFloatArray);

implementation

uses
  formatutils, sortutils;

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
  FX, FY: Double;
  I, N: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    Line := TStringList.Create;
    try
      Line.Delimiter := #127;
      Line.QuoteChar := '"';
      SetLength(X, Lines.Count);
      SetLength(Y, Lines.Count);
      N := 0;
      for I := 0 to Lines.Count - 1 do begin
        S := Trim(Lines[I]);
        if (S = '') or (S[1] = '#') or (Pos(#127, S) > 0) then
          Continue;
        // DelimitedText treats repeating spaces (including tabs) as one delimiter.
        // To workaround this, use a spercial character.
        Line.DelimitedText := StringReplace(S, ^I, #127, [rfReplaceAll]);
        if Line.Count > 1 then begin
          if StringToFloatLocaleIndependent(Line[0], FX) and StringToFloatLocaleIndependent(Line[1], FY) then begin
            X[N] := FX;
            Y[N] := FY;
            Inc(N);
          end;
        end;
      end;
      SetLength(X, N);
      SetLength(Y, N);
      // While it is not required now, sort the data
      SortDataPoints(X, Y);
    finally
      FreeAndNil(Line);
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

procedure WriteData(const AFileName: string; const X: TFloatArray; const Y: TFloatArray);
var
  Lines: TStrings;
  S: string;
  I: Integer;
begin
  Lines := TStringList.Create;
  try
    for I := 0 to Length(X) - 1 do begin
      S := FloatToStrLocaleIndependent(X[I]) + ' ' + FloatToStrLocaleIndependent(Y[I]);
      Lines.Add(S);
    end;
    Lines.SaveToFile(AFileName);
  finally
    FreeAndNil(Lines);
  end;
end;

end.


unit dataio;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, lcvtypes;

procedure ReadData(const AFileName: string; out X: TDoubleArray; out Y: TDoubleArray; out ObjectName: string);

procedure WriteData(const AFileName: string; const X: TDoubleArray; const Y: TDoubleArray);

implementation

uses
  formatutils, sortutils;

const
  defNameDirective = '#NAME=';

// Reads a text file assuming it contains columns of the floating-point values.
// Only the first two columns are taken into account.
// The empty lines and lines starting with '#' are ignored.
// The lines containing ASCII 127 character are ignored.
// The lines contaning less than 2 floating-point values in the first two
// columns are ignored.
// The columns are assumed to be separated by tabs or spaces.
// The data is sorted by X
procedure ReadData(const AFileName: string; out X: TDoubleArray; out Y: TDoubleArray; out ObjectName: string);
var
  Lines, Line: TStrings;
  S: string;
  FX, FY: Double;
  I, N: Integer;
begin
  ObjectName := ChangeFileExt(ExtractFileName(AFileName), '');
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
        if S = '' then
          Continue;
        if S[1] = '#' then begin
          if Copy(S, 1, Length(defNameDirective)) = defNameDirective then begin
            S := Trim(Copy(S, Length(defNameDirective) + 1, 255));
            if S <> '' then
              ObjectName := S;
          end;
          Continue;
        end;
        if Pos(#127, S) > 0 then
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
      // Sort the data
      SortDataPoints(X, Y);
    finally
      FreeAndNil(Line);
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

procedure WriteData(const AFileName: string; const X: TDoubleArray; const Y: TDoubleArray);
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


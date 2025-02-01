unit dataio;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, common;

procedure ReadData(const AFileName: string; out X: TFloatArray; out Y: TFloatArray);

procedure WriteData(const AFileName: string; const X: TFloatArray; const Y: TFloatArray);

implementation

function StringToFloat(const S: string; out F: Double): Boolean;
var
  Code: Integer;
begin
  Val(S, F, Code);
  Result := Code = 0;
end;

type
  TXY = class
    X, Y: Double;
    constructor Create(AX, AY: Double);
  end;

constructor TXY.Create(AX, AY: Double);
begin
  inherited Create;
  X := AX;
  Y := AY;
end;

function CompareXY(Item1, Item2: Pointer): Integer;
begin
  if TXY(Item1).X < TXY(Item2).X then
    Result := -1
  else
  if TXY(Item1).X > TXY(Item2).X then
    Result := 1
  else
    Result := 0;
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
  TempXYlist: TList;
  Lines, Line: TStrings;
  S: string;
  FX, FY: Double;
  I, N: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    TempXYlist := TList.Create;
    try
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
              TempXYlist.Add(TXY.Create(FX, FY));
              Inc(N);
            end;
          end;
        end;
        // While it is not required now, sort the data
        TempXYlist.Sort(@CompareXY);
        SetLength(X, N);
        SetLength(Y, N);
        for I := 0 to N - 1 do begin
          X[I] := TXY(TempXYlist.Items[I]).X;
          Y[I] := TXY(TempXYlist.Items[I]).Y;
        end;
      finally
        for I := TempXYlist.Count - 1 downto 0 do begin
          TXY(TempXYlist[I]).Free;
          TempXYlist[I] := nil;
        end;
        FreeAndNil(TempXYlist);
      end;
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


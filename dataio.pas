unit dataio;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, lcvtypes;

procedure ReadData(const AFileName: string; out X: TDoubleArray; out Y: TDoubleArray; out Errors: TDoubleArray; out ObjectName: string);

procedure WriteData(const AFileName: string; const X: TDoubleArray; const Y: TDoubleArray; const Errors: TDoubleArray);

implementation

uses
  formatutils, sortutils;

const
  defNameDirective = '#NAME=';

const
  defExplicitDelimiters = [^I, ',', ';'];

procedure SplitSpecial(const S: string; out Fields: TStringArray; MaxFields: Integer);

  procedure AddField(const Field: string);
  begin
    SetLength(Fields, Length(Fields) + 1);
    Fields[Length(Fields) - 1] := Field;
  end;

var
  Field: String;
  InQuotes: Boolean;
  Len: Integer;
  P: Integer;
begin
  Fields := nil;
  InQuotes := False;
  Len := Length(S);
  Field := '';
  P := 1;
  if (P <= Len) and (S[P] = '"') then begin
    InQuotes := True;
    Inc(P);
  end;
  while P <= Len do begin
    if InQuotes then begin
      if P = Len then begin
        if S[P] = '"' then begin
          Inc(P);
          InQuotes := False;
        end
        else begin
          Field := Field + S[P];
          Inc(P);
  	end;
      end
      else begin
        if (S[P] = '"') and (S[P + 1] = '"') then begin
          Field := Field + '"';
          Inc(P, 2);
        end
        else
        if S[P] = '"' then begin
          Inc(P);
          InQuotes := False;
        end
        else begin
          Field := Field + S[P];
          Inc(P);
  	end
      end
    end
    else begin
      if S[P] in ([' '] + defExplicitDelimiters) then begin
        AddField(Field);
        if (MaxFields > 0) and (Length(Fields) > MaxFields) then
          Exit;
        Field := '';
        while (P <= Len) and (S[P] = ' ') do
          Inc(P);
        if (P <= Len) and (S[P] in defExplicitDelimiters) then
          Inc(P);
        while (P <= Len) and (S[P] = ' ') do
          Inc(P);
        if (P <= Len) and (S[P] = '"') then begin
          InQuotes := True;
          Inc(P);
  	end;
      end
      else begin
        Field := Field + S[P];
        Inc(P);
      end;
    end;
  end;
  AddField(Field);
end;

// Reads a text file assuming it contains columns of the floating-point values.
// Only the first three columns are taken into account.
// The empty lines and lines starting with '#' are ignored.
// The lines contaning less than 2 floating-point values in the first two
// columns are ignored.
// The columns are assumed to be separated by tabs, commas, semicolons, or spaces.
// The data is sorted by X
procedure ReadData(const AFileName: string; out X: TDoubleArray; out Y: TDoubleArray; out Errors: TDoubleArray; out ObjectName: string);
var
  Lines: TStrings;
  Fields: TStringArray;
  S: string;
  FX, FY, Error: Double;
  I, N: Integer;
begin
  ObjectName := ChangeFileExt(ExtractFileName(AFileName), '');
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    SetLength(X, Lines.Count);
    SetLength(Y, Lines.Count);
    SetLength(Errors, Lines.Count);
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
      SplitSpecial(S, Fields, 0);
      if Length(Fields) > 1 then begin
        if StringToFloatLocaleIndependent(Fields[0], FX) and StringToFloatLocaleIndependent(Fields[1], FY) then begin
          X[N] := FX;
          Y[N] := FY;
          Errors[N] := 0;
          Inc(N);
        end;
      end;
      if Length(Fields) > 2 then begin
        if StringToFloatLocaleIndependent(Fields[2], Error) then
          Errors[N - 1] := Error;
      end;
    end;
    SetLength(X, N);
    SetLength(Y, N);
    SetLength(Errors, N);
    // Sort the data
    SortDataPoints(X, Y, Errors);
  finally
    FreeAndNil(Lines);
  end;
end;

procedure WriteData(const AFileName: string; const X: TDoubleArray; const Y: TDoubleArray; const Errors: TDoubleArray);
var
  Delim: Char;
  FileExt: string;
  Lines: TStrings;
  S: string;
  I: Integer;
begin
  FileExt := ExtractFileExt(AFileName);
  if SameText(FileExt, '.CSV') then
    Delim := ','
  else
  if SameText(FileExt, '.TSV') then
    Delim := ^I
  else
    Delim := ' ';
  Lines := TStringList.Create;
  try
    for I := 0 to Length(X) - 1 do begin
      S := FloatToStrLocaleIndependent(X[I]) + Delim + FloatToStrLocaleIndependent(Y[I]);
      if Errors <> nil then
        S := S + Delim + FloatToStrLocaleIndependent(Errors[I]);
      Lines.Add(S);
    end;
    Lines.SaveToFile(AFileName);
  finally
    FreeAndNil(Lines);
  end;
end;

end.


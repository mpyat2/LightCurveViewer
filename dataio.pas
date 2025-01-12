unit dataio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, typ, csvdocument;

type
  TFloatArray = array of ArbFloat;

procedure ReadCSV(const AFileName: string; out X: TFloatArray; out Y: TFloatArray; Delimiter: Char = #09);

implementation

function StringToFloat(const S: string; out F: ArbFloat): Boolean;
var
  Code: Integer;
begin
  Val(S, F, Code);
  Result := Code = 0;
end;

procedure ReadCSV(const AFileName: string; out X: TFloatArray; out Y: TFloatArray; Delimiter: Char = #09);
var
  CSVDoc: TCSVDocument;
  FX, FY: ArbFloat;
  I, N: Integer;
begin
  CSVDoc := TCSVDocument.Create;
  try
    CSVDoc.Delimiter := Delimiter;
    CSVDoc.LoadFromFile(AFileName);
    SetLength(X, CSVDoc.RowCount);
    SetLength(Y, CSVDoc.RowCount);
    N := 0;
    for I := 0 to CSVDoc.RowCount - 1 do begin
      if StringToFloat(CSVDoc.Cells[0, I], FX) and StringToFloat(CSVDoc.Cells[1, I], FY) then begin
        X[I] := FX;
        Y[I] := FY;
        Inc(N);
      end;
    end;
    SetLength(X, N);
    SetLength(Y, N);
  finally
    FreeAndNil(CSVDoc);
  end;
end;

end.


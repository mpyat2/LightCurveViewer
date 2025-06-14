unit edithelper;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, StdCtrls;

type

  { TEditHelper }

  TEditHelper = class helper for TEdit
    function Evaluate: Double;
    function EvaluateInt(out V: Integer): Boolean;
  end;

implementation

uses
  math, fpexprpars;

function TEditHelper.Evaluate: Double;
var
  FParser: TFPExpressionParser;
  ParserResult: TFPExpressionResult;
  S: string;
begin
  Result := NaN;
  S := Trim(self.Text);
  if S = '' then
    Exit;
  if FormatSettings.DecimalSeparator <> '.' then
    S := StringReplace(S, FormatSettings.DecimalSeparator, '.', [rfReplaceAll]);
  FParser := TFpExpressionParser.Create(nil);
  try
    FParser.BuiltIns := [];
    try
      FParser.Expression := S;
      ParserResult := FParser.Evaluate;
      if ParserResult.ResultType = rtFloat then
        Result := ParserResult.ResFloat
      else
      if ParserResult.ResultType = rtInteger then
        Result := ParserResult.resInteger
      else
      if ParserResult.ResultType = rtCurrency then
        Result := ParserResult.resCurrency
      else
        Result := NaN;
    except
      Result := NaN;
    end;
  finally
    FreeAndNil(FParser);
  end;
end;

function TEditHelper.EvaluateInt(out V: Integer): Boolean;
var
  FParser: TFPExpressionParser;
  ParserResult: TFPExpressionResult;
  S: string;
begin
  Result := False;
  V := 0;
  S := Trim(self.Text);
  if S = '' then
    Exit;
  FParser := TFpExpressionParser.Create(nil);
  try
    try
      FParser.Expression := S;
      ParserResult := FParser.Evaluate;
      if ParserResult.ResultType = rtInteger then begin
        V := ParserResult.resInteger;
        Result := True;
      end;
    except
      Result := False;
      V := 0;
    end;
  finally
    FreeAndNil(FParser);
  end;
end;


end.


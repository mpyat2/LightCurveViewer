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
begin
  Result := NaN;
  FParser := TFpExpressionParser.Create(nil);
  try
    try
      FParser.Expression := Trim(self.Text);
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
begin
  Result := False;
  V := 0;
  FParser := TFpExpressionParser.Create(nil);
  try
    try
      FParser.Expression := Trim(self.Text);
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


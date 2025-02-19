unit miscutils;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, lcvtypes;

procedure CalcError(const S: string);

function GetFPUexceptionAsString: string;

function GetMedianInterval(const X: TDoubleArray): Double;

function GetRecommendedFrequencyResolution(Xmin, Xmax: Double; TrigPolyDegree: Integer): Double;

implementation

uses
  math, sortutils;

procedure CalcError(const S: string);
begin
  raise Exception.Create(S);
end;


function FPUexceptionToString(FPUexception: TFPUException): string;
begin
  case FPUexception of
    exInvalidOp:
      Result := 'InvalidOp';
    exDenormalized:
      Result := 'Denormalized';
    exZeroDivide:
      Result := 'ZeroDivide';
    exOverflow:
      Result := 'Overflow';
    exUnderflow:
      Result := 'Underflow';
    exPrecision:
      Result := 'Precision';
  else
    Result := 'Unknown';
  end;
end;

function GetFPUexceptionAsString: string;
var
  FPUExceptionMask: TFPUExceptionMask;
  FPUexception: TFPUException;
begin
  FPUExceptionMask := GetExceptionMask;
  Result := '';
  for FPUexception := low(TFPUExceptionMask) to high(TFPUExceptionMask) do begin
    if FPUexception in FPUExceptionMask then
      Result := Result + FPUexceptionToString(FPUexception) + ' ';
  end;
end;

function GetMedianInterval(const X: TDoubleArray): Double;
var
  tempArray, Intervals: TDoubleArray;
  Interval: Double;
  I, N: Integer;
begin
  Result := NaN;
  if Length(X) < 1 then
    Exit;
  SetLength(tempArray, Length(X));
  for I := 0 to Length(tempArray) - 1 do
    tempArray[I] := X[I];
  SortFloatArray(tempArray);
  SetLength(Intervals, Length(tempArray) - 1);
  N := 0;
  for I := 1 to Length(tempArray) - 1 do begin
    Interval := tempArray[I] - tempArray[I - 1];
    if Interval > 0 then begin // actually, <> 0, because the array is sorted
      Intervals[N] := Interval;
      Inc(N);
    end;
  end;
  SetLength(Intervals, N);
  if Length(Intervals) < 1 then
    Exit;
  SortFloatArray(Intervals);
  // Median
  I := Length(Intervals) div 2; // Central element
  if Odd(Length(Intervals)) then
    Result := Intervals[I]
  else
    Result := (Intervals[I - 1] + Intervals[I]) / 2;
end;

function GetRecommendedFrequencyResolution(Xmin, Xmax: Double; TrigPolyDegree: Integer): Double;
begin
  if (Xmax > Xmin) and (TrigPolyDegree > 0) then
    Result := 0.05 / (Xmax - Xmin) / TrigPolyDegree
  else
    Result := NaN;
end;

end.


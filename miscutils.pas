unit miscutils;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, lcvtypes;

type

  { SleglsException }

  SleglsException = class(Exception)
  private
    FTerm: Integer;
  public
    constructor Create(const Msg : string; ATerm: Integer);
    property Term: Integer read FTerm;
  end;

procedure SleglsError(const Msg: string; Term: Integer);

procedure CalcError(const S: string);

function GetFPUexceptionAsString: string;

// WARNING!! The input array is changed.
function WirthMedian(var data: TDoubleArray): Double;

function GetMedianInterval(const X: TDoubleArray): Double;

function GetRecommendedFrequencyResolution(Xmin, Xmax: Double; TrigPolyDegree: Integer): Double;

function GetNofFrequencies(lowfreq, hifreq: Double; freq_step: Double): Integer;

function CalculateCycle(T, Period, Epoch: Double): Int64; inline;

function CalculatePhase(T, Period, Epoch: Double): Double; inline;

function GetLogicalCpuCount: Integer;

{$IFDEF Linux}
procedure OpenURLasync(const URL: string);
{$ENDIF}

implementation

uses
{$IF defined(windows)}
  Windows,
{$ELSEIF defined(linux)}
  ctypes,
  Process,
{$ENDIF}
  math, sortutils;

{ SleglsException }

constructor SleglsException.Create(const Msg: string; ATerm: Integer);
begin
  inherited Create(Msg);
  FTerm := ATerm;
end;

procedure SleglsError(const Msg: string; Term: Integer);
begin
  raise SleglsException.Create(Msg, Term);
end;

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

// from: N.Wirth. "Algorithms and Data Structures. Oberon version": "2.3.4 Finding the Median"
// Translated from Oberon (with 1 fix!)
// See also http://ndevilla.free.fr/median/median/src/wirth.c
function WirthFind(var a: TDoubleArray; k: SizeInt; n: SizeInt): Double;
var
 L, R, i, j: SizeInt;
 w, x: Double;
begin
  L := 0; R := n - 1;
  while L < R do begin
    x := a[k]; i := L; j := R;
    repeat
      while a[i] < x do Inc(i);
      while x < a[j] do Dec(j);
      if i <= j then begin
        w := a[i]; a[i] := a[j]; a[j] := w;
        Inc(i); Dec(j);
      end;
    until i > j;
    if j < k then L := i;
    if k < i then R := j;
  end;
  Result := a[k];
end;

// 'data' changed but not sorted completely!
function WirthMedian(var data: TDoubleArray): Double;
var
  k, n: SizeInt;
begin
  Result := 0;
  n := Length(data);
  if n < 1 then Exit;
  if n = 1 then begin
    Result := data[0];
    Exit;
  end;
  if n = 2 then begin
    Result := data[0];
    Result := (Result + data[1]) / 2; // to prevent integer overflow
    Exit;
  end;
  k := n div 2;
  Result := WirthFind(data, k, n);
  if Odd(n) then
    Exit;
  Result := (Result + WirthFind(data, k - 1, n)) / 2;
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

function GetNofFrequencies(lowfreq, hifreq: Double; freq_step: Double): Integer;
begin
  Result := Trunc((hifreq - lowfreq) / freq_step) + 1;
end;

function CalculateCycle(T, Period, Epoch: Double): Int64; inline;
begin
  Result := Floor64((T - Epoch) / Period);
end;

function CalculatePhase(T, Period, Epoch: Double): Double; inline;
var
  Cycle: Int64;
begin
  Cycle := CalculateCycle(T, Period, Epoch);
  Result := (T - Epoch - Period * Cycle) / Period;
end;

// http://wiki.freepascal.org/Example_of_multi-threaded_application:_array_of_threads

{$IFDEF Linux}
const _SC_NPROCESSORS_ONLN = 83;
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
{$ENDIF}

function GetLogicalCpuCount1: Integer;
// returns a good default for the number of threads on this system
{$IF defined(windows)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  i: Integer;
  ProcessAffinityMask, SystemAffinityMask: DWORD_PTR;
  Mask: DWORD;
  SystemInfo: SYSTEM_INFO;
begin
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask) then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := DWord(1) shl i;
      if (ProcessAffinityMask and Mask) <> 0 then
        inc(Result);
    end;
  end
  else begin
    //can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(SystemInfo);
    Result := SystemInfo.dwNumberOfProcessors;
  end;
end;
{$ELSEIF defined(linux)}
begin
  Result:=sysconf(_SC_NPROCESSORS_ONLN);
end;
{$ELSE}
begin
  Result:=1;
end;
{$ENDIF}

function GetLogicalCpuCount: Integer;
begin
  Result := Min(32, GetLogicalCpuCount1);
end;

{$IFDEF Linux}
procedure OpenURLasync(const URL: string);
var
  P: TProcess;
begin
  P := TProcess.Create(nil);
  try
    P.Executable := 'xdg-open';
    P.Parameters.Add(URL);
    P.Options := [poNoConsole];
    P.Execute;
  finally
    FreeAndNil(P);
  end;
end;
{$ENDIF}

end.


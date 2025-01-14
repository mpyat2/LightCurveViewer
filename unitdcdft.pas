unit unitdcdft;

{$mode ObjFPC}{$H+}
//{$R+}

interface

uses
  Windows, Classes, SysUtils, math, typ, sle, common;

type
  TFloat11 = array[0..10] of ArbFloat;
  TFloat11Array = array of TFloat11;

type
  PDCDFTparameters = ^TDCDFTparameters;
  TDCDFTparameters = record
    X: TFloatArray;
    Y: TFloatArray;
    FrequencyMin: Double;
    FrequencyMax: Double;
    FrequencyResolution: Double;
    TrigPolyDegree: Integer;
    frequencies: TFloatArray;
    periods: TFloatArray;
    power: TFloatArray;
    Error: string;
  end;

procedure dcdft_proc(
          const t, mag: TFloatArray;
          lowfreq, hifreq: ArbFloat;
          freq_step: ArbFloat;
          TrigPolyDegree: ArbInt;
          CmdLineNumberOfThreads: Integer;
          out frequencies, periods, power: TFloatArray);

procedure SetGlobalTerminateAllThreads(AValue: Boolean);

implementation

var
  GlobalTerminateAllThreads: Boolean = False;

procedure SetGlobalTerminateAllThreads(AValue: Boolean);
begin
  GlobalTerminateAllThreads := AValue;
end;

procedure CalcError(const S: string);
begin
  raise Exception.Create(S);
end;

// http://wiki.freepascal.org/Example_of_multi-threaded_application:_array_of_threads
function GetLogicalCpuCount: integer;
// returns a good default for the number of threads on this system
// Windows only!
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

type
  TCalcThread = class(TThread)
  private
    Ft, Fmag: TFloatArray;
    FThreadNo: Integer;
    Flowfreq: ArbFloat;
    Ffreq_step: ArbFloat;
    Fn_freq: ArbInt;
    FTrigPolyDegree: ArbInt;
    Fpartial_frequencies, Fpartial_periods, Fpartial_power: TFloatArray;
    FExecuteCompleted: Boolean;
  private
    function GetFreq(I: ArbInt): ArbFloat;
    function GetPeriod(I: ArbInt): ArbFloat;
    function GetPower(I: ArbInt): ArbFloat;
  public
    property AThreadNo: Integer read FThreadNo;
    property ExecuteCompleted: Boolean read FExecuteCompleted;
    property An_freq: ArbInt read Fn_freq;
    property Freq[I: ArbInt]: ArbFloat read GetFreq;
    property Period[I: ArbInt]: ArbFloat read GetPeriod;
    property Power[I: ArbInt]: ArbFloat read GetPower;
  private
    procedure dcdft_proc_1;
  protected
    procedure Execute; override;
  public
    constructor Create(ThreadNo: Integer;
                       const t, mag: TFloatArray;
                       lowfreq: ArbFloat;
                       freq_step: ArbFloat;
                       n_freq: ArbInt;
                       trigPolyDegree: ArbInt);
  end;

function TCalcThread.GetFreq(I: ArbInt): ArbFloat;
begin
  Result := Fpartial_frequencies[I];
end;

function TCalcThread.GetPeriod(I: ArbInt): ArbFloat;
begin
  Result := Fpartial_periods[I];
end;

function TCalcThread.GetPower(I: ArbInt): ArbFloat;
begin
  Result := Fpartial_power[I];
end;

constructor TCalcThread.Create(ThreadNo: Integer;
                               const t, mag: TFloatArray;
                               lowfreq: ArbFloat;
                               freq_step: ArbFloat;
                               n_freq: ArbInt;
                               trigPolyDegree: ArbInt);
begin
  inherited Create(True);
  FreeOnTerminate := False;  // after inherited Create!
  FThreadNo := ThreadNo;
  Ft := t;
  Fmag := mag;
  Flowfreq := lowfreq;
  Ffreq_step := freq_step;
  Fn_freq := n_freq;
  FTrigPolyDegree := trigPolyDegree;
  SetLength(Fpartial_frequencies, Fn_freq);
  SetLength(Fpartial_periods, Fn_freq);
  SetLength(Fpartial_power, Fn_freq);
  FillChar(Fpartial_frequencies[0], Fn_freq * SizeOf(ArbFloat), 0);
  FillChar(Fpartial_periods[0], Fn_freq * SizeOf(ArbFloat), 0);
  FillChar(Fpartial_power[0], Fn_freq * SizeOf(ArbFloat), 0);
  FExecuteCompleted := False;
end;

procedure TCalcThread.Execute;
begin
  try
    dcdft_proc_1;
  except
    on E: Exception do begin
      GlobalTerminateAllThreads := True;
      OutputDebugString(PChar('Thread #' + IntToStr(FThreadNo) + ' raised an error!'));
      raise;
    end;
  end;
end;

procedure TCalcThread.dcdft_proc_1;
var
  ndata: ArbInt;
  nu: ArbFloat;
  angle: ArbFloat;
  term: ArbInt;
  pwr: ArbFloat;
  I, II, III, Idx: ArbInt;
  a: TFloat11Array;
  x: TFloat11;
  fittedValues: TFloatArray;
begin
  if (FTrigPolyDegree < 1) or (FTrigPolyDegree > 5) then
    CalcError('Polynomial degree must be in the range 1..5');

  ndata := Length(Ft);

  SetLength(a, ndata);
  SetLength(fittedValues, ndata);

  // Constant
  for II := 0 to ndata - 1 do begin
    a[II][0] := 1;
  end;

  nu := Flowfreq;
  for I := 0 to Fn_freq - 1 do begin
    if Terminated or GlobalTerminateAllThreads then begin
      OutputDebugString(PChar('Thread #' + IntToStr(FThreadNo) + ' terminated!'));
      Exit;
    end;

    if nu > 0.0 then begin
      Fpartial_frequencies[I] := nu;
      Fpartial_periods[I] := 1 / nu;
      // Calculate cos and sin (c1 and s1) of the time-sequence for the trial frequency
      for II := 0 to ndata - 1 do begin
        angle := 2 * Pi * nu * Ft[II];
        for III := 1 to FTrigPolyDegree do begin
          Idx := 2 * (III - 1) + 1;
          a[II][Idx]     := Cos(III * angle);
          a[II][Idx + 1] := Sin(III * angle);
        end;
      end;
      // Fit to the signal (mag)
      slegls(a[0, 0], ndata, 1 + FTrigPolyDegree * 2, 11, Fmag[0], x[0], term);
      if term <> 1 then
        raise Exception.Create('"slegls" error: ' + IntToStr(term));
      // Get the power of the trial frequency
      for II := 0 to ndata - 1 do begin
        fittedvalues[II] := 0.0; // no constant
        for III := 1 to FTrigPolyDegree do begin
          Idx := 2 * (III - 1) + 1;
          fittedvalues[II] := fittedvalues[II] + x[Idx] * a[II][Idx] + x[Idx + 1] * a[II][Idx + 1];
        end;
      end;
      pwr := PopnVariance(fittedvalues);
      Fpartial_power[I] := pwr;
    end
    else begin
      Fpartial_periods[I] := NaN;
      Fpartial_power[I] := NaN;
    end;
    nu := nu + Ffreq_step;
  end;
  FExecuteCompleted := True;
end;

procedure dcdft_proc(
          const t, mag: TFloatArray;
          lowfreq, hifreq: ArbFloat;
          freq_step: ArbFloat;
          TrigPolyDegree: ArbInt;
          CmdLineNumberOfThreads: Integer;
          out frequencies, periods, power: TFloatArray);
var
  n_freq: ArbInt;
  ndata: ArbInt;
  mag_var: ArbFloat;
  startfreq: ArbFloat;
  NumberOfThreads, StepsPerThread, Remainder, StepsToDo: integer;
  I, II, Idx: Integer;
  Threads: array of TCalcThread;
begin
  n_freq := Floor((hifreq - lowfreq) / freq_step);

  SetLength(frequencies, n_freq + 1);
  SetLength(periods, n_freq + 1);
  SetLength(power, n_freq + 1);

  if CmdLineNumberOfThreads < 1 then
    NumberOfThreads := GetLogicalCpuCount()
  else
    NumberOfThreads := CmdLineNumberOfThreads;

  OutputDebugString(PChar('Number of threads: ' + IntToStr(NumberOfThreads)));

  StepsPerThread := (n_freq + 1) div NumberOfThreads;
  Remainder := (n_freq + 1) - StepsPerThread * NumberOfThreads;

  GlobalTerminateAllThreads := False;
  SetLength(Threads, NumberOfThreads);
  for I := 0 to Length(Threads) - 1 do Threads[I] := nil; // not nesessary, already initialized

  try
    for I := 0 to NumberOfThreads - 1 do begin
      startfreq := lowfreq + StepsPerThread * I * freq_step;
      StepsToDo := StepsPerThread;
      if I = NumberOfThreads - 1 then
        StepsToDo := StepsToDo + Remainder;
      OutputDebugString(PChar('Thread ' + IntToStr(I) + '; steps: ' + IntToStr(StepsToDo)));
      Threads[I] := TCalcThread.Create(I, t, mag, startfreq, freq_step, StepsToDo, TrigPolyDegree);
      // check for exception while creation (see FPC docs)
      if Assigned(Threads[I].FatalException) then begin
        if Assigned(Threads[I].FatalException) then begin
          if Threads[I].FatalException is Exception then
            CalcError(Exception(Threads[I].FatalException).Message)
          else
            CalcError('Unknown Exception');
        end;
      end;
    end;

    for I := 0 to NumberOfThreads - 1 do
      Threads[I].Start;

    for I := 0 to NumberOfThreads - 1 do
      Threads[I].WaitFor;

    for I := 0 to NumberOfThreads - 1 do begin
      if Assigned(Threads[I].FatalException) then begin
        if Threads[I].FatalException is Exception then
          CalcError(Exception(Threads[I].FatalException).Message)
        else
          CalcError('Unknown Exception');
      end;
    end;

    if GlobalTerminateAllThreads then begin
      Abort;
    end;

    for I := 0 to NumberOfThreads - 1 do begin
      if not Threads[I].ExecuteCompleted then
         CalcError('Unknown Error: not all threads are completed.');
    end;

    for I := 0 to NumberOfThreads - 1 do begin
      StepsToDo := Threads[I].An_freq;
      for II := 0 to StepsToDo - 1 do begin
         Idx := StepsPerThread * I + II;
         frequencies[Idx] := Threads[I].Freq[II];
         periods[Idx] := Threads[I].Period[II];
         power[Idx] := Threads[I].Power[II];
      end;
    end;

    ndata := Length(t);
    mag_var := PopnVariance(mag);

    for I := 0 to n_freq do begin
      power[I] := power[I] / mag_var;
    end;

  finally
    for I := Length(Threads) - 1 downto 0 do begin
       FreeAndNil(Threads[I]);
    end;
  end;
end;

end.


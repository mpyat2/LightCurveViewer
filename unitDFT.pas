unit unitDFT;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Windows, Classes, SysUtils, math, common, DoLongOp;

type
  PDCDFTparameters = ^TDCDFTparameters;
  TDCDFTparameters = record
    X: TFloatArray;
    Y: TFloatArray;
    FrequencyMin: Double;
    FrequencyMax: Double;
    FrequencyResolution: Double;
    TrendDegree: Integer;
    TrigPolyDegree: Integer;
    frequencies: TFloatArray;
    periods: TFloatArray;
    power: TFloatArray;
    Error: string;
  end;

procedure dcdft_proc(
          const t, mag: TFloatArray;
          lowfreq, hifreq: Double;
          freq_step: Double;
          TrendDegree: Integer;
          TrigPolyDegree: Integer;
          CmdLineNumberOfThreads: Integer;
          ProgressCaptionProc: TProgressCaptionProc;
          out frequencies, periods, power: TFloatArray);

procedure SetGlobalTerminateAllThreads(AValue: Boolean);

implementation

const
  GlobalCounterIncrement = 1000;

var
  GlobalTerminateAllThreads: Boolean = False;
  GlobalCounter: Integer;
  GlobalCounterCriticalSection: TCriticalSection;

function IncrementGlobalCounter(N: Integer): Integer;
begin
  EnterCriticalSection(GlobalCounterCriticalSection);
  try
    GlobalCounter := GlobalCounter + N;
    Result := GlobalCounter;
  finally
    LeaveCriticalSection(GlobalCounterCriticalSection);
  end;
end;

procedure SetGlobalTerminateAllThreads(AValue: Boolean);
begin
  GlobalTerminateAllThreads := AValue;
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

  { TCalcThread }

  TCalcThread = class(TThread)
  private
    Ft, Fmag: TFloatArray;
    FThreadNo: Integer;
    Flowfreq: Double;
    Ffreq_step: Double;
    Fn_freq: Integer;
    FTrendDegree: Integer;
    FTrigPolyDegree: Integer;
    Fpartial_frequencies, Fpartial_periods, Fpartial_power: TFloatArray;
    FExecuteCompleted: Boolean;
    FTotalNfreq: Integer;
    FProgressCaptionProc: TProgressCaptionProc;
    FInfoMessage: string;
  private
    function GetFreq(I: Integer): Double;
    function GetPeriod(I: Integer): Double;
    function GetPower(I: Integer): Double;
  public
    property AThreadNo: Integer read FThreadNo;
    property ExecuteCompleted: Boolean read FExecuteCompleted;
    property An_freq: Integer read Fn_freq;
    property Freq[I: Integer]: Double read GetFreq;
    property Period[I: Integer]: Double read GetPeriod;
    property Power[I: Integer]: Double read GetPower;
  private
    procedure InfoMessageProc;
    procedure dcdft_proc_1;
  protected
    procedure Execute; override;
  public
    constructor Create(ThreadNo: Integer;
                       const t, mag: TFloatArray;
                       lowfreq: Double;
                       freq_step: Double;
                       n_freq: Integer;
                       trendDegree: Integer;
                       trigPolyDegree: Integer;
                       TotalNfreq: Integer;
                       ProgressCaptionProc: TProgressCaptionProc);
  end;

function TCalcThread.GetFreq(I: Integer): Double;
begin
  Result := Fpartial_frequencies[I];
end;

function TCalcThread.GetPeriod(I: Integer): Double;
begin
  Result := Fpartial_periods[I];
end;

function TCalcThread.GetPower(I: Integer): Double;
begin
  Result := Fpartial_power[I];
end;

constructor TCalcThread.Create(ThreadNo: Integer;
                               const t, mag: TFloatArray;
                               lowfreq: Double;
                               freq_step: Double;
                               n_freq: Integer;
                               trendDegree: Integer;
                               trigPolyDegree: Integer;
                               TotalNfreq: Integer;
                               ProgressCaptionProc: TProgressCaptionProc);
begin
  inherited Create(True);
  FreeOnTerminate := False;  // after inherited Create!
  FThreadNo := ThreadNo;
  Ft := t;
  Fmag := mag;
  Flowfreq := lowfreq;
  Ffreq_step := freq_step;
  Fn_freq := n_freq;
  FTrendDegree := trendDegree;
  FTrigPolyDegree := trigPolyDegree;
  SetLength(Fpartial_frequencies, Fn_freq);
  SetLength(Fpartial_periods, Fn_freq);
  SetLength(Fpartial_power, Fn_freq);
  FillChar(Fpartial_frequencies[0], Fn_freq * SizeOf(Double), 0);
  FillChar(Fpartial_periods[0], Fn_freq * SizeOf(Double), 0);
  FillChar(Fpartial_power[0], Fn_freq * SizeOf(Double), 0);
  FProgressCaptionProc := ProgressCaptionProc;
  FTotalNfreq := TotalNfreq;
  FExecuteCompleted := False;
end;

procedure TCalcThread.Execute;
begin
  try
    dcdft_proc_1;
  except
    on E: Exception do begin
      GlobalTerminateAllThreads := True;
      //OutputDebugString(PChar('Thread #' + IntToStr(FThreadNo) + ' raised an error!'));
      raise;
    end;
  end;
end;

procedure TCalcThread.InfoMessageProc;
begin
  if Assigned(FProgressCaptionProc) then
    FProgressCaptionProc(FInfoMessage);
end;

procedure TCalcThread.dcdft_proc_1;
var
  ndata: Integer;
  nu: Double;
  pwr: Double;
  I, II, III, Idx: Integer;
  meanTime: Double;
  times: TFloatArray;
  temp_mags: TFloatArray;
  fit: TFloatArray;
  N, Nrest: Integer;
  NofParameters: Integer;
  angle: Double;
  a: TFloatArray;
begin
  if Length(Ft) <> Length(FMag) then
    CalcError('X and Y arrays myst be of equal length');

  ndata := Length(Ft);
  SetLength(times, ndata);
  SetLength(temp_mags, ndata);
  meanTime := Mean(Ft);
  for II := 0 to ndata - 1 do begin
    times[II] := Ft[II] - meanTime;
  end;

  if (FTrendDegree < 0) then
    CalcError('Trend degree must be >= 0');

  if (FTrigPolyDegree < 0) then
    CalcError('Trigonometric polynomial degree must be >= 0');

  NofParameters := 1 + FTrendDegree + FTrigPolyDegree * 2;

  if NofParameters > 51 then
    CalcError('Too many parameters. Please reduce trend or trigonometric polynomial degree');

  SetLength(a, ndata * NofParameters);

  // Trend: one-time initialization
  for I := 0 to ndata - 1 do begin
    for II := 0 to FTrendDegree do begin
      Idx := I * NofParameters + II;
      a[Idx] := math.Power(times[I], II);
    end;
  end;

  SetLength(fit, ndata);

  Nrest := Fn_freq;
  nu := Flowfreq;
  for I := 0 to Fn_freq - 1 do begin
    if Terminated or GlobalTerminateAllThreads then begin
      //OutputDebugString(PChar('Thread #' + IntToStr(FThreadNo) + ' terminated!'));
      Exit;
    end;

    if nu > 0.0 then begin
      Fpartial_frequencies[I] := nu;
      Fpartial_periods[I] := 1 / nu;

      // Trigonometric polinomial
      for II := 0 to ndata - 1 do begin
        angle := 2 * Pi * nu * times[II];
        for III := 1 to FTrigPolyDegree do begin
          Idx := II * NofParameters + 1 + FTrendDegree + 2 * (III - 1);
          a[Idx]     := Cos(III * angle);
          a[Idx + 1] := Sin(III * angle);
        end;
      end;

      PolyFit(times, Fmag, FTrendDegree, FTrigPolyDegree, a, fit);

      for II := 0 to ndata - 1 do begin
        temp_mags[II] := Fmag[II] - fit[II];
      end;

      pwr := PopnVariance(temp_mags); // \sigma^2_{O-C}

      Fpartial_power[I] := pwr;

      if (I + 1) mod GlobalCounterIncrement = 0 then begin
        N := IncrementGlobalCounter(GlobalCounterIncrement);
        Nrest := Nrest - GlobalCounterIncrement;
        FInfoMessage := IntToStr(N) + ' / ' + IntToStr(FTotalNfreq);
        Synchronize(@InfoMessageProc);
      end;
    end
    else begin
      Fpartial_periods[I] := NaN;
      Fpartial_power[I] := NaN;
    end;
    nu := nu + Ffreq_step;
  end;

  N := IncrementGlobalCounter(Nrest);
  FInfoMessage := IntToStr(N) + ' / ' + IntToStr(FTotalNfreq);
  Synchronize(@InfoMessageProc);

  FExecuteCompleted := True;
  //OutputDebugString(PChar('Thread #' + IntToStr(FThreadNo) + ' finished!'));
end;

procedure dcdft_proc(
          const t, mag: TFloatArray;
          lowfreq, hifreq: Double;
          freq_step: Double;
          TrendDegree: Integer;
          TrigPolyDegree: Integer;
          CmdLineNumberOfThreads: Integer;
          ProgressCaptionProc: TProgressCaptionProc;
          out frequencies, periods, power: TFloatArray);
var
  n_freq: Integer;
  ndata: Integer;
  a: TFloatArray;
  meanTime: Double;
  times: TFloatArray;
  fit: TFloatArray;
  sigmaSquaredO: Double;
  startfreq: Double;
  temp_mags: TFloatArray;
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

  //OutputDebugString(PChar('Number of threads: ' + IntToStr(NumberOfThreads)));

  StepsPerThread := (n_freq + 1) div NumberOfThreads;
  Remainder := (n_freq + 1) - StepsPerThread * NumberOfThreads;

  GlobalTerminateAllThreads := False;
  SetLength(Threads, NumberOfThreads);
  for I := 0 to Length(Threads) - 1 do Threads[I] := nil; // not nesessary, already initialized

  GlobalCounter := 0;
  InitCriticalSection(GlobalCounterCriticalSection);
  try
    try
      for I := 0 to NumberOfThreads - 1 do begin
        startfreq := lowfreq + StepsPerThread * I * freq_step;
        StepsToDo := StepsPerThread;
        if I = NumberOfThreads - 1 then
          StepsToDo := StepsToDo + Remainder;
        //OutputDebugString(PChar('Thread ' + IntToStr(I) + '; steps: ' + IntToStr(StepsToDo)));
        Threads[I] := TCalcThread.Create(I, t, mag, startfreq, freq_step, StepsToDo, TrendDegree, TrigPolyDegree, n_freq + 1, ProgressCaptionProc);
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

      // Trend
      ndata := Length(mag);
      SetLength(times, ndata);
      SetLength(temp_mags, ndata);
      meanTime := Mean(t);
      for I := 0 to ndata - 1 do begin
        times[I] := t[I] - meanTime;
      end;
      SetLength(a, ndata * (1 + TrendDegree));
      for I := 0 to ndata - 1 do begin
        for II := 0 to TrendDegree do begin
          Idx := I * (1 + TrendDegree) + II;
          a[Idx] := math.Power(times[I], II);
        end;
      end;

      SetLength(fit, ndata);
      PolyFit(t, mag, TrendDegree, 0, a, fit);

      for I := 0 to ndata - 1 do begin
        temp_mags[I] := mag[I] - fit[I];
      end;

      sigmaSquaredO := PopnVariance(temp_mags);

      for I := 0 to n_freq do begin
        power[I] := 1.0 - power[I] / sigmaSquaredO;
      end;

    finally
      for I := Length(Threads) - 1 downto 0 do begin
         FreeAndNil(Threads[I]);
      end;
    end;
  finally
    DoneCriticalSection(GlobalCounterCriticalSection);
  end;
end;

end.


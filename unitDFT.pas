unit unitDFT;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, math, lcvtypes;

{$IFDEF WIN64}
procedure sincos(var a: Double; var s: Double; var c: Double); cdecl; external 'sincos.dll' name 'sincos_';
{$ENDIF}

type
  //PDCDFTparameters = ^TDCDFTparameters;
  TDCDFTparameters = record
    X: TDoubleArray;
    Y: TDoubleArray;
    FrequencyMin: Double;
    FrequencyMax: Double;
    FrequencyResolution: Double;
    TrendDegree: Integer;
    TrigPolyDegree: Integer;
    frequencies: TDoubleArray;
    power: TDoubleArray;
    StartTime: Double;
    Error: string;
  end;

procedure dcdft_proc(
          const t, mag: TDoubleArray;
          lowfreq, hifreq: Double;
          freq_step: Double;
          TrendDegree: Integer;
          TrigPolyDegree: Integer;
          CmdLineNumberOfThreads: Integer;
          ProgressCaptionProc: TProgressCaptionProc;
          out frequencies, power: TDoubleArray);

procedure SetGlobalTerminateAllThreads(AValue: Boolean);

implementation

uses
  miscutils, fitproc;

const
  GlobalCounterIncrement = 100;

var
  GlobalTerminateAllThreads: Boolean = False;
  GlobalCounter: Integer;
  GlobalCounterCriticalSection: TRTLCriticalSection;

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

type

  { TCalcThread }

  TCalcThread = class(TThread)
  private
    Ft, Fmag: TDoubleArray;
    FThreadNo: Integer;
    Flowfreq: Double;
    Ffreq_step: Double;
    Fn_freq: Integer;
    FTrendDegree: Integer;
    FTrigPolyDegree: Integer;
    Fpartial_frequencies, Fpartial_power: TDoubleArray;
    FExecuteCompleted: Boolean;
    FTotalNfreq: Integer;
    FProgressCaptionProc: TProgressCaptionProc;
    FInfoMessage: string;
  private
    function GetFreq(I: Integer): Double;
    function GetPower(I: Integer): Double;
  public
    property AThreadNo: Integer read FThreadNo;
    property ExecuteCompleted: Boolean read FExecuteCompleted;
    property An_freq: Integer read Fn_freq;
    property Freq[I: Integer]: Double read GetFreq;
    property Power[I: Integer]: Double read GetPower;
  private
    procedure InfoMessageProc;
    procedure dcdft_proc_1;
  protected
    procedure Execute; override;
  public
    constructor Create(ThreadNo: Integer;
                       const t, mag: TDoubleArray;
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

function TCalcThread.GetPower(I: Integer): Double;
begin
  Result := Fpartial_power[I];
end;

constructor TCalcThread.Create(ThreadNo: Integer;
                               const t, mag: TDoubleArray;
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

  if n_freq < 1 then
    CalcError('Nothing to do!');

  Ft := t;
  Fmag := mag;
  Flowfreq := lowfreq;
  Ffreq_step := freq_step;
  Fn_freq := n_freq;
  FTrendDegree := trendDegree;
  FTrigPolyDegree := trigPolyDegree;
  SetLength(Fpartial_frequencies, Fn_freq);
  SetLength(Fpartial_power, Fn_freq);
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
  I, II, III, Idx: Integer;
  meanTime: Double;
  times: TDoubleArray;
  temp_mags: TDoubleArray;
  N, Nrest: Integer;
  NofParameters: Integer;
  angle: Double;
  a_param: Double;
  //tanAd2, tanAd2squared: Double;
  //TArbFloatArray for compatibility with NumLib
  a: TArbFloatArray;
  magArbFloatArray: TArbFloatArray;
  fit: TArbFloatArray;
  fit_calculated: Boolean;
begin
  if Length(Ft) <> Length(FMag) then
    CalcError('X and Y arrays myst be of equal length');

  if (FTrendDegree < 0) then
    CalcError('Trend degree must be >= 0');

  if (FTrigPolyDegree < 0) then
    CalcError('Trigonometric polynomial degree must be >= 0');

  NofParameters := 1 + FTrendDegree + FTrigPolyDegree * 2;

  if NofParameters > 51 then
    CalcError('Too many parameters. Please reduce trend or trigonometric polynomial degree');

  ndata := Length(Ft);
  SetLength(times, ndata);
  SetLength(temp_mags, ndata);
  SetLength(magArbFloatArray, ndata);
  meanTime := Mean(Ft);
  for I := 0 to ndata - 1 do begin
    times[I] := Ft[I] - meanTime;
    magArbFloatArray[I] := Fmag[I]; // for compatibility with NumLib
  end;

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
  for I := 0 to Fn_freq - 1 do begin
    if Terminated or GlobalTerminateAllThreads then begin
      //OutputDebugString(PChar('Thread #' + IntToStr(FThreadNo) + ' terminated!'));
      Exit;
    end;

    nu := Flowfreq + Ffreq_step * I;

    Fpartial_frequencies[I] := nu;
    fit_calculated := False;
    if nu > 0.0 then begin
      // Trigonometric polinomial
      for II := 0 to ndata - 1 do begin
        angle := 2 * Pi * nu * times[II];
        for III := 1 to FTrigPolyDegree do begin
          Idx := II * NofParameters + 1 + FTrendDegree + 2 * (III - 1);

          // V0: Sin, Cos
          //a[Idx]     := Cos(III * angle);
          //a[Idx + 1] := Sin(III * angle);
          // V1: Sin, Cos via Tan
          //tanAd2 := Math.tan(III * angle / 2.0);
          //tanAd2squared := tanAd2 * tanAd2;
          //a[Idx]     := ((1 - tanAd2squared) / (1 + tanAd2squared));
          //a[Idx + 1] := (2.0 * tanAd2 / (1 + tanAd2squared));
          // V3: math.sincos or Fortran77 version if DLL is included (Windows 64 only)
          a_param := III * angle; // all DLL function parameters are 'var'
          sincos(a_param, a[Idx + 1], a[Idx]);
        end;
      end;

      try
        PolyFit(a, magArbFloatArray, FTrendDegree, FTrigPolyDegree, fit);
        fit_calculated := True;
      except
        on ex: SleglsException do begin
          case ex.Term of
            2: begin
                 // there is no unambiguous solution
                 fit_calculated := False;
               end;
            else
              raise;
          end;
        end;
        on ex: DgelsException do begin
          if ex.Info > 0 then begin
            // Parameters are fine but the Least Squares solution could not be calculated.
            fit_calculated := False;
          end
          else // bad parameters
            raise;
        end
        else
          raise;
      end;
    end;
    if fit_calculated then begin
      for II := 0 to ndata - 1 do begin
        temp_mags[II] := magArbFloatArray[II] - fit[II];
      end;
      Fpartial_power[I] := PopnVariance(temp_mags); // \sigma^2_{O-C}
    end
    else
      Fpartial_power[I] := NaN;

    if (I + 1) mod GlobalCounterIncrement = 0 then begin
      N := IncrementGlobalCounter(GlobalCounterIncrement);
      Nrest := Nrest - GlobalCounterIncrement;
      FInfoMessage := IntToStr(N) + ' / ' + IntToStr(FTotalNfreq);
      Synchronize(@InfoMessageProc);
    end;
  end;

  N := IncrementGlobalCounter(Nrest);
  FInfoMessage := IntToStr(N) + ' / ' + IntToStr(FTotalNfreq);
  Synchronize(@InfoMessageProc);

  FExecuteCompleted := True;
  //OutputDebugString(PChar('Thread #' + IntToStr(FThreadNo) + ' finished!'));
end;

// For normalizing
function CalcSigmaSquaredO(const t, mag: TDoubleArray;
                           TrendDegree: Integer): Double;
var
  times: TDoubleArray;
  temp_mags: TDoubleArray;
  meanTime: Double;
  ndata: Integer;
  I, II, Idx: Integer;
  // For compatibility with NumLib
  a: TArbFloatArray;
  magArbFloatArray: TArbFloatArray;
  fit: TArbFloatArray;
  fit0: Double;
begin
  if TrendDegree < 0 then
    CalcError('CalcSigmaSquaredO: TrendDegree cannot be < 0');

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

  SetLength(magArbFloatArray, ndata);
  SetLength(fit, ndata);
  for I := 0 to ndata - 1 do begin
    magArbFloatArray[I] := mag[I]; // for compatibility with NumLib
  end;

  if TrendDegree = 0 then begin
    // Special case: PolyFit may generate error. Also, no need of using the complicated procedure
    fit0 := Math.Mean(magArbFloatArray);
    for I := 0 to ndata - 1 do begin
      fit[I] := fit0;
    end;
  end
  else begin
    PolyFit(a, magArbFloatArray, TrendDegree, 0, fit);
  end;

  for I := 0 to ndata - 1 do begin
    temp_mags[I] := magArbFloatArray[I] - fit[I];
  end;

  Result := PopnVariance(temp_mags);
end;

procedure dcdft_proc(
          const t, mag: TDoubleArray;
          lowfreq, hifreq: Double;
          freq_step: Double;
          TrendDegree: Integer;
          TrigPolyDegree: Integer;
          CmdLineNumberOfThreads: Integer;
          ProgressCaptionProc: TProgressCaptionProc;
          out frequencies, power: TDoubleArray);
var
  n_freq: Integer;
  sigmaSquaredO: Double;
  startfreq: Double;
  NumberOfThreads, StepsPerThread, Remainder, StepsToDo: integer;
  I, II, Idx: Integer;
  Threads: array of TCalcThread;
begin
  n_freq := GetNofFrequencies(lowfreq, hifreq, freq_step);

  SetLength(frequencies, n_freq);
  SetLength(power, n_freq);

  if CmdLineNumberOfThreads < 1 then
    NumberOfThreads := GetLogicalCpuCount()
  else
    NumberOfThreads := CmdLineNumberOfThreads;

  //OutputDebugString(PChar('Number of threads: ' + IntToStr(NumberOfThreads)));

  StepsPerThread := n_freq div NumberOfThreads;
  Remainder := n_freq - StepsPerThread * NumberOfThreads;

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
        Threads[I] := TCalcThread.Create(I, t, mag, startfreq, freq_step, StepsToDo, TrendDegree, TrigPolyDegree, n_freq, ProgressCaptionProc);
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
           power[Idx] := Threads[I].Power[II];
        end;
      end;
    finally
      for I := Length(Threads) - 1 downto 0 do begin
         FreeAndNil(Threads[I]);
      end;
    end;
  finally
    DoneCriticalSection(GlobalCounterCriticalSection);
  end;

  sigmaSquaredO := CalcSigmaSquaredO(t, mag, TrendDegree);

  for I := 0 to n_freq - 1 do begin
    power[I] := 1.0 - power[I] / sigmaSquaredO;
  end;

end;

end.


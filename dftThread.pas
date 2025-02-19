unit dftThread;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, lcvtypes, math, unitDFT;

type

  { TDFTThread }

  TDFTThread = class(TThread)
  private
    FParams: TDCDFTparameters;
    FProgressCaptionProc: TProgressCaptionProc;
  protected
    procedure Execute; override;
  public
    property Params: TDCDFTparameters read FParams;
    constructor Create(AParams: TDCDFTparameters;
                       AOnTerminate: TNotifyEvent;
                       ProgressCaptionProc: TProgressCaptionProc);
  end;


implementation

constructor TDFTThread.Create(AParams: TDCDFTparameters;
                              AOnTerminate: TNotifyEvent;
                              ProgressCaptionProc: TProgressCaptionProc);
begin
  inherited Create(True);
  OnTerminate := AOnTerminate;
  FreeOnTerminate := True;
  FProgressCaptionProc := ProgressCaptionProc;
  FParams := AParams;
end;

procedure TDFTThread.Execute;
var
  FPUExceptionMask: TFPUExceptionMask;
begin
  try
    // Under Linux, all exceptions get masked (at least sometimes)
    // For compatibility, we explicitly set the mask
    FPUExceptionMask := GetExceptionMask;
    SetExceptionMask([exDenormalized, exUnderflow, exPrecision]);
    try
      dcdft_proc(FParams.X, FParams.Y,
                 FParams.FrequencyMin, FParams.FrequencyMax, FParams.FrequencyResolution,
                 FParams.TrendDegree, FParams.TrigPolyDegree,
                 0,
                 FProgressCaptionProc,
                 FParams.frequencies, FParams.power);
    finally
      SetExceptionMask(FPUExceptionMask);
    end;
  except
    on E: Exception do begin
      FParams.Error := E.Message;
      if FParams.Error = '' then
        FParams.Error := 'Unknown Error';
    end
    else
      FParams.Error := 'Unknown Error';
  end;
end;

end.


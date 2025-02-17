unit dftThread;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, lcvtypes, unitDFT;

type

  { TDFTThread }

  TDFTThread = class(TThread)
  private
    FParamsPtr: PDCDFTparameters;
    FProgressCaptionProc: TProgressCaptionProc;
  protected
    procedure Execute; override;
  public
    constructor Create(AParamsPtr: PDCDFTparameters;
                       AOnTerminate: TNotifyEvent;
                       ProgressCaptionProc: TProgressCaptionProc);
  end;


implementation

constructor TDFTThread.Create(AParamsPtr: PDCDFTparameters;
                              AOnTerminate: TNotifyEvent;
                              ProgressCaptionProc: TProgressCaptionProc);
begin
  inherited Create(True);
  OnTerminate := AOnTerminate;
  FreeOnTerminate := True;
  FProgressCaptionProc := ProgressCaptionProc;
  FParamsPtr := AParamsPtr;
end;

procedure TDFTThread.Execute;
begin
  try
    dcdft_proc(FParamsPtr^.X, FParamsPtr^.Y,
               FParamsPtr^.FrequencyMin, FParamsPtr^.FrequencyMax, FParamsPtr^.FrequencyResolution,
               FParamsPtr^.TrendDegree, FParamsPtr^.TrigPolyDegree,
               0,
               FProgressCaptionProc,
               FParamsPtr^.frequencies, FParamsPtr^.periods, FParamsPtr^.power);
  except
    on E: Exception do begin
      FParamsPtr^.Error := E.Message;
      if FParamsPtr^.Error = '' then
        FParamsPtr^.Error := 'Unknown Error';
    end
    else
      FParamsPtr^.Error := 'Unknown Error';
  end;
end;

end.


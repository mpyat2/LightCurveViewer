unit dftThread;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Classes, SysUtils, unitDFT;

type

  { TDFTThread }

  TDFTThread = class(TThread)
  private
    FParamsPtr: PDCDFTparameters;
  protected
    procedure Execute; override;
  public
    constructor Create(AParamsPtr: PDCDFTparameters; AOnTerminate: TNotifyEvent);
  end;


implementation

constructor TDFTThread.Create(AParamsPtr: PDCDFTparameters; AOnTerminate: TNotifyEvent);
begin
  inherited Create(True);
  OnTerminate := AOnTerminate;
  FreeOnTerminate := True;
  FParamsPtr := AParamsPtr;
end;

procedure TDFTThread.Execute;
begin
  try
    dcdft_proc(FParamsPtr^.X, FParamsPtr^.Y,
               FParamsPtr^.FrequencyMin, FParamsPtr^.FrequencyMax, FParamsPtr^.FrequencyResolution,
               FParamsPtr^.TrendDegree, FParamsPtr^.TrigPolyDegree,
               0,
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


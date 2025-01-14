unit dftthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unitdcdft;

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
               True, 0,
               FParamsPtr^.frequencies, FParamsPtr^.periods, FParamsPtr^.amp, FParamsPtr^.power);
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


unit unitdftdialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  TAGraph, TASeries, unitdcdft;

type

  { TFormDFTDialog }

  TFormDFTDialog = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    PageControl1: TPageControl;
    TabSheetFrequencies: TTabSheet;
    TabSheetTable: TTabSheet;
  private

  public
    procedure PlotData(const frequencies, periods, apm, power: TFloatArray);
  end;

var
  FormDFTDialog: TFormDFTDialog;

implementation

{$R *.lfm}

uses
  math;

{ TFormDFTDialog }

procedure TFormDFTDialog.PlotData(const frequencies, periods, apm, power: TFloatArray);
var
  I: Integer;
begin
  Hide;
  Chart1LineSeries1.Clear;
  for I := 0 to Length(frequencies) - 1 do begin
    if not IsNan(power[I]) then
      Chart1LineSeries1.AddXY(frequencies[I], power[I]);
  end;
  Show;
end;

end.


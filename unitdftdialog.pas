unit unitdftdialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  TAGraph, TASeries;

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

  end;

var
  FormDFTDialog: TFormDFTDialog;

implementation

{$R *.lfm}

end.


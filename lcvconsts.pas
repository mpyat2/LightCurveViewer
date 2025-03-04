unit lcvconsts;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Graphics;

const
  defRemoteManual = 'https://github.com/mpyat2/LightCurveViewer/blob/main/doc/LightCurveViewer.pdf';
  defLocalManual  = 'doc/LightCurveViewer.pdf';

const
  CycleByCycleColors: array of TColor = (clBlack, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua(*, clSkyBlue*));

implementation

end.


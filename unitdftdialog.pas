unit unitdftdialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Grids, Menus, ActnList, TAGraph, TASeries, common;

type

  { TFormDFTDialog }

  TFormDFTDialog = class(TForm)
    ActionGridCopyAll: TAction;
    ActionList: TActionList;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    DrawGrid1: TDrawGrid;
    MenuItem1: TMenuItem;
    PageControl1: TPageControl;
    PopupMenuGrid: TPopupMenu;
    TabSheetFrequencies: TTabSheet;
    TabSheetTable: TTabSheet;
    procedure ActionGridCopyAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetGridCell(C, R: Integer): string;
     procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  public
    procedure PlotData(const frequencies, periods, apm, power: TFloatArray);
  end;

var
  FormDFTDialog: TFormDFTDialog;

implementation

{$R *.lfm}

uses
  math, Clipbrd;

{ TFormDFTDialog }

procedure TFormDFTDialog.FormCreate(Sender: TObject);
begin
  DrawGrid1.OnDrawCell := @GridDrawCell;
end;

procedure TFormDFTDialog.ActionGridCopyAllExecute(Sender: TObject);
var
  CurrentCursor: TCursor;
  S, S1: string;
  R: Integer;
begin
  CurrentCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    S := '';
    for R := 0 to DrawGrid1.RowCount - 1 do begin
      S1 := GetGridCell(0, R) + ^I + GetGridCell(1, R) + ^M^J;
      S := S + S1;
    end;
    Clipboard.AsText := S;
  finally
    Screen.Cursor := CurrentCursor;
  end;
end;

function TFormDFTDialog.GetGridCell(C, R: Integer): string;
var
  V: Double;
begin
  Result := '';
  if R < DrawGrid1.FixedRows + Chart1LineSeries1.Count then begin
    if R >= DrawGrid1.FixedRows then begin
      if C = DrawGrid1.FixedCols then
        V := Chart1LineSeries1.XValue[R - DrawGrid1.FixedRows]
      else
      if C = DrawGrid1.FixedCols + 1 then
        V := Chart1LineSeries1.YValue[R - DrawGrid1.FixedRows]
      else
        Exit;
      Result := FloatToStr(V);
    end
    else begin
      if C = DrawGrid1.FixedCols then
        Result := 'Frequency'
      else
      if C = DrawGrid1.FixedCols + 1 then
        Result := 'Power'
      else
        Exit;
    end;
  end;
end;

procedure TFormDFTDialog.GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  GridCanvas: TCanvas;
  S: string;
begin
  GridCanvas := DrawGrid1.Canvas;
  GridCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, GetGridCell(aCol, aRow));
end;

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
  DrawGrid1.ColCount := 2 + DrawGrid1.FixedCols;
  DrawGrid1.RowCount := Chart1LineSeries1.Count + DrawGrid1.FixedRows;
  Show;
end;

end.


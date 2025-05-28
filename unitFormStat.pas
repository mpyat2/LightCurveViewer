unit unitFormStat;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, Menus, ActnList, lcvtypes;

type

  { TFormStatistics }

  TFormStatistics = class(TForm)
    ActionSelectAll: TAction;
    ActionCopy: TAction;
    ActionList1: TActionList;
    ButtonClose: TButton;
    DrawGrid1: TDrawGrid;
    MenuItemCopy: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    PanelControls: TPanel;
    PanelButtons: TPanel;
    PopupMenu1: TPopupMenu;
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FParameters: TStringArray;
    FValues: TDoubleArray;
    procedure CalcStat(const ATimes, AMagnitudes: TDoubleArray);
    function GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  public
  end;

procedure ShowStatistics(const Times, Magnitudes: TDoubleArray);

implementation

{$R *.lfm}

uses
  ClipBrd, math, miscutils, guiutils;

procedure ShowStatistics(const Times, Magnitudes: TDoubleArray);
var
  F: TFormStatistics;
  A: TDoubleArray; I: Integer;
begin
  F := TFormStatistics.Create(Application);
  try
    SetLength(A, Length(Magnitudes)); for I := 0 to Length(Magnitudes) - 1 do A[I] := Magnitudes[I];
    F.CalcStat(Times, Magnitudes);
    for I := 0 to Length(Magnitudes) - 1 do if A[I] <> Magnitudes[I] then ShowMessage('!!');
    F.ShowModal;
  finally
    FreeAndNil(F);
  end;
end;

{ TFormStatistics }

procedure TFormStatistics.FormCreate(Sender: TObject);
begin
  DrawGrid1.OnDrawCell := @GridDrawCell;
  DrawGrid1.DefaultColWidth := 128;
end;

procedure TFormStatistics.ActionCopyExecute(Sender: TObject);
begin
  Clipboard.AsText := GetGridSelectionAsText(DrawGrid1, @GetGridCell);
end;

procedure TFormStatistics.ActionSelectAllExecute(Sender: TObject);
var
  Selection: TRect;
begin
  Selection.Top := DrawGrid1.FixedRows;
  Selection.Bottom := DrawGrid1.RowCount - 1;
  Selection.Left := DrawGrid1.FixedCols;
  Selection.Right := DrawGrid1.ColCount - 1;
  DrawGrid1.Selection := Selection;
end;

procedure TFormStatistics.CalcStat(const ATimes, AMagnitudes: TDoubleArray);
var
  ATempMagnitudes: TDoubleArray;
  I: Integer;
begin
  if Length(ATimes) <> Length(AMagnitudes) then
    CalcError('Invalid parameters');
  if ATimes = nil then
    Exit;
  SetLength(FParameters, 5);
  SetLength(FValues, 5);
  FParameters[0] := 'Number of obs';
  FValues[0] := Length(ATimes);
  FParameters[1] := 'Time mean';
  FValues[1] := math.Mean(ATimes);
  FParameters[2] := 'Magnitude Mean';
  FValues[2] := math.Mean(AMagnitudes);
  FParameters[3] := 'Magnitude StDev';
  FValues[3] := math.StdDev(AMagnitudes);
  FParameters[4] := 'Magnitude Median';
  SetLength(ATempMagnitudes, Length(AMagnitudes));
  for I := 0 to Length(AMagnitudes) - 1 do
    ATempMagnitudes[I] := AMagnitudes[I];
  // WARNING!! This function modifies the input array
  FValues[4] := WirthMedian(ATempMagnitudes);
end;

function TFormStatistics.GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
var
  Idx: Integer;
begin
  Result := '';
  if R < 1 then begin
    case C of
      0: Result := 'Parameter';
      1: Result := 'Value';
    end;
  end
  else begin
    Idx := R - 1;
    if (Idx >= 0) and (Idx < Length(FParameters)) then begin
      case C of
        0: Result := FParameters[Idx];
        1: Result := FloatToStr(FValues[Idx]);
      end;
    end;
  end;
end;

procedure TFormStatistics.GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  GridCanvas: TCanvas;
begin
  GridCanvas := DrawGrid1.Canvas;
  GridCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, GetGridCell(DrawGrid1, aCol, aRow));
end;

end.


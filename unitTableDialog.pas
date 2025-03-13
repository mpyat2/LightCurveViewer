unit unitTableDialog;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Menus, ActnList,
  ExtCtrls, StdCtrls, lcvtypes;

type

  { TFormTable }

  TFormTable = class(TForm)
    ActionSelectAll: TAction;
    ActionCopy: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    DrawGrid1: TDrawGrid;
    MenuItem1: TMenuItem;
    MenuItemCopy: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTimes: TDoubleArray;
    FMagnitudes: TDoubleArray;
    FPeriod: Double;
    FEpoch: Double;
    function GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  public

  end;

procedure ShowObservations(const Times, Magnitudes: TDoubleArray; Period, Epoch: Double; Title: string);

implementation

{$R *.lfm}

uses
  Clipbrd, math, miscutils, guiutils;

procedure ShowObservations(const Times, Magnitudes: TDoubleArray; Period, Epoch: Double; Title: string);
var
  F: TFormTable;
begin
  if Length(times) <> Length(magnitudes) then
    raise Exception.Create('X and Y arrays must be of equal length');
  F := TFormTable.Create(Application);
  try
    F.Caption := Title;
    F.FTimes := Times;
    F.FMagnitudes := Magnitudes;
    F.FPeriod := Period;
    F.FEpoch := Epoch;
    if (not IsNan(Period)) and (not IsNan(Epoch)) then
      F.DrawGrid1.ColCount := 4
    else
      F.DrawGrid1.ColCount := 3;
    F.DrawGrid1.FixedCols := 1;
    F.DrawGrid1.RowCount := Length(Times) + 1;
    F.DrawGrid1.FixedRows := 1;
    F.ShowModal;
  finally
    FreeAndNil(F);
  end;
end;

{ TFormTable }

procedure TFormTable.FormCreate(Sender: TObject);
begin
  DrawGrid1.OnDrawCell := @GridDrawCell;
  DrawGrid1.ColWidths[0] := 64;
end;

procedure TFormTable.ActionCopyExecute(Sender: TObject);
var
  CurrentCursor: TCursor;
begin
  CurrentCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    Clipboard.AsText := GetGridSelectionAsText(DrawGrid1, @GetGridCell);
  finally
    Screen.Cursor := CurrentCursor;
  end;
end;

procedure TFormTable.ActionSelectAllExecute(Sender: TObject);
var
  Selection: TRect;
begin
  Selection.Top := DrawGrid1.FixedRows;
  Selection.Bottom := DrawGrid1.RowCount - 1;
  Selection.Left := DrawGrid1.FixedCols;
  Selection.Right := DrawGrid1.ColCount - 1;
  DrawGrid1.Selection := Selection;
end;

function TFormTable.GetGridCell(Grid: TDrawGrid; C, R: Integer): string;
var
  Idx: Integer;
begin
  Result := '';
  if R < 1 then begin
    case C of
      1: Result := 'Time';
      2: Result := 'Magnitude';
      3: Result := 'Phase';
    end;
  end
  else begin
    Idx := R - 1;
    if (Idx >= 0) and (Idx < Length(FTimes)) then begin
      case C of
        0: Result := Format('%8d', [Idx + 1]);
        1: Result := FloatToStr(FTimes[Idx]);
        2: Result := FloatToStr(FMagnitudes[Idx]);
        3: try
             Result := FloatToStr(CalculatePhase(FTimes[Idx], FPeriod, FEpoch));
           except
             on E: Exception do begin
               Result := E.Message;
             end;
           end;
      end;
    end;
  end;
end;

procedure TFormTable.GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  GridCanvas: TCanvas;
begin
  GridCanvas := DrawGrid1.Canvas;
  GridCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, GetGridCell(DrawGrid1, aCol, aRow));
end;


end.


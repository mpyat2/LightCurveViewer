unit colorLegend;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, Menus, ActnList,
  lcvtypes, Types;

type

  { TFormColorLegend }

  TFormColorLegend = class(TForm)
    ActionCopyWithTitles: TAction;
    ActionSelectAll: TAction;
    ActionCopy: TAction;
    ActionListGrid: TActionList;
    DrawGrid1: TDrawGrid;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemCopyWithTitles: TMenuItem;
    PopupMenuGrid: TPopupMenu;
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCopyWithTitlesExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  private
    FRegions: TFoldedRegions;
    function GetGridCell(Grid: TDrawGrid; aCol, aRow: Integer): string;
  public
    procedure UpdateGrid;
  end;

procedure ShowColorLegend(Regions: TFoldedRegions; const Title: string);

procedure HideColorLegend;

implementation

{$R *.lfm}

uses
  Clipbrd, guiutils;

var
  FormColorLegend: TFormColorLegend = nil;

procedure ShowColorLegend(Regions: TFoldedRegions; const Title: string);
begin
  if FormColorLegend = nil then begin
    FormColorLegend := TFormColorLegend.Create(Application);
    FormColorLegend.Position := poDesigned;
    FormColorLegend.Top := Application.MainForm.Top + Application.MainForm.Height - FormColorLegend.Height;
    FormColorLegend.Left := Application.MainForm.Left + Application.MainForm.Width - FormColorLegend.Width;
  end;
  FormColorLegend.Hide;
  FormColorLegend.Caption := Title;
  FormColorLegend.FRegions := Regions;
  FormColorLegend.UpdateGrid;
  FormColorLegend.Show;
end;

procedure HideColorLegend;
begin
  if FormColorLegend <> nil then begin
    FormColorLegend.Hide;
    FormColorLegend.Caption := '';
    FormColorLegend.FRegions := nil;
  end;
end;

{ TFormColorLegend }

procedure TFormColorLegend.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  GridCanvas: TCanvas;
  DrawTextStyle: TTextStyle;
begin
  GridCanvas := DrawGrid1.Canvas;
  if aCol = 0 then begin
    if (aRow = 0) then begin
      GridCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, GetGridCell(DrawGrid1, aCol, aRow));
    end
    else
    if (aRow > 0) and (FRegions <> nil) and (aRow <= FRegions.Count) then begin
      GridCanvas.Brush.Color := FRegions.Get(aRow - 1).FColor;
      GridCanvas.Rectangle(aRect);
      GridCanvas.Font.Color := clWhite - GridCanvas.Brush.Color;
      GridCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, GetGridCell(DrawGrid1, aCol, aRow));
    end;
  end
  else
  if aCol > 0 then begin
    GridCanvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, GetGridCell(DrawGrid1, aCol, aRow));
  end;
end;

procedure TFormColorLegend.ActionCopyExecute(Sender: TObject);
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

procedure TFormColorLegend.ActionCopyWithTitlesExecute(Sender: TObject);
var
  CurrentCursor: TCursor;
begin
  CurrentCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    Clipboard.AsText := GetGridSelectionAsText(DrawGrid1, @GetGridCell, True);
  finally
    Screen.Cursor := CurrentCursor;
  end;
end;

procedure TFormColorLegend.ActionSelectAllExecute(Sender: TObject);
var
  Selection: TRect;
begin
  Selection.Top := DrawGrid1.FixedRows;
  Selection.Bottom := DrawGrid1.RowCount - 1;
  Selection.Left := DrawGrid1.FixedCols;
  Selection.Right := DrawGrid1.ColCount - 1;
  DrawGrid1.Selection := Selection;
end;

function TFormColorLegend.GetGridCell(Grid: TDrawGrid; aCol, aRow: Integer): string;
var
  R: TFoldedRegion;
begin
  Result := '';
  if aRow = 0 then begin
    case aCol of
      0: Result := 'Cycle φ ± 0.5';
      1: Result := 'Data X min';
      2: Result := 'Data X max';
      3: Result := 'φ = 0';
      4: Result := 'φ = -0.5';
      5: Result := 'φ = +0.5';
    end;
  end
  else
  if (aRow > 0) and (FRegions <> nil) and (aRow <= FRegions.Count) then begin
    R := FRegions.Get(aRow - 1);
    case aCol of
      0: Result := 'Cycle ' + IntToStr(R.FCycleN);
      1: Result := FloatToStr(R.FX1);
      2: Result := FloatToStr(R.FX2);
      3: Result := FloatToStr(R.FCylce0);
      4: Result := FloatToStr(R.FCylce0 - R.FPeriod / 2.0);
      5: Result := FloatToStr(R.FCylce0 + R.FPeriod / 2.0);
    end;
  end;
end;

procedure TFormColorLegend.UpdateGrid;
begin
  if FRegions <> nil then
    DrawGrid1.RowCount := FRegions.Count + 1
  else
    DrawGrid1.RowCount := 1;
  DrawGrid1.DefaultColWidth := 200;
  DrawGrid1.ColWidths[0] := 100;
end;

end.


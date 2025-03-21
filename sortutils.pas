unit sortutils;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils, lcvtypes;

procedure SortModelPoints(var Fit: TFitColumnArray);

procedure SortDataPoints(var X, Y, E: TDoubleArray);

procedure SortDataNPointsYDesc(var X, Y: TDoubleArray; var N: TIntegerArray);

procedure SortFloatArray(var X: TDoubleArray);

implementation

uses
  math;

function CompareFitPoints(Item1, Item2: Pointer): Integer;
begin
  if TFitPoint(Item1).X < TFitPoint(Item2).X then
    Result := -1
  else
  if TFitPoint(Item1).X > TFitPoint(Item2).X then
    Result := 1
  else
    Result := 0;
end;

function CompareXY(Item1, Item2: Pointer): Integer;
begin
  if TXY(Item1).X < TXY(Item2).X then
    Result := -1
  else
  if TXY(Item1).X > TXY(Item2).X then
    Result := 1
  else
    Result := 0;
end;

function CompareXYNYdesc(Item1, Item2: Pointer): Integer;
begin
  if TXYN(Item1).Y > TXYN(Item2).Y then
    Result := -1
  else
  if TXYN(Item1).Y < TXYN(Item2).Y then
    Result := 1
  else
    Result := 0;
end;

function CompareD(Item1, Item2: Pointer): Integer;
begin
  if TDouble(Item1).D < TDouble(Item2).D then
    Result := -1
  else
  if TDouble(Item1).D > TDouble(Item2).D then
    Result := 1
  else
    Result := 0;
end;

procedure SortModelPoints(var Fit: TFitColumnArray);
var
  List: TList;
  Item: TFitPoint;
  X, Y, E, O: Double;
  I: Integer;
begin
  Assert(Length(Fit[FitColumnType.x]) = Length(Fit[FitColumnType.yFit]), 'SortModelPoints: X and YFit must be of equal length');
  Assert(Length(Fit[FitColumnType.yFit]) = Length(Fit[FitColumnType.yErrors]), 'SortModelPoints: YFit and YErrors must be of equal length');
  if Fit[FitColumnType.yObserved] <> nil then begin
    Assert(Length(Fit[FitColumnType.x]) = Length(Fit[FitColumnType.yObserved]), 'SortModelPoints: X and YObserved must be of equal length');
  end;
  List := TList.Create;
  try
    for I := 0 to Length(Fit[FitColumnType.x]) - 1 do begin
      X := Fit[FitColumnType.x][I];
      Y := Fit[FitColumnType.yFit][I];
      E := Fit[FitColumnType.yErrors][I];
      if Fit[FitColumnType.yObserved] <> nil then
        O := Fit[FitColumnType.yObserved][I]
      else
        O := NaN;
      List.Add(TFitPoint.Create(X, Y, E, O));
    end;
    List.Sort(@CompareFitPoints);
    for I := 0 to Length(Fit[FitColumnType.x]) - 1 do begin
      Item := TFitPoint(List[I]);
      Fit[FitColumnType.x][I] := Item.X;
      Fit[FitColumnType.yFit][I] := Item.Y;
      Fit[FitColumnType.yErrors][I] := Item.E;
      if Fit[FitColumnType.yObserved] <> nil then begin
        Fit[FitColumnType.yObserved][I] := Item.O;
      end;
    end;
  finally
    for I := List.Count - 1 downto 0 do begin
      TFitPoint(List[I]).Free;
      List[I] := nil;
    end;
    FreeAndNil(List);
  end;
end;

procedure SortDataPoints(var X, Y, E: TDoubleArray);
var
  List: TList;
  I: Integer;
begin
  Assert(Length(X) = Length(Y), 'SortDataPoints: X and Y must be of equal length');
  Assert((E = nil) or (Length(X) = Length(E)), 'SortDataPoints: X and E must be of equal length');
  List := TList.Create;
  try
    if E <> nil then begin
      for I := 0 to Length(X) - 1 do
        List.Add(TXY.Create(X[I], Y[I], E[I]));
    end
    else begin
      for I := 0 to Length(X) - 1 do
        List.Add(TXY.Create(X[I], Y[I], 0.0));
    end;

    List.Sort(@CompareXY);

    if E <> nil then begin
      for I := 0 to Length(X) - 1 do begin
        X[I] := TXY(List[I]).X;
        Y[I] := TXY(List[I]).Y;
        E[I] := TXY(List[I]).E;
      end;
    end
    else begin
      for I := 0 to Length(X) - 1 do begin
        X[I] := TXY(List[I]).X;
        Y[I] := TXY(List[I]).Y;
      end;
    end;

  finally
    for I := List.Count - 1 downto 0 do begin
      TXY(List[I]).Free;
      List[I] := nil;
    end;
    FreeAndNil(List);
  end;
end;

procedure SortDataNPointsYDesc(var X, Y: TDoubleArray; var N: TIntegerArray);
var
  List: TList;
  I: Integer;
begin
  Assert(Length(X) = Length(Y), 'SortDataNPoints: X and Y must be of equal length');
  Assert(Length(X) = Length(N), 'SortDataNPoints: X and N must be of equal length');
  List := TList.Create;
  try
    for I := 0 to Length(X) - 1 do
      List.Add(TXYN.Create(X[I], Y[I], N[I]));

    List.Sort(@CompareXYNYdesc);

    for I := 0 to Length(X) - 1 do begin
      X[I] := TXYN(List[I]).X;
      Y[I] := TXYN(List[I]).Y;
      N[I] := TXYN(List[I]).N;
    end;
  finally
    for I := List.Count - 1 downto 0 do begin
      TXYN(List[I]).Free;
      List[I] := nil;
    end;
    FreeAndNil(List);
  end;
end;

procedure SortFloatArray(var X: TDoubleArray);
var
  List: TList;
  I: Integer;
begin
  List := TList.Create;
  try
    for I := 0 to Length(X) - 1 do begin
      List.Add(TDouble.Create(X[I]));
    end;
    List.Sort(@CompareD);
    for I := 0 to Length(X) - 1 do begin
      X[I] := TDouble(List[I]).D;
    end;
  finally
    for I := List.Count - 1 downto 0 do begin
      TDouble(List[I]).Free;
      List[I] := nil;
    end;
    FreeAndNil(List);
  end;
end;

end.


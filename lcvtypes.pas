unit lcvtypes;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Graphics, contnrs;

type
  FitColumnType = (x, yFit, yErrors, yFitAlgebraic, yObserved);

type
  TChartColorMode = (None, Phase, Day, Year, Custom);

//type
//  TColorArray = array of TColor;

type
  TDoubleArray = array of Double;
  TIntegerArray = array of Integer;
  TBooleanArray = array of Boolean;
  THarmonicIntArray = array[0..8] of Integer;
  THarmonicDblArray = array[0..8] of Double;
  TFitColumnArray = array[FitColumnType] of TDoubleArray;

type
  TFitPoint = class
    X, Y, E, O: Double;
    constructor Create(AX, AY, AE, AO: Double);
  end;

type
  TXYN = class
    X: Double;
    Y: Double;
    N: Integer;
    constructor Create(AX, AY: Double; AN: Integer);
  end;

type
  TXY = class
    X: Double;
    Y: Double;
    E: Double;
    constructor Create(AX, AY, AE: Double);
  end;

type
  TDouble = class
    D: Double;
    constructor Create(V: Double);
  end;

type
  TBaseRegion = class
    FX1, FX2: Double;
    FColor: TColor;
    constructor Create(X1, X2: Double; Color: TColor);
  end;

type
  TSimpleRegion = class(TBaseRegion)
    FIntervalName: string;
    constructor Create(X1, X2: Double; Color: TColor; const IntervalName: string);
  end;

type
  TFoldedRegion = class(TBaseRegion)
    FCycleN: Integer;
    FCycle0: Double;
    FPeriod: Double;
    constructor Create(X1, X2: Double; Color: TColor; CycleN: Integer; Cycle0, Period: Double);
  end;

type
  TBaseRegions = class(TObjectList)
    FInfo: string;
    function Get(Index: Integer): TBaseRegion;
  end;

type
  TSimpleRegions = class(TBaseRegions)
    procedure AddRegion(X1, X2: Double; Color: TColor; const IntervalName: string);
  end;

type
  TFoldedRegions = class(TBaseRegions)
    procedure AddRegion(X1, X2: Double; Color: TColor; CycleN: Integer; Cycle0, Period: Double);
  end;

type
  TPhaseHistoryItem = class
    FPhase: Double;
    FEpoch: Double;
    constructor Create(APhase, AEpoch: Double);
    function GetDescription: string;
  end;

type
  TPhaseHistory = class(TObjectList)
    function Get(Index: Integer): TPhaseHistoryItem;
    procedure InsertItem(APhase, AEpoch: Double);
  end;

type
  TProgressCaptionProc = procedure (const Msg: string) of object;

implementation

uses
  SysUtils;

{ TFitPoint }

constructor TFitPoint.Create(AX, AY, AE, AO: Double);
begin
  inherited Create;
  X := AX;
  Y := AY;
  E := AE;
  O := AO;
end;

{ TXYN }

constructor TXYN.Create(AX, AY: Double; AN: Integer);
begin
  inherited Create;
  X := AX;
  Y := AY;
  N := AN;
end;

{ TXY }

constructor TXY.Create(AX, AY, AE: Double);
begin
  inherited Create;
  X := AX;
  Y := AY;
  E := AE;
end;

{ TDouble }

constructor TDouble.Create(V: Double);
begin
  Self.D := V;
end;

{ TBaseRegion }

constructor TBaseRegion.Create(X1, X2: Double; Color: TColor);
begin
  FX1 := X1;
  FX2 := X2;
  FColor := Color;
end;

{ TSimpleRegion }

constructor TSimpleRegion.Create(X1, X2: Double; Color: TColor; const IntervalName: string);
begin
  inherited Create(X1, X2, Color);
  FIntervalName := IntervalName;
end;

{ TFoldedRegion }

constructor TFoldedRegion.Create(X1, X2: Double; Color: TColor; CycleN: Integer; Cycle0, Period: Double);
begin
  inherited Create(X1, X2, Color);
  FCycleN := CycleN;
  FCycle0 := Cycle0;
  FPeriod := Period;
end;

{ TBaseRegions }

function TBaseRegions.Get(Index: Integer): TBaseRegion;
begin
  Result := Self[Index] as TBaseRegion;
end;

{ TSimpleRegions }

procedure TSimpleRegions.AddRegion(X1, X2: Double; Color: TColor; const IntervalName: string);
begin
  Self.Add(TSimpleRegion.Create(X1, X2, Color, IntervalName));
end;

{ TFoldedRegions }

procedure TFoldedRegions.AddRegion(X1, X2: Double; Color: TColor; CycleN: Integer; Cycle0, Period: Double);
begin
  Self.Add(TFoldedRegion.Create(X1, X2, Color, CycleN, Cycle0, Period));
end;

{ TPhaseHistoryItem }

constructor TPhaseHistoryItem.Create(APhase, AEpoch: Double);
begin
  inherited Create;
  FPhase := APhase;
  FEpoch := AEpoch;
end;

function TPhaseHistoryItem.GetDescription: string;
begin
  Result := Format('Period: %g; Epoch: %g', [FPhase, FEpoch])
end;

{ TPhaseHistory }

function TPhaseHistory.Get(Index: Integer): TPhaseHistoryItem;
begin
  Result := Self[Index] as TPhaseHistoryItem;
end;

procedure TPhaseHistory.InsertItem(APhase, AEpoch: Double);
var
  Item: TPhaseHistoryItem;
  I: Integer;
begin
  for I := 0 to Self.Count - 1 do begin
    Item := Get(I);
    if (Item.FPhase = APhase) and (Item.FEpoch = AEpoch) then begin
      Self.Delete(I);
      Break;
    end;
  end;
  Self.Insert(0, TPhaseHistoryItem.Create(APhase, AEpoch));
end;

end.


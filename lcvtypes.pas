unit lcvtypes;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Graphics, contnrs;

type
  FitColumnType = (x, yFit, yErrors, yFitAlgebraic, yObserved);

type
  TChartColorMode = (None, Phase, Day, Year);

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
  TSimpleRegion = class
    FX1, FX2: Double;
    FColor: TColor;
    constructor Create(X1, X2: Double; Color: TColor);
  end;

type
  TSimpleRegions = class(TObjectList)
    procedure AddRegion(X1, X2: Double; Color: TColor);
    function Get(Index: Integer): TSimpleRegion;
  end;

type
  TFoldedRegion = class(TSimpleRegion)
    FCycleN: Integer;
    FCylce0: Double;
    FPeriod: Double;
    constructor Create(X1, X2: Double; Color: TColor; CycleN: Integer; Cylce0, Period: Double);
  end;

type
  TFoldedRegions = class(TObjectList)
    procedure AddRegion(X1, X2: Double; Color: TColor; CycleN: Integer; Cylce0, Period: Double);
    function Get(Index: Integer): TFoldedRegion;
  end;

type
  TProgressCaptionProc = procedure (const Msg: string) of object;

implementation

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

{ TSimpleRegion }

constructor TSimpleRegion.Create(X1, X2: Double; Color: TColor);
begin
  FX1 := X1;
  FX2 := X2;
  FColor := Color;
end;

{ TSimpleRegions }

procedure TSimpleRegions.AddRegion(X1, X2: Double; Color: TColor);
begin
  Self.Add(TSimpleRegion.Create(X1, X2, Color));
end;

function TSimpleRegions.Get(Index: Integer): TSimpleRegion;
begin
  Result := Self[Index] as TSimpleRegion;
end;

{ TFoldedRegion }

constructor TFoldedRegion.Create(X1, X2: Double; Color: TColor; CycleN: Integer; Cylce0, Period: Double);
begin
  inherited Create(X1, X2, Color);
  FCycleN := CycleN;
  FCylce0 := Cylce0;
  FPeriod := Period;
end;

{ TFoldedRegions }

procedure TFoldedRegions.AddRegion(X1, X2: Double; Color: TColor; CycleN: Integer; Cylce0, Period: Double);
begin
  Self.Add(TFoldedRegion.Create(X1, X2, Color, CycleN, Cylce0, Period));
end;

function TFoldedRegions.Get(Index: Integer): TFoldedRegion;
begin
  Result := Self[Index] as TFoldedRegion;
end;

end.


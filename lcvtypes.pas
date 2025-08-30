unit lcvtypes;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Graphics, contnrs;

type
  FitColumnType = (x, yFit, yErrors, yFitAlgebraic, yObserved);

//type
//  TColorArray = array of TColor;

type
  TDoubleArray = array of Double;
  TIntegerArray = array of Integer;
  TInt5Array = array[0..4] of Integer;
  TDouble5Array = array[0..4] of Double;
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
  TFoldedRegion = class
    FX1, FX2: Double;
    FColor: TColor;
    FCycleN: Integer;
    FCylce0: Double;
    FPeriod: Double;
    constructor Create(X1, X2: Double; Color: TColor; CycleN: Integer; Cylce0, Period: Double);
  end;

type
  TFoldedRegions = class(TObjectList)
    constructor Create;
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

{ TFoldedRegion }

constructor TFoldedRegion.Create(X1, X2: Double; Color: TColor; CycleN: Integer; Cylce0, Period: Double);
begin
  FX1 := X1;
  FX2 := X2;
  FColor := Color;
  FCycleN := CycleN;
  FCylce0 := Cylce0;
  FPeriod := Period;
end;

{ TFoldedRegions }

constructor TFoldedRegions.Create;
begin
  inherited Create;
end;

procedure TFoldedRegions.AddRegion(X1, X2: Double; Color: TColor; CycleN: Integer; Cylce0, Period: Double);
begin
  Self.Add(TFoldedRegion.Create(X1, X2, Color, CycleN, Cylce0, Period));
end;

function TFoldedRegions.Get(Index: Integer): TFoldedRegion;
begin
  Result := Self[Index] as TFoldedRegion;
end;

end.


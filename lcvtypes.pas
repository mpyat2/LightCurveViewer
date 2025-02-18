unit lcvtypes;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

type
  FitColumnType = (x, yFit, yErrors, yObserved);

type
  TDoubleArray = array of Double;
  TInt3Array = array[0..2] of Integer;
  TDouble3Array = array[0..2] of Double;
  TFitColumnArray = array[FitColumnType] of TDoubleArray;

type
  TFitPoint = class
    X, Y, E, O: Double;
    constructor Create(AX, AY, AE, AO: Double);
  end;

type
  TXY = class
    X: Double;
    Y: Double;
    constructor Create(AX, AY: Double);
  end;

type
  TDouble = class
    D: Double;
    constructor Create(V: Double);
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

{ TXY }

constructor TXY.Create(AX, AY: Double);
begin
  inherited Create;
  X := AX;
  Y := AY;
end;

{ TDouble }

constructor TDouble.Create(V: Double);
begin
  Self.D := V;
end;

end.


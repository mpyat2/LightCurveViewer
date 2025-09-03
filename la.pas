unit la;

{$mode ObjFPC}{$H+}

{$include lcv.inc}

interface

uses
  Classes, SysUtils;

const
  LAPACK_MIN_SHARED_LIB = {$IFDEF LINUX}'liblapack_min.so'{$ELSE}'lapack_min.dll'{$ENDIF};

{$IFDEF DYNAMIC_LOAD_LIB}
type
  Tdgels_solve = procedure(
    var trans: AnsiChar;
    var m, n, nrhs: Integer;
    var a: Double; var lda: Integer;
    var b: Double; var ldb: Integer;
    var info: Integer); cdecl;

  Tinvert_matrix = function(
    var a: Double;
    n: Integer): Integer; cdecl;

  Ttranspose_matrix = procedure(
    var a: Double;
    rows, cols: Integer;
    var b: double); cdecl;

var
  dgels_solve: Tdgels_solve;
  invert_matrix: Tinvert_matrix;
  transpose_matrix: Ttranspose_matrix;
{$ELSE}
procedure dgels_solve(
  var trans: AnsiChar;
  var m, n, nrhs: Integer;
  var a: Double; var lda: Integer;
  var b: Double; var ldb: Integer;
  var info: Integer);
  cdecl; external LAPACK_MIN_SHARED_LIB;

function invert_matrix(
  var a: Double;
  n: Integer): Integer;
  cdecl; external LAPACK_MIN_SHARED_LIB;

procedure transpose_matrix(
  var a: Double;
  rows, cols: Integer;
  var b: double);
  cdecl; external LAPACK_MIN_SHARED_LIB;
{$ENDIF}

//procedure TransposeMatrix(
//  const A: array of Double;
//  rows, cols: Integer;
//  var B: array of Double);

procedure MultiplyMatrices(
  const A, B: array of Double;
  rowsA, colsA, colsB: Integer;
  var C: array of Double);

implementation

{$IFDEF DYNAMIC_LOAD_LIB}
uses
  DynLibs{$IFDEF WINDOWS}, Windows{$ENDIF};
{$ENDIF}

//procedure TransposeMatrix(
//  const A: array of Double;
//  rows, cols: Integer;
//  var B: array of Double);
//var
//  i, j: Integer;
//begin
//  for i := 0 to rows - 1 do
//    for j := 0 to cols - 1 do
//      B[j * rows + i] := A[i * cols + j];
//end;

procedure MultiplyMatrices(
  const A, B: array of Double;
  rowsA, colsA, colsB: Integer;
  var C: array of Double);
var
  i, j, k: Integer;
  sum: Double;
begin
  for i := 0 to rowsA - 1 do
    for j := 0 to colsB - 1 do
    begin
      sum := 0.0;
      for k := 0 to colsA - 1 do
        sum += A[i * colsA + k] * B[k * colsB + j];
      C[i * colsB + j] := sum;
    end;
end;

{$IFDEF DYNAMIC_LOAD_LIB}
var
  LapackMinLib: TLibHandle;
  LibFullPath: string;
  Msg: string;

procedure FreeLapackMinLib;
begin
  dgels_solve := nil;
  invert_matrix := nil;
  transpose_matrix := nil;
  FreeLibrary(LapackMinLib);
  LapackMinLib := 0;
end;

initialization
  dgels_solve := nil;
  invert_matrix := nil;
  transpose_matrix := nil;
  LibFullPath := ExtractFilePath(ParamStr(0)) + LAPACK_MIN_SHARED_LIB;
  LapackMinLib := SafeLoadLibrary(LibFullPath);
  if LapackMinLib <> 0 then begin
    dgels_solve := Tdgels_solve(GetProcedureAddress(LapackMinLib, 'dgels_solve'));
    invert_matrix := Tinvert_matrix(GetProcedureAddress(LapackMinLib, 'invert_matrix'));
    transpose_matrix := Ttranspose_matrix(GetProcedureAddress(LapackMinLib, 'transpose_matrix'));
  end;
  if (not Assigned(dgels_solve)) or (not Assigned(invert_matrix)) or (not Assigned(transpose_matrix)) then begin
    if LapackMinLib <> 0 then begin
      FreeLapackMinLib;
    end;
    Msg := 'The ' + LAPACK_MIN_SHARED_LIB + ' library is invalid or cannot be loaded. Program will be terminated.';
    {$IFDEF WINDOWS}
    MessageBox(0, PChar(Msg), PChar(ExtractFileName(ParamStr(0))), MB_OK or MB_ICONERROR);
    {$ELSE}
    Writeln(stderr, Msg);
    {$ENDIF}
    Halt(1);
  end;
finalization
  if LapackMinLib <> 0 then begin
    FreeLapackMinLib;
  end;
{$ENDIF}
end.


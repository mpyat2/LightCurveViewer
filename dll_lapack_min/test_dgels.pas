program TestDGELS;

{$mode objfpc}
uses
  SysUtils;

type
  PDouble = ^Double;
  PInteger = ^LongInt;

procedure dgels_solve(trans: PChar; 
                      m, n, nrhs: PInteger;
                      a: PDouble; lda: PInteger;
                      b: PDouble; ldb: PInteger;
                      info: PInteger); cdecl;
                      external 'lapack_min.dll';

var
  m, n, nrhs, lda, ldb, info: LongInt;
  a: array[0..5] of Double;   // 3x2 matrix
  b: array[0..2] of Double;   // RHS (3x1)
  trans: AnsiChar;
  i: Integer;

begin
  // Example system: minimize || A*x - b ||
  // A = [1 1;
  //      1 2;
  //      1 3]
  // b = [1, 2, 2]
  m := 3;
  n := 2;
  nrhs := 1;
  lda := m;
  ldb := m;
  trans := 'N';

  // Column-major storage for A (Fortran-style)
  a[0] := 1; a[1] := 1; a[2] := 1; // column 1
  a[3] := 1; a[4] := 2; a[5] := 3; // column 2

  // b vector
  b[0] := 1;
  b[1] := 2;
  b[2] := 2;

  info := 0;
  dgels_solve(@trans, @m, @n, @nrhs, @a[0], @lda, @b[0], @ldb, @info);

  if info = 0 then
  begin
    WriteLn('DGELS succeeded. Solution x = ');
    for i := 0 to n-1 do
      WriteLn('x[', i, '] = ', b[i]:0:6);
  end
  else
    WriteLn('DGELS failed, INFO = ', info);
end.

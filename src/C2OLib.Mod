MODULE C2OLib;
IMPORT SYSTEM;

(*
-- Bit manipulations and unsigned integer conversion (Turbo Library)
-- © Helmut Zinn
--
--      CP  Turbo Pascal & C  MAX signed  unsigned
--  8 Bit  FF  BYTE  BYTE  127  256
--  16 Bit  FFFF  INTEGER  WORD, LONGINT  32.767  65.536
--  32 Bit  FFFF FFFF  LONGINT  HUGEINT  2.147.483.647
--  64 Bit  FFFF FFFF FFFF FFFF  HUGEINT  ./.  9.223.372.036.854.775.807
*)

PROCEDURE BitAnd* (a, b: LONGINT): LONGINT;
(* A bitwise AND is applied to the values <a> and <b>. *)
VAR sa, sb: SET;
BEGIN
  sa := SYSTEM.VAL(SET, a);
  sb := SYSTEM.VAL(SET, b);
  RETURN SYSTEM.VAL(LONGINT, sa * sb);
END BitAnd;

PROCEDURE BitOr* (a, b: LONGINT): LONGINT;
(* A bitwise OR is applied to the values <a> and <b>. *)
VAR sa, sb: SET;
BEGIN
  sa := SYSTEM.VAL(SET, a);
  sb := SYSTEM.VAL(SET, b);
  RETURN SYSTEM.VAL(LONGINT, sa + sb);
END BitOr;

PROCEDURE BitXor* (a, b: LONGINT): LONGINT;
(* A bitwise XOR is applied to the values <a> and <b>. *)
VAR sa, sb: SET;
BEGIN
  sa := SYSTEM.VAL(SET, a);
  sb := SYSTEM.VAL(SET, b);
  RETURN SYSTEM.VAL(LONGINT, sa / sb);
END BitXor;

PROCEDURE BitNot* (a: LONGINT): LONGINT;
(* The value of <a> is bitwise negated. *)
VAR sa: SET;
BEGIN
  sa := SYSTEM.VAL(SET, a);
  RETURN SYSTEM.VAL(LONGINT, -sa);
END BitNot;

PROCEDURE BitAsr* (a, n: LONGINT): LONGINT;
(* Arithmetic shift right n times, a * 2^-n, a >> n. *)
BEGIN
  ASSERT(n >= 0, 20); ASSERT(n <= 31, 21);
  RETURN ASH(a, -n);
END BitAsr;

PROCEDURE BitShl* (a, n: LONGINT): LONGINT;
(* Logical shift left n times, a * 2^n, a << n. *)
BEGIN
  ASSERT(n >= 0, 20); ASSERT(n <= 31, 21);
  RETURN ASH(a, n);
END BitShl;

PROCEDURE BitShr* (a, n: LONGINT): LONGINT;
(* Logical shift right n times, a * 2^-n, a >> n. *)
BEGIN
  ASSERT(n >= 0, 20); ASSERT(n <= 31, 21);
  IF n > 0 THEN
    a := ASH(a, -1);
    a := BitAnd(a, 7FFFFFFFH); (* insert zero at the first bit *)
    DEC(n);
  END;
  RETURN ASH(a, -n);
END BitShr;

PROCEDURE UnsignedLong* (x: LONGINT): HUGEINT;
(* Convert unsigned integer [32 bits] to HUGEINT [64 bits]. *)
VAR hx: HUGEINT;
BEGIN
  SYSTEM.GET(SYSTEM.ADR(x), hx); (* Safely convert LONGINT to HUGEINT *)
  IF x < 0 THEN
    RETURN hx + 4294967296
  ELSE
    RETURN hx
  END;
END UnsignedLong;

PROCEDURE UnsignedShort* (x: HUGEINT): LONGINT;
(* Convert HUGEINT [64 bits] to unsigned integer [32 bits]. *)
VAR lx: LONGINT;
BEGIN
  SYSTEM.GET(SYSTEM.ADR(x), lx); (* Safely convert HUGEINT to LONGINT *)
  ASSERT(x >= 0, 20);
  ASSERT(x < 2 * (MAX(LONGINT) + 1), 21);
  RETURN lx
END UnsignedShort;

PROCEDURE LowWord* (x: LONGINT): LONGINT;
(* Return the least significant [16 bits] of an unsigned integer [32 bits] *)
(* LowWord := x & 0xFFFF. *)
BEGIN
  RETURN x MOD 10000H;
END LowWord;

PROCEDURE HighWord* (x: LONGINT): LONGINT;
(* Return the most significant [16 bits] of an unsigned integer [32 bits] *)
(* HighWord := (x >> 16) & 0xFFFF. *)
BEGIN
  RETURN ASH(x, -16) MOD 10000H;
END HighWord;

PROCEDURE MergeWords* (high, low: LONGINT): LONGINT;
(* Concatenate high [16 bits] and low [16 bits] to an unsigned integer [32 bits] *)
(* MergeWord := (high << 16) OR low. *)
BEGIN
  ASSERT(high >= 0, 20); ASSERT(high < 2 * (MAX(INTEGER) + 1), 21);
  ASSERT(low >= 0, 20); ASSERT(low < 2 * (MAX(INTEGER) + 1), 21);
  RETURN ASH(high, 16) + low;
END MergeWords;

PROCEDURE Int* (x: REAL): LONGINT;
(* Returns integer value of x. *)
BEGIN
  RETURN SHORT(ENTIER(x))
END Int;

PROCEDURE Trunc* (x: REAL): LONGINT;
(* Returns integer value of x. *)
BEGIN
  RETURN Int(x);
END Trunc;

PROCEDURE Round* (x: REAL): LONGINT;
(* Returns integer value of x. *)
BEGIN
  RETURN Int(x + 0.5);
END Round;

END C2OLib.

(*-----------------------------------------------------------------------------
--   Unit Strings - Emulation of my old Turbo-Pascal string handling
--   (C) Helmut Zinn   V88/05/21 - V94/03/09 - V05/03/18
-----------------------------------------------------------------------------*)

MODULE C2OStrings;



  IMPORT Strings := ooc2Strings, IntStr := oocIntStr, RealStr := oocRealStr;

  CONST TAB* = 09X; CR* = 0DX;

  TYPE DynString* = POINTER TO ARRAY OF CHAR;

  PROCEDURE MakeDynString* (s: ARRAY OF CHAR): DynString;
    VAR len: LONGINT; d: DynString;
  BEGIN
    len := Strings.Length(s);  (* len := LEN(s$); *)
    NEW(d, len + 1);
    COPY(s, d^); (* d^ := s$; *)
    RETURN d;
  END MakeDynString;

  PROCEDURE MakeDynChar* (zeichen: CHAR; len: LONGINT): DynString;
    VAR i: LONGINT; res: DynString;
  BEGIN
    NEW(res, len + 1);
    FOR i := 0 TO len - 1 DO res[i] := zeichen END;
    res[len] := 0X;
    RETURN res;
  END MakeDynChar;

  PROCEDURE Cap* (ch: CHAR): CHAR;
  BEGIN
    IF ('a' <= ch) & (ch <= 'z') THEN RETURN CAP(ch) END;
    RETURN ch;
  END Cap;

  PROCEDURE IsAlpha* (ch: CHAR): BOOLEAN;
  BEGIN
    ch := Cap(ch);
    IF ('A' <= ch) & (ch <= 'Z') THEN RETURN TRUE END;
    RETURN FALSE
  END IsAlpha;

  PROCEDURE IsDigit* (ch: CHAR): BOOLEAN;
  BEGIN
    RETURN ('0' <= ch) & (ch <= '9');
  END IsDigit;

  PROCEDURE IsHex* (ch: CHAR): BOOLEAN;
  BEGIN
    RETURN ('A' <= CAP(ch)) & (CAP(ch) <= 'F');
  END IsHex;

  PROCEDURE IsSpace* (ch: CHAR): BOOLEAN;
  BEGIN
    RETURN (ch = ' ') OR (ch = TAB);
  END IsSpace;

  PROCEDURE Pos* (target, source: ARRAY OF CHAR): INTEGER;
    VAR pos: INTEGER;
      found: BOOLEAN;
  BEGIN
    (*Strings.Find(source, target, 0, pos);*) (* -- Note: Not found is pos := -1 (CP) instead of pos := 0 (TP) !!! *)
    Strings.FindNext(target, source, 0, found, pos); (* -- Note: oo2c strings unit returns boolean, so this needs to be converted to the expected interface *)
    IF ~found THEN pos := -1 END;
    RETURN pos;
  END Pos;

  PROCEDURE Substr* (source: ARRAY OF CHAR; a, len: INTEGER): DynString;
    VAR res: DynString;
  BEGIN
    NEW(res, len + 1);
    Strings.Extract(source, a, len, res^);
    RETURN res;
  END Substr;

  PROCEDURE Left* (source: ARRAY OF CHAR; len: INTEGER): DynString;
  BEGIN
    IF len >= Strings.Length(source) THEN
      RETURN MakeDynString(source)
    ELSE
      RETURN Substr(source, 0, len)
    END;
  END Left;

  PROCEDURE Right* (source: ARRAY OF CHAR; len: INTEGER): DynString;
    VAR a: INTEGER;
  BEGIN
    a := Strings.Length(source) - SHORT(len);
    IF a <= 0 THEN
      RETURN MakeDynString(source)
    ELSE
      RETURN Substr(source, a, len)
    END;
  END Right;

  PROCEDURE Lpad* (source: ARRAY OF CHAR; len: INTEGER; zeichen: CHAR): DynString;
    VAR
      z: ARRAY 2 OF CHAR;
      res: ARRAY 256 OF CHAR;
      l: INTEGER;
  BEGIN
    COPY(source, res);
    z := ' '; z[0] := zeichen;
    l := Strings.Length(res);
    WHILE l < len DO
      Strings.Concat(z, res, res); (*res := z$ + res$ *)
    END; (* verlängern *)
    RETURN Substr(res, 0, len); (* verkürzen  *)
  END Lpad;

  PROCEDURE Rpad* (source: ARRAY OF CHAR; len: INTEGER; zeichen: CHAR): DynString;
    VAR
      z: ARRAY 2 OF CHAR;
      res: ARRAY 256 OF CHAR;
      l: INTEGER;
  BEGIN
    COPY(source, res);
    z := ' '; z[0] := zeichen;
    l := Strings.Length(res);
    WHILE l < len DO
      Strings.Concat(res, z, res); (* res := res$ + z *)
    END; (* verlängern *)
    RETURN Substr(res, 0, len); (* verkürzen  *)
  END Rpad;

  PROCEDURE Trim* (source: ARRAY OF CHAR): DynString;
    VAR a, b, i, k, n, l: INTEGER; res: DynString;
  BEGIN
    l := Strings.Length(source);
    FOR i := 0 TO l - 1 DO (* Tabs gegen Blank tauschen *)
      IF source[i] = CHR(09) THEN source[i] := ' ' END;
    END;
    a := 0; b := Strings.Length(source) - 1;
    WHILE (a <= b) & (source[a] = ' ') DO a := a + 1 END;
    WHILE (a <= b) & (source[b] = ' ') DO b := b - 1 END;
    res := Substr(source, a, b - a + 1);
    n := Strings.Length(res^); (* maximal 1 Blank zwischen den Wörter *)
    FOR i := n - 1 TO 0 BY - 1 DO
      IF (res[i] = " ") & (res[i + 1] = " ") THEN
        FOR k := i TO n - 1 DO
          res[k] := res[k + 1]
        END;
      END;
    END;
    RETURN res;
  END Trim;

  PROCEDURE Before* (source: ARRAY OF CHAR; target: ARRAY OF CHAR): DynString;
    VAR a: INTEGER;
  BEGIN
    a := Pos(target, source);
    IF a = - 1 THEN
      RETURN MakeDynString(source)
    ELSE
      RETURN Left(source, a)
    END;
  END Before;

  PROCEDURE After* (source: ARRAY OF CHAR; target: ARRAY OF CHAR): DynString;
    VAR a: INTEGER;
  BEGIN
    a := Pos(target, source);
    IF a = - 1 THEN
      RETURN MakeDynString('')
    ELSE
      RETURN Right(source, Strings.Length(source) - a - Strings.Length(target))
    END;
  END After;

  PROCEDURE Parse* (VAR source: ARRAY OF CHAR; separator: ARRAY OF CHAR): DynString;
    VAR res, tmp: DynString;
  BEGIN
    res := Before(source, separator);
    NEW(tmp, LEN(source));
    tmp := After(source, separator);
    COPY(tmp^, source);
    RETURN res;
  END Parse;

  PROCEDURE ToUpper* (source: ARRAY OF CHAR): DynString;
    VAR target: ARRAY 256 OF CHAR;
  BEGIN
    COPY(source, target);
    Strings.Capitalize(target);
    RETURN MakeDynString(target);
  END ToUpper;

  PROCEDURE toLower(VAR stringVar: ARRAY OF CHAR);
  VAR
    i: INTEGER;
  BEGIN
    i := 0;
    WHILE stringVar[i] # 0X DO
      (* Check if the character is uppercase *)
      IF (stringVar[i] >= "A") & (stringVar[i] <= "Z") THEN
        (* Convert to lowercase by setting the 6th bit *)
        stringVar[i] := CHR(ORD(stringVar[i]) + 20H)
      END;
      INC(i)
    END
  END toLower;

  PROCEDURE ToLower* (source: ARRAY OF CHAR): DynString;
    VAR target: ARRAY 256 OF CHAR;
  BEGIN
    COPY(source, target);
    toLower(target);
    RETURN MakeDynString(target);
  END ToLower;

  PROCEDURE IntToString* (x: LONGINT): DynString;
    VAR s: ARRAY 256 OF CHAR;
  BEGIN
    IntStr.IntToStr(x, s);
    RETURN MakeDynString(s);
  END IntToString;

  PROCEDURE StringToInt* (s: ARRAY OF CHAR): LONGINT;
    VAR x: LONGINT; res: SHORTINT;
  BEGIN
    IntStr.StrToInt(s, x, res);
    RETURN x;
  END StringToInt;

  PROCEDURE StringToReal* (VAR s: ARRAY OF CHAR): REAL;
    VAR x: REAL; res: SHORTINT;
  BEGIN
    RealStr.StrToReal(s, x, res);
    RETURN x;
  END StringToReal;

  PROCEDURE RealToString* (x: REAL): DynString;
    VAR s: ARRAY 256 OF CHAR;
  BEGIN
    RealStr.RealToStr(x, s);
    RETURN MakeDynString(s);
  END RealToString;

END C2OStrings.

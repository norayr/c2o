(*-----------------------------------------------------------------------------
--   Unit Strings - Emulation of my old Turbo-Pascal string handling
--   (C) Helmut Zinn   V88/05/21 - V94/03/09 - V05/03/18
-----------------------------------------------------------------------------*)

MODULE C2cpStrings;

	

	IMPORT Strings;

	CONST TAB* = 09X; CR* = 0DX;

	TYPE DynString* = POINTER TO ARRAY OF CHAR;

	PROCEDURE MakeDynString* (s: ARRAY OF CHAR): DynString;
		VAR len: LONGINT; d: DynString;
	BEGIN
		len := LEN(s$);
		NEW(d, len + 1);
		d^ := s$;
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

	PROCEDURE Pos* (target, source: ARRAY OF CHAR): LONGINT;
		VAR pos: LONGINT;
	BEGIN
		Strings.Find(source, target, 0, pos); (* -- Note: Not found is pos := -1 (CP) instead of pos := 0 (TP) !!! *)
		RETURN pos;
	END Pos;

	PROCEDURE Substr* (source: ARRAY OF CHAR; a, len: LONGINT): DynString;
		VAR res: DynString;
	BEGIN
		NEW(res, len + 1);
		Strings.Extract(source, a, len, res^);
		RETURN res;
	END Substr;

	PROCEDURE Left* (source: ARRAY OF CHAR; len: LONGINT): DynString;
	BEGIN
		IF len >= LEN(source$) THEN
			RETURN MakeDynString(source$)
		ELSE
			RETURN Substr(source, 0, len)
		END;
	END Left;

	PROCEDURE Right* (source: ARRAY OF CHAR; len: LONGINT): DynString;
		VAR a: LONGINT;
	BEGIN
		a := LEN(source$) - len;
		IF a <= 0 THEN
			RETURN MakeDynString(source$)
		ELSE
			RETURN Substr(source, a, len)
		END;
	END Right;

	PROCEDURE Lpad* (source: ARRAY OF CHAR; len: LONGINT; zeichen: CHAR): DynString;
		VAR
			z: ARRAY 2 OF CHAR;
			res: ARRAY 256 OF CHAR;
	BEGIN
		res := source$;
		z := ' '; z[0] := zeichen;
		WHILE LEN(res$) < len DO res := z$ + res$ END; (* verlängern *)
		RETURN Substr(res, 0, len); (* verkürzen  *)
	END Lpad;

	PROCEDURE Rpad* (source: ARRAY OF CHAR; len: LONGINT; zeichen: CHAR): DynString;
		VAR
			z: ARRAY 2 OF CHAR;
			res: ARRAY 256 OF CHAR;
	BEGIN
		res := source$;
		z := ' '; z[0] := zeichen;
		WHILE LEN(res$) < len DO res := res$ + z END; (* verlängern *)
		RETURN Substr(res, 0, len); (* verkürzen  *)
	END Rpad;

	PROCEDURE Trim* (source: ARRAY OF CHAR): DynString;
		VAR a, b, i, k, n: LONGINT; res: DynString;
	BEGIN
		FOR i := 0 TO LEN(source$) - 1 DO (* Tabs gegen Blank tauschen *)
			IF source[i] = CHR(09) THEN source[i] := ' ' END;
		END;
		a := 0; b := LEN(source$) - 1;
		WHILE (a <= b) & (source[a] = ' ') DO a := a + 1 END;
		WHILE (a <= b) & (source[b] = ' ') DO b := b - 1 END;
		res := Substr(source, a, b - a + 1);
		n := LEN(res$); (* maximal 1 Blank zwischen den Wörter *)
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
		VAR a: LONGINT;
	BEGIN
		a := Pos(target, source);
		IF a = - 1 THEN
			RETURN MakeDynString(source$)
		ELSE
			RETURN Left(source, a)
		END;
	END Before;

	PROCEDURE After* (source: ARRAY OF CHAR; target: ARRAY OF CHAR): DynString;
		VAR a: LONGINT;
	BEGIN
		a := Pos(target, source);
		IF a = - 1 THEN
			RETURN MakeDynString('')
		ELSE
			RETURN Right(source, LEN(source$) - a - LEN(target$))
		END;
	END After;

	PROCEDURE Parse* (VAR source: ARRAY OF CHAR; separator: ARRAY OF CHAR): DynString;
		VAR res: DynString;
	BEGIN
		res := Before(source, separator);
		source := After(source, separator)$;
		RETURN res;
	END Parse;

	PROCEDURE ToUpper* (source: ARRAY OF CHAR): DynString;
		VAR target: ARRAY 256 OF CHAR;
	BEGIN
		Strings.ToUpper(source, target);
		RETURN MakeDynString(target$);
	END ToUpper;

	PROCEDURE ToLower* (source: ARRAY OF CHAR): DynString;
		VAR target: ARRAY 256 OF CHAR;
	BEGIN
		Strings.ToLower(source, target);
		RETURN MakeDynString(target$);
	END ToLower;

	PROCEDURE IntToString* (x: LONGINT): DynString;
		VAR s: ARRAY 256 OF CHAR;
	BEGIN
		Strings.IntToString(x, s);
		RETURN MakeDynString(s$);
	END IntToString;

	PROCEDURE StringToInt* (s: ARRAY OF CHAR): LONGINT;
		VAR x, res: LONGINT;
	BEGIN
		Strings.StringToInt(s, x, res);
		RETURN x;
	END StringToInt;

	PROCEDURE StringToReal* (IN s: ARRAY OF CHAR): REAL;
		VAR x: REAL; res: LONGINT;
	BEGIN
		Strings.StringToReal(s, x, res);
		RETURN x;
	END StringToReal;

	PROCEDURE RealToString* (x: REAL): DynString;
		VAR s: ARRAY 256 OF CHAR;
	BEGIN
		Strings.RealToString(x, s);
		RETURN MakeDynString(s$);
	END RealToString;

END C2cpStrings.

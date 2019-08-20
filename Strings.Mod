(*---------------------------------------------------------------------------
--   Dynamic string handling (Turbo Library) 
--   © Helmut Zinn   V88/05/21 - V94/03/09 - V05/03/18
--   2013 including Luowy's improvement
---------------------------------------------------------------------------*)

MODULE C2cpStrings;

	

	IMPORT Strings;

	CONST 
		tab* = 09X; 
		line* = 0DX;

	TYPE String* = POINTER TO ARRAY OF CHAR;

	PROCEDURE NewString* (IN str: ARRAY OF CHAR): String;
		VAR s: String;
	BEGIN
		NEW(s, LEN(str$) + 1);
		s^ := str$;
		RETURN s;
	END NewString; 

	PROCEDURE NewStringFillChar* (fill: CHAR; len: INTEGER): String;
		VAR i: INTEGER; s: String;
	BEGIN
		NEW(s, len + 1);
		FOR i := 0 TO len - 1 DO s[i] := fill END;
		s[len] := 0X;
		RETURN s;
	END NewStringFillChar;


	(*======== chars =======================*)

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
		RETURN (ch = ' ') OR (ch = tab);
	END IsSpace;


	(*======== dynamic strings ========*)

	PROCEDURE Pos* (IN target, source: ARRAY OF CHAR): INTEGER;
		VAR pos: INTEGER;
	BEGIN
		Strings.Find(source, target, 0, pos); (* Note: Not found is pos := -1 (CP) instead of pos := 0 (TP) !!! *)
		RETURN pos;
	END Pos;

	PROCEDURE SubStr* (IN source: ARRAY OF CHAR; pos, len: INTEGER): String;
		VAR s: String;
	BEGIN
		NEW(s, len + 1);
		Strings.Extract(source, pos, len, s^);
		RETURN s;
	END SubStr;

	PROCEDURE Left* (IN source: ARRAY OF CHAR; len: INTEGER): String;
	BEGIN
		IF len >= LEN(source$) THEN
			RETURN NewString(source)
		ELSE
			RETURN SubStr(source, 0, len)
		END;
	END Left;

	PROCEDURE Right* (IN source: ARRAY OF CHAR; len: INTEGER): String;
		VAR a: INTEGER;
	BEGIN
		a := LEN(source$) - len;
		IF a <= 0 THEN
			RETURN NewString(source)
		ELSE
			RETURN SubStr(source, a, len)
		END;
	END Right;

	PROCEDURE PadLeft* (IN source: ARRAY OF CHAR; len: INTEGER; pad: CHAR): String;
		VAR
			s: String; i, j, slen: INTEGER;
	BEGIN
		NEW(s, len + 1);
		slen := LEN(source$);
		i := 0; WHILE slen < len DO s[i] := pad; INC(i); INC(slen) END; (* append pad left *)
		j := 0; WHILE (i < len) DO s[i] := source[j]; INC(i); INC(j) END; (* copy source *)
		s[i] := 0X;
		RETURN s;
	END PadLeft;

	PROCEDURE PadRight* (IN source: ARRAY OF CHAR; len: INTEGER; pad: CHAR): String;
		VAR
			s: String; i, slen: INTEGER;
	BEGIN
		NEW(s, len + 1);
		slen := LEN(source$);
		i := 0; WHILE (i < len) & (i < slen) DO s[i] := source[i]; INC(i) END; (* copy source *)
		WHILE i < len DO s[i] := pad; INC(i) END; (* append pad right *)
		s[i] := 0X;
		RETURN s;
	END PadRight;

	PROCEDURE Trim* (IN source: ARRAY OF CHAR): String;
		VAR
			a, b, i, n: INTEGER; s: String; 
			ch, pch: CHAR;
	BEGIN
		a := 0; b := LEN(source$) - 1;
		WHILE (a <= b) & ((source[a] = ' ') OR (source[a] = tab)) DO INC(a) END;
		WHILE (a <= b) & ((source[b] = ' ') OR (source[b] = tab)) DO DEC(b) END;
		i := a; pch := 0X; n := 0;
		WHILE (i < b)DO
			ch := source[i]; 
			IF ch = tab THEN ch := ' ' END; (* change tabs to spaces *)
			IF (ch = ' ') & (pch = ' ') THEN INC(n) END; (* maximum 1 blank between the words *)
			pch := ch; INC(i);
		END;
		(* after knowing the length copy source *)
		NEW(s, b - a - n + 2);
		i := 0; pch := ' ';
		WHILE a <= b DO
			ch := source[a]; 
			IF ch = tab THEN ch := ' ' END; (* change tabs to spaces *)
			IF (ch # ' ') OR (pch # ' ') THEN s[i] := ch; INC(i) END; (* maximum 1 blank between the words *)
			pch := ch; INC(a);
		END; 
		s[i] := 0X;
		RETURN s;
	END Trim;

	PROCEDURE Before* (IN source, target: ARRAY OF CHAR): String;
		VAR a: INTEGER;
	BEGIN
		a := Pos(target, source);
		IF a =  - 1 THEN
			RETURN NewString(source)
		ELSE
			RETURN Left(source, a)
		END;
	END Before;

	PROCEDURE After* (IN source, target: ARRAY OF CHAR): String;
		VAR a: INTEGER;
	BEGIN
		a := Pos(target, source);
		IF a =  - 1 THEN
			RETURN NewString('')
		ELSE
			RETURN Right(source, LEN(source$) - a - LEN(target$))
		END;
	END After;

	PROCEDURE Parse* (VAR source: ARRAY OF CHAR; IN separator: ARRAY OF CHAR): String;
		VAR res: String;
	BEGIN
		res := Before(source, separator);
		source := After(source, separator)$;
		RETURN res;
	END Parse;

	PROCEDURE ToUpper* (IN source: ARRAY OF CHAR): String;
		VAR target: ARRAY 256 OF CHAR;
	BEGIN
		Strings.ToUpper(source, target);
		RETURN NewString(target);
	END ToUpper;

	PROCEDURE ToLower* (IN source: ARRAY OF CHAR): String;
		VAR target: ARRAY 256 OF CHAR;
	BEGIN
		Strings.ToLower(source, target);
		RETURN NewString(target);
	END ToLower;

	PROCEDURE IntToString* (x: INTEGER): String;
		VAR s: ARRAY 256 OF CHAR;
	BEGIN
		Strings.IntToString(x, s);
		RETURN NewString(s);
	END IntToString;

	PROCEDURE StringToInt* (s: ARRAY OF CHAR): INTEGER;
		VAR x, res: INTEGER;
	BEGIN
		Strings.StringToInt(s, x, res);
		RETURN x;
	END StringToInt;

	PROCEDURE StringToReal* (IN s: ARRAY OF CHAR): REAL;
		VAR x: REAL; res: INTEGER;
	BEGIN
		Strings.StringToReal(s, x, res);
		RETURN x;
	END StringToReal;

	PROCEDURE RealToString* (x: REAL): String;
		VAR s: ARRAY 256 OF CHAR;
	BEGIN
		Strings.RealToString(x, s);
		RETURN NewString(s);
	END RealToString;

END C2cpStrings.

(*-----------------------------------------------------------------------
--   Unit Files - Emulation of Turbo-Pascal file handling
--   © Helmut Zinn   V05/03/19 - V05/05/12
-----------------------------------------------------------------------*)

MODULE C2cpFiles;

	

	IMPORT
		C2cpStrings, C2cpTrace, Converters, Documents, Files,
		StdFolds, StdLog, Stores, TextMappers, TextModels, TextViews, Views;

	CONST
		cName* = 1; cString* = 2; cInteger* = 3; cReal* = 4; cChar* = 5; cSpace* = 6; cEOL* = 7;

	TYPE
		Scanner* = POINTER TO RECORD
			rd-: Stores.Reader;
			lineNumber-: INTEGER;
			nextCh-: CHAR;
			class*: INTEGER;
			i-: LONGINT;
			x-: REAL;
			s*: ARRAY 256 OF CHAR;
		END;

		Text* = POINTER TO RECORD
			done-: BOOLEAN; 	(* last operation was successful *)
			exist-: BOOLEAN; 	(* file already exist *)
			reading, writing: BOOLEAN;
			loc: Files.Locator;
			fname: Files.Name;
			conv: Converters.Converter;
			vw-: Views.View;
			md-: TextModels.Model;
			sc-: TextMappers.Scanner;
			fm*: TextMappers.Formatter;
		END;

	VAR
		convtxt*: Converters.Converter;

	PROCEDURE SplitFilename* (IN filename: ARRAY OF CHAR; OUT path, name, extension: ARRAY OF CHAR);
		VAR i, n, pa, pe: INTEGER;
	BEGIN
		n := LEN(filename$);
		pa :=  - 1;
		pe := n;
		FOR i := 0 TO n - 1 DO
			IF (filename[i] = '/') OR (filename[i] = '\') THEN pa := i; pe := n
			ELSIF (filename[i] = '.') THEN pe := i
			END;
		END;
		path := C2cpStrings.SubStr(filename, 0, pa + 1)$;
		name := C2cpStrings.SubStr(filename, pa + 1, pe - pa - 1)$;
		extension := C2cpStrings.SubStr(filename, pe + 1, n - pe - 1)$;
	END SplitFilename;

	PROCEDURE Exist* (IN path: ARRAY OF CHAR; IN old: Files.Name): BOOLEAN;
		VAR loc: Files.Locator; file: Files.File;
	BEGIN
		loc := Files.dir.This(path);
		file := Files.dir.Old(loc, old, Files.shared);
		RETURN file # NIL;
	END Exist;

	PROCEDURE PathToFileSpec (IN path: ARRAY OF CHAR; OUT loc: Files.Locator; OUT name: Files.Name);
		VAR i, j: INTEGER; ch: CHAR;
	BEGIN
		i := 0; j := 0; loc := Files.dir.This('');
		WHILE (loc.res = 0) & (i < LEN(path) - 1) & (j < LEN(name) - 1) & (path[i] # 0X) DO
			ch := path[i]; INC(i);
			IF (j > 0) & ((ch = '/') OR (ch = '\')) THEN name[j] := 0X; j := 0; loc := loc.This(name)
			ELSE name[j] := ch; INC(j)
			END
		END;
		IF path[i] = 0X THEN name[j] := 0X ELSE loc.res := 1; name := '' END;
	END PathToFileSpec;

	PROCEDURE GetFileConverter (IN filename: ARRAY OF CHAR; OUT conv: Converters.Converter);
		VAR
			path, name, extension: ARRAY 256 OF CHAR;
	BEGIN
		SplitFilename(filename, path, name, extension); 
		extension := C2cpStrings.ToLower(extension)$;
		IF extension$ = 'odc' THEN
			conv := NIL
		ELSE
			conv := Converters.list; 
			WHILE (conv # NIL) & (conv.fileType # extension) DO conv := conv.next END;
			IF conv = NIL THEN conv := convtxt END;
		END;
	END GetFileConverter;

	(*-------------------------------------------------------------------------
	--	Scanner
	--	VAR sc: C2cpFiles.Scanner;
	--
	--	NEW(sc)
	--	sc.OpenScanner
	--	sc.Scan
	--	WHILE ~sc.Eof() DO
	--		...
	--		sc.Scan
	--	END
	--	sc.CloseScanner
	-------------------------------------------------------------------------*)

	PROCEDURE (sc: Scanner) OpenScanner* (filename: ARRAY OF CHAR), NEW;
		VAR
			loc: Files.Locator;
			name: Files.Name;
			fi: Files.File;
			c: SHORTCHAR;
	BEGIN
		sc.lineNumber := 1;
		(* Assign filename *)
		PathToFileSpec(filename, loc, name);
		fi := Files.dir.Old(loc, name, Files.shared);
		(* Reset *)
		sc.rd.ConnectTo(fi);
		sc.rd.SetPos(0);
		(* look ahead *)
		sc.rd.ReadSChar(c); sc.nextCh := c;
	END OpenScanner;

	PROCEDURE (sc: Scanner) Eof* (): BOOLEAN, NEW;
	BEGIN
		RETURN sc.rd.rider.eof
	END Eof;

	PROCEDURE (sc: Scanner) Scan*, NEW;
		CONST LF = 0AX;
		VAR ch: CHAR; hex: BOOLEAN;

		PROCEDURE ReadChar;
			VAR n: INTEGER; c: SHORTCHAR;
		BEGIN
			n := LEN(sc.s$); sc.s[n] := ch; sc.s[n + 1] := 0X;
			sc.rd.ReadSChar(c); ch := c;
			IF ch = LF THEN ch := C2cpStrings.line END; (* change LF to CR *)
		END ReadChar;

	BEGIN
		ch := sc.nextCh;
		sc.s := '';
		IF (ch = C2cpStrings.line) OR (ch = LF) THEN
			ch := C2cpStrings.line; (* change LF to CR *)
			sc.class := cEOL;
			INC(sc.lineNumber); (* Neue Zeilennummer schon bei CR *)
			ReadChar; (* add CR - lese LF *)
			IF (ch = C2cpStrings.line) OR (ch = LF) THEN (* Skip LF *)
				ReadChar; (* lese Zeichen nach LF *)
				sc.s[LEN(sc.s$) - 1] := 0X; (* delete LF *)
			END;
		ELSIF C2cpStrings.IsSpace(ch) THEN
			sc.class := cSpace;
			WHILE C2cpStrings.IsSpace(ch) DO ReadChar END;
		ELSIF C2cpStrings.IsAlpha(ch) THEN
			sc.class := cName;
			WHILE C2cpStrings.IsAlpha(ch) OR C2cpStrings.IsDigit(ch) (* OR (ch = '.') *) DO ReadChar END;
			(* --wegen CBiber-- *)
		ELSIF ch = "'" THEN
			sc.class := cString;
			REPEAT
				ReadChar;
			UNTIL (ch = "'") OR (ch = C2cpStrings.line) OR (ch = LF);
			IF ch = "'" THEN ReadChar END;
		ELSIF ch = '"' THEN
			sc.class := cString;
			REPEAT
				ReadChar;
			UNTIL (ch = '"') OR (ch = C2cpStrings.line) OR (ch = LF);
			IF ch = '"' THEN ReadChar END;
		ELSE
			IF (ch = '+') OR (ch = '-') OR (ch = '$') THEN ReadChar END;
			IF C2cpStrings.IsDigit(ch) OR (sc.s = '$') & C2cpStrings.IsHex(ch) THEN
				sc.class := cInteger;
				IF sc.s = '$' THEN hex := TRUE ELSE hex := FALSE END;
				WHILE C2cpStrings.IsDigit(ch) OR C2cpStrings.IsHex(ch) DO
					IF C2cpStrings.IsHex(ch) THEN hex := TRUE END;
					ReadChar;
				END;
				IF C2cpStrings.Cap(ch) = 'H' THEN
					hex := TRUE;
					ReadChar;
				END;
				IF hex THEN sc.i := C2cpStrings.StringToInt(sc.s)
				ELSE sc.i := C2cpStrings.StringToInt(sc.s)END;
				IF (ch = '.') & ~ hex THEN
					sc.class := cReal;
					ReadChar;
					WHILE C2cpStrings.IsDigit(ch) DO ReadChar END;
					IF ch = 'E' THEN
						ReadChar;
						IF (ch = '+') OR (ch = '-') THEN ReadChar END;
						WHILE C2cpStrings.IsDigit(ch) DO ReadChar END;
					END;
					sc.x := C2cpStrings.StringToReal(sc.s);
				END;
			ELSE
				sc.class := cChar;
				IF sc.s = '' THEN ReadChar END;
			END;
		END;
		sc.nextCh := ch;
		C2cpTrace.Token(sc.lineNumber, sc.s);
	END Scan;

	PROCEDURE (sc: Scanner) CloseScanner*, NEW;
	BEGIN
		sc.rd.ConnectTo(NIL);
		C2cpTrace.CloseDebug;
	END CloseScanner;

	(*-------------------------------------------------------------------------
	--	Text Interface for Turbo Library Emulation
	--
	--  VAR text: C2cpFiles.Text;
	--
	--  NEW(text);
	--	IF text.done THEN ...
	--	IF text.exist THEN ...
	--
	--  text.Assign (IN filename: ARRAY OF CHAR);
	--  text.AssignCrt;
	--  text.Append;
	--  text.Eof(): BOOLEAN;
	--  text.Flush;
	--  text.Close;
	--  text.Debug;
	--
	--  text.Reset;
	--	text.sc.xxx
	--
	--  text.Rewrite;
	--	text.fm.xxx
	-------------------------------------------------------------------------*)

	PROCEDURE (text: Text) Assign* (IN filename: ARRAY OF CHAR), NEW;
	BEGIN
		PathToFileSpec(filename, text.loc, text.fname);
		GetFileConverter(filename, text.conv);
		text.vw := Views.Old(Views.dontAsk, text.loc, text.fname, text.conv);
		IF text.vw = NIL THEN
			text.exist := FALSE;
			text.md := TextModels.dir.New();
			text.vw := TextViews.dir.New(text.md);
		ELSE
			text.exist := TRUE;
			IF text.vw IS Documents.Document THEN text.vw := text.vw(Documents.Document).ThisView() END;
			text.md := text.vw(TextViews.View).ThisModel();
		END;
		text.done := text.loc.res = 0;
		text.reading := FALSE;
		text.writing := FALSE;
	END Assign;

	PROCEDURE (text: Text) AssignCrt*, NEW;
	BEGIN
		text.md := StdLog.text; 	(* text.md := TextModels.dir.New(); *)
		text.vw := TextViews.dir.New(text.md);
		text.sc.ConnectTo(text.md);
		text.fm.ConnectTo(text.md);
		text.fm.SetPos(text.md.Length()); 	(* set the formatter to the end of the text *)
		StdLog.Open; 	(* Views.OpenView(text.vw); *)
		text.reading := TRUE;
		text.writing := TRUE;
		text.done := TRUE;
	END AssignCrt;

	PROCEDURE (text: Text) Debug*, NEW;
	BEGIN
		Views.OpenView(text.vw); (* open the text view for debugging *)
	END Debug;

	PROCEDURE (text: Text) Reset*, NEW;
	BEGIN
		text.sc.ConnectTo(text.md);
		text.reading := TRUE;
		text.done := TRUE;
	END Reset;

	PROCEDURE (text: Text) Rewrite*, NEW;
	BEGIN
		text.fm.ConnectTo(text.md);
		text.fm.SetPos(0); 	(* set the formatter to the beginning of the text *)
		text.md.Delete(text.fm.Pos(), text.md.Length()); 	(* delete all old text *)
		text.writing := TRUE;
		text.done := TRUE;
	END Rewrite;

	PROCEDURE (text: Text) Eof* (): BOOLEAN, NEW;
	BEGIN
		ASSERT(text.reading, 20);
		RETURN text.sc.rider.eot
	END Eof;

	PROCEDURE (text: Text) Flush*, NEW;
		VAR res: INTEGER;
	BEGIN
		ASSERT(text.writing, 20);
		Views.Register(text.vw, Views.dontAsk, text.loc, text.fname, text.conv, res);
		IF res # 0 THEN text.Debug END; (* on error open the text view in a window *)
		IF res = 0 THEN text.done := TRUE ELSE text.done := FALSE END;
	END Flush;

	PROCEDURE (text: Text) SetColor* (color: INTEGER), NEW;
		VAR
			new: TextModels.Attributes;
	BEGIN
		new := text.fm.rider.attr;
		new := TextModels.NewColor(new, color);
		text.fm.rider.SetAttr(new); 	(* change current attributes of formatter *)
	END SetColor;

	PROCEDURE (text: Text) WriteOpenFold* (IN shortForm: ARRAY OF CHAR), NEW;
		VAR
			fold: StdFolds.Fold;
			t: TextModels.Model;
	BEGIN
		t := TextModels.dir.NewFromString(shortForm); (* convert a string into a text model *)
		fold := StdFolds.dir.New(StdFolds.expanded, "", t);
		text.fm.WriteView(fold);
		text.fm.WriteChar(' ');
	END WriteOpenFold;

	PROCEDURE (text: Text) WriteCloseFold*, NEW;
		VAR
			len: INTEGER;
			fold: StdFolds.Fold;
	BEGIN
		fold := StdFolds.dir.New(StdFolds.expanded, "", NIL);
		text.fm.WriteView(fold);
		fold.Flip; (* swap long-form text, now between the two fold views, with hidden short-form text *)
		len := text.fm.rider.Base().Length(); (* determine the text carrier's new length *)
		text.fm.SetPos(len); (* position the formatter to the end of the text *)
		text.fm.WriteChar(' ');
	END WriteCloseFold;

	PROCEDURE (text: Text) Close*, NEW;
		VAR res: INTEGER;
	BEGIN
		res := 0;
		IF text.reading THEN
			text.sc.ConnectTo(NIL);
			text.reading := FALSE;
		END;
		IF text.writing THEN
			Views.Register(text.vw, Views.dontAsk, text.loc, text.fname, text.conv, res);
			IF res # 0 THEN text.Debug END; (* on error open the text view in a window *)
			text.fm.ConnectTo(NIL);
			text.writing := FALSE;
		END;
		IF res = 0 THEN text.done := TRUE ELSE text.done := FALSE END;
	END Close;

BEGIN
	convtxt := Converters.list;
	WHILE (convtxt # NIL) & (convtxt.fileType # "txt") DO
		convtxt := convtxt.next
	END;
END C2cpFiles.

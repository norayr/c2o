(*-----------------------------------------------------------------------------
--   Von C nach Pascal übersetzen
--   (C) Helmut Zinn   V94/04/06 - V95/11/07 - V05/05/12
-----------------------------------------------------------------------------*)

MODULE C2cpCmds;

	

	IMPORT
		C2cpCBiber, C2cpFiles, C2cpStrings, C2cpTable,
		Controllers, HostCmds, HostFiles, StdCmds, StdLog, Windows;

	CONST
		debug = FALSE;
		bibdir = 'C2cp/Transl/'; (* Pfad der Übersetzungsbiblothek *)

	VAR
		pfad: ARRAY 256 OF CHAR; (* Pfad des zu übersetzenden Programmes *)
		out, hdr, tmp: C2cpFiles.Text; (* Ausgabedatei für odc & FORWARD-Definitionen *)
		forwardpos: LONGINT;

	PROCEDURE PreLoad (IN name: ARRAY OF CHAR);
		VAR i: LONGINT;
	BEGIN
		(* reading pre definition symbol table file *)
		IF C2cpFiles.Exist(pfad, name + '.pre') THEN
			C2cpTable.Load(pfad, name + '.pre');
			(* reading the needed symbol table library *)
			FOR i := 1 TO C2cpTable.numberOfSym DO
				IF C2cpTable.GetSign(i) = 'H' THEN
					C2cpTable.Load(bibdir, C2cpTable.GetIdentifier(i) + '.hz');
				END;
			END;
		END;
	END PreLoad;

	PROCEDURE InsertAt (pos: LONGINT; clipboard: C2cpFiles.Text);
	BEGIN
		out.md.Insert(pos, clipboard.md, 0, clipboard.md.Length()); out.fm.SetPos(out.md.Length());
	END InsertAt;

	PROCEDURE WriteBuffer (out: C2cpFiles.Text; IN aufgabe: ARRAY OF CHAR);
		VAR i: LONGINT;
	BEGIN
		FOR i := 0 TO LEN(aufgabe$) - 1 DO
			IF aufgabe[i] = C2cpStrings.TAB THEN out.fm.WriteTab;
			ELSIF aufgabe[i] = C2cpStrings.CR THEN out.fm.WriteLn;
			ELSE out.fm.WriteChar(aufgabe[i]);
			END;
		END;
	END WriteBuffer;

	PROCEDURE WriteHeader (out, hdr: C2cpFiles.Text);
		VAR i, n: LONGINT; name: ARRAY 256 OF CHAR;
	BEGIN
		InsertAt(0, hdr);
		name := C2cpTable.GetAlias(1)$;
		out.fm.WriteString('MODULE '); out.fm.WriteString(name); out.fm.WriteString(';'); out.fm.WriteLn;
		out.fm.WriteLn;
		out.fm.WriteTab; out.fm.WriteString('IMPORT ');
		FOR i := 0 TO C2cpTable.numberOfSym - 1 DO
			n := C2cpTable.AlphaOrder(i);
			IF C2cpTable.GetSign(n) = 'M' THEN
				name := C2cpTable.GetAlias(n)$;
				IF name # C2cpTable.GetIdentifier(1)$ THEN
					out.fm.WriteString(name); out.fm.WriteString(', ')
				END;
			END;
		END;
		out.fm.WriteString('StdLog;'); out.fm.WriteLn;
		out.fm.WriteLn; out.fm.WriteTab;
		forwardpos := out.fm.rider.Pos();
	END WriteHeader;

	PROCEDURE WriteTrailer (out, tmp: C2cpFiles.Text);
		VAR name: ARRAY 256 OF CHAR;
	BEGIN
		name := C2cpTable.GetAlias(1)$;
		out.fm.WriteLn;
		out.fm.WriteString('END '); out.fm.WriteString(name); out.fm.WriteString('.'); out.fm.WriteLn;
		tmp.fm.WriteLn; tmp.fm.WriteTab;
		InsertAt(forwardpos, tmp);
	END WriteTrailer;

	PROCEDURE Pass1 (IN quelle: ARRAY OF CHAR);
		VAR
			s: C2cpFiles.Scanner; zustand: LONGINT; aufgabe: ARRAY 512 OF CHAR;
	BEGIN
		StdLog.String('   ' + pfad + quelle); StdLog.Ln;
		IF C2cpFiles.Exist(pfad, quelle$) THEN
			(* create symbol table via reading source file *)
			NEW(s);
			s.OpenScanner(pfad + quelle);
			zustand := 1;
			WHILE ~ s.Eof() DO
				s.Scan;
				C2cpCBiber.Biber(s, zustand, aufgabe);
				IF zustand = 17 THEN
					Pass1(aufgabe);
				END;
			END;
			s.CloseScanner;
		ELSE
			(* create symboltable via reading library *.hz file *)
			C2cpTable.Load(bibdir, quelle + 'z');
		END;
	END Pass1;

	PROCEDURE Pass2 (IN quelle: ARRAY OF CHAR);
		VAR
			s: C2cpFiles.Scanner; zustand: LONGINT; aufgabe: ARRAY 512 OF CHAR;
	BEGIN
		StdLog.String('   ' + pfad + quelle); StdLog.Ln;
		NEW(s);
		s.OpenScanner(pfad + quelle);
		zustand := 1;
		WHILE ~ s.Eof() DO
			s.Scan;
			C2cpCBiber.Biber(s, zustand, aufgabe);
			IF debug THEN
				StdLog.Int(s.line); StdLog.String(': '); StdLog.String(s.s);
				StdLog.Int(zustand); StdLog.String(': '); StdLog.String(aufgabe); StdLog.Ln;
			END;
			IF zustand = 13 THEN forwardpos := out.fm.rider.Pos() END;
			WriteBuffer(out, aufgabe);
			IF zustand = 17 THEN
				IF C2cpFiles.Exist(pfad, aufgabe$) THEN
					out.fm.WriteString(' ====================================> START <=== *)');
					out.fm.WriteLn; out.fm.WriteTab;
					Pass2(aufgabe);
					out.fm.WriteString('(* #include '); out.fm.WriteString(aufgabe);
					out.fm.WriteString(' ====================================> -END- <=== ');
				END;
			END;
			IF (zustand >= 7) & (zustand <= 9) THEN (*7*) (*8*) (*9*)
				WriteBuffer(tmp, aufgabe);
				IF zustand = 7 THEN tmp.fm.WriteString('^ ') END;
				IF zustand = 9 THEN tmp.fm.WriteLn; tmp.fm.WriteTab; END;
			END;
		END;
		s.CloseScanner;
		StdLog.String('  '); StdLog.Int(s.line); StdLog.String(' lines readed'); StdLog.Ln;
	END Pass2;

	PROCEDURE TranslateFile* (IN filename: ARRAY OF CHAR);
		VAR
			project, ext, source: ARRAY 256 OF CHAR;
	BEGIN
		StdLog.Ln; StdLog.String('Von C nach Pascal übersetzen'); StdLog.Ln;
		C2cpFiles.SplitFilename(filename$, pfad, project, ext);
		source := project + '.' + ext;
		IF C2cpFiles.Exist(pfad, source$) THEN (* Aufgabe ausführen *)
			(* Pass 1 - create symbol table *)
			StdLog.String('> Pass 1: Create symbol table'); StdLog.Ln;
			C2cpTable.Dispose;
			C2cpTable.Insert('0', project, '', '', '', 'M', 1); (* the module name is always the first entry *)
			PreLoad(project);
			Pass1(source);
			C2cpTable.Pruefen;
			(* Pass 2 - translate program *)
			StdLog.String('> Pass 2: Translate'); StdLog.Ln;
			NEW(hdr); hdr.Assign('C2cp/Rsrc/Header.odc'); hdr.Reset;
			NEW(out); out.Assign(pfad + project + '.odc'); out.Rewrite;
			NEW(tmp); tmp.Assign(pfad + project + '.for'); tmp.Rewrite;
			WriteHeader(out, hdr);
			Pass2(source);
			WriteTrailer(out, tmp);
			out.Close;
			C2cpTable.Save(pfad, project + '.csy');
			StdLog.String('Aufgabe ausgeführt'); StdLog.Ln;
		ELSE
			StdLog.String('Datei '); StdLog.String(pfad + source); StdLog.String(' nicht gefunden'); StdLog.Ln;
		END;
		StdLog.Ln;
	END TranslateFile;

	PROCEDURE Translate*;
		VAR
			w: Windows.Window;
	BEGIN
		w := Windows.dir.Focus(Controllers.targetPath);
		IF w # NIL THEN
			IF w.seq.Dirty() THEN HostCmds.Save END;
			(* loc := w.loc; name := w.name; conv := w.conv; *)
			TranslateFile(w.loc(HostFiles.Locator).path + '\' + w.name);
			StdCmds.OpenDoc(w.loc(HostFiles.Locator).path + '\' + C2cpStrings.Before(w.name, '.'));
		END;
	END Translate;

END C2cpCmds.

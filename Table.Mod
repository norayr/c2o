(*-----------------------------------------------------------------------------
--   Symbol Tabelle verwalten
--   (C) Helmut Zinn   V90/03/24 - V95/12/13 - V05/05/12
-------------------------------------------------------------------------------
--   Search       Search for qualident & identifier in the table - return its index.
--                Schlüssel für Name holen.
--   Insert       Insert qualident, identifier, type and alias into the table.
--                Neue Daten in Tabelle eintragen.
--   Update       Update at qualident, identifier the type and the alias.
--                Vorhandene Daten in Tabelle ändern.
--
--   Merken       Zeilennummer des Eintrages merken.
--   Reserviert   Auf reservierte Wörter prüfen.
--   Pruefen      Alias-Namen für doppelte Namen vergeben.
--
--   AlphaOrder   Change the index to alpha order index.
--                Welches Wort an diesem Platz wäre, wenn alpha sortiert.
--   GetQualident Get the qualident from the symbol table.
--                Gruppen-Name zum Schlüssel holen.
--   GetIdentifier     Get the identifier from the symbol table.
--                Name zum Schlüssel holen.
--   GetAlias     Get the alias name of the identifier from the symbol table.
--                Alias-Name zum Schlüssel holen.
--   GetType      Get the type of the identifier from the symbol table.
--                Type zum Schlüssel holen.
--   GetInfo      Info zum Schlüssel holen.
--   GetSign      Kennzeichen zum Schlüssel holen.
--   GetLinenumber Zeilennummer zum Schlüssel holen.
--
--   FindFirst    Erster Aliasname einer Gruppe mit Zeichen = 'x' holen.
--   FindNext     Nächster Aliasname einer Gruppe mit Zeichen = 'x' holen.
--
--   Load         Load the symbol table from file.	 [qualidant].name[:type][/alias]
--                Symboltabelle von Datei laden.
--   Save         Save the symbol table to file.	   '/' = [Tab]
--                Symboltabelle in  Datei sichern.
--   ShowSymList  List symbol table for debugging.
--                Die gesamte Schlüsseltabelle für Testzwecke ausgeben.
--   Dispose      Give the table memory free - empty table for new use.
--                Symboltabelle im Speicher löschen.
-----------------------------------------------------------------------------*)

MODULE C2cpTable;

	

	IMPORT
		C2cpKeytab, C2cpStrings, 
		Converters, Files, Ports, StdLog, TextMappers, TextModels, TextRulers, TextViews, Views;

	CONST
		maxSym = C2cpKeytab.maxKey DIV 2; (* maxKey = 2000; in Keytab definiert *)

	VAR
		numberOfSym-: LONGINT;
		symtable: ARRAY maxSym + 1 OF RECORD
			index: LONGINT;
			qualident: LONGINT;
			identifier: LONGINT;
			alias: LONGINT;
			ntype: LONGINT;
			info: LONGINT;
			sign: CHAR;
			linenumber: LONGINT;
		END;

		pos: LONGINT; (* Suchposition für FindFirst und FindNext *)
		gru: ARRAY 256 OF CHAR; (* Suchgruppe   für FindFirst und FindNext *)
		zei: CHAR; (* Suchzeichen  für FindFirst und FindNext *)
		conv: Converters.Converter; (* reading & writing txt files *)

	PROCEDURE AlphaOrder* (index: LONGINT): LONGINT;
	BEGIN
		RETURN symtable[index].index;
	END AlphaOrder;

	PROCEDURE GetQualident* (index: LONGINT): C2cpStrings.DynString;
	BEGIN
		RETURN C2cpKeytab.GetKeyword(symtable[index].qualident)
	END GetQualident;

	PROCEDURE GetIdentifier* (index: LONGINT): C2cpStrings.DynString;
	BEGIN
		RETURN C2cpKeytab.GetKeyword(symtable[index].identifier)
	END GetIdentifier;

	PROCEDURE GetAlias* (index: LONGINT): C2cpStrings.DynString;
		VAR ali: C2cpStrings.DynString;
	BEGIN
		ali := C2cpKeytab.GetKeyword(symtable[index].alias);
		IF ali$ = '' THEN ali := GetIdentifier(index) END;
		RETURN ali;
	END GetAlias;

	PROCEDURE GetType* (index: LONGINT): C2cpStrings.DynString;
	BEGIN
		RETURN C2cpKeytab.GetKeyword(symtable[index].ntype)
	END GetType;

	PROCEDURE GetInfo* (index: LONGINT): C2cpStrings.DynString;
	BEGIN
		RETURN C2cpKeytab.GetKeyword(symtable[index].info)
	END GetInfo;

	PROCEDURE GetSign* (index: LONGINT): CHAR;
	BEGIN
		RETURN symtable[index].sign
	END GetSign;

	PROCEDURE GetLinenumber* (index: LONGINT): LONGINT;
	BEGIN
		RETURN symtable[index].linenumber
	END GetLinenumber;

	PROCEDURE SeekFor (qualident, identifier: ARRAY OF CHAR): LONGINT;
		VAR
			a, b, i, n: LONGINT;
			qu1, id1, qu2, id2: ARRAY 255 OF CHAR;
	BEGIN
		qu1 := C2cpStrings.ToUpper(qualident)$;
		id1 := C2cpStrings.ToUpper(identifier)$;
		a := 0; (* in Tabelle suchen *)
		b := numberOfSym - 1;
		WHILE a <= b DO
			i := (a + b) DIV 2;
			n := AlphaOrder(i);
			qu2 := C2cpStrings.ToUpper(GetQualident(n) )$;
			id2 := C2cpStrings.ToUpper(GetIdentifier(n) )$;
			(* -- internal order by qualidant, identifier (case sensitive) *)
			IF (qu1$ > qu2$) OR (qu1$ = qu2$) & ((id1$ > id2$) OR (id1$ = id2$) & (identifier$ > GetIdentifier(n)$))
				THEN a := i + 1
			ELSE b := i - 1 END;
		END;
		RETURN a;
	END SeekFor;

	PROCEDURE Search* (qualident, identifier: ARRAY OF CHAR): LONGINT;
		VAR nr: LONGINT;
	BEGIN
		nr := SeekFor(qualident, identifier);
		IF (nr > numberOfSym) OR (qualident$ # GetQualident(AlphaOrder(nr))$)
			OR (identifier$ # GetIdentifier(AlphaOrder(nr))$) THEN
			nr := SeekFor('', identifier); (* suche in Außengruppe wiederholen *)
		END;
		IF (nr > numberOfSym) OR (identifier$ # GetIdentifier(AlphaOrder(nr))$) THEN
			RETURN 0 (* nicht gefunden *)
		ELSE
			RETURN AlphaOrder(nr) (* gefunden *)
		END;
	END Search;

	PROCEDURE Insert* (qualident, identifier, alias, ntype, info: ARRAY OF CHAR; zc: CHAR; zn: LONGINT);
		VAR a, i: LONGINT;
	BEGIN
		IF zc < ' ' THEN zc := ' ' END;
		a := SeekFor(qualident, identifier);
		IF (a >= numberOfSym) OR (qualident$ # GetQualident(AlphaOrder(a))$)
			OR (identifier$ # GetIdentifier(AlphaOrder(a))$) THEN
			IF numberOfSym < maxSym THEN
				numberOfSym := numberOfSym + 1; (* in Tabelle eintragen *)
				symtable[numberOfSym].qualident := C2cpKeytab.SearchKey(qualident);
				symtable[numberOfSym].identifier := C2cpKeytab.SearchKey(identifier);
				symtable[numberOfSym].alias := C2cpKeytab.SearchKey(alias);
				symtable[numberOfSym].ntype := C2cpKeytab.SearchKey(ntype);
				symtable[numberOfSym].info := C2cpKeytab.SearchKey(info);
				symtable[numberOfSym].sign := zc;
				symtable[numberOfSym].linenumber := zn;
				IF GetIdentifier(numberOfSym)$ # '' THEN (* Eintrag bestädigen *)
					FOR i := numberOfSym TO a + 1 BY - 1 DO
						symtable[i].index := symtable[i - 1].index;
					END;
					symtable[a].index := numberOfSym;
				ELSE (* Eintrag zurücknehmen  *)
					numberOfSym := numberOfSym - 1; (* weil KeyTab-Überlauf  *)
				END;
			ELSE (* kein Eintrag weil Tabellenüberlauf *)
				StdLog.String(' *** C2cpTabelle.Insert: symbol table overflow *** ');
				StdLog.Ln;
			END;
		ELSE
			i := AlphaOrder(a);
			IF GetAlias(i)$ = identifier$ THEN symtable[i].alias := C2cpKeytab.SearchKey(alias) END;
			IF GetType(i)$ = '' THEN symtable[i].ntype := C2cpKeytab.SearchKey(ntype) END;
			IF GetInfo(i)$ = '' THEN symtable[i].info := C2cpKeytab.SearchKey(info) END;
			IF GetSign(i) = ' ' THEN symtable[i].sign := zc END;
			IF GetLinenumber(i) = 0 THEN symtable[i].linenumber := zn END;
		END;
	END Insert;

	PROCEDURE Update* (qualident, identifier, alias, ntype, info: ARRAY OF CHAR; zc: CHAR; zn: LONGINT);
		VAR i: LONGINT;
	BEGIN
		IF zc < ' ' THEN zc := ' ' END;
		i := Search(qualident, identifier);
		IF i > 0 THEN
			IF alias # '' THEN symtable[i].alias := C2cpKeytab.SearchKey(alias) END;
			IF ntype # '' THEN symtable[i].ntype := C2cpKeytab.SearchKey(ntype) END;
			IF info # '' THEN symtable[i].info := C2cpKeytab.SearchKey(info) END;
			IF zc # ' ' THEN symtable[i].sign := zc END;
			IF zn # 0 THEN symtable[i].linenumber := zn END;
		END;
	END Update;

	PROCEDURE Merken* (index, zn: LONGINT);
	BEGIN
		IF index > 0 THEN
			IF symtable[index].linenumber = 0 THEN symtable[index].linenumber := zn END;
		END;
	END Merken;

	PROCEDURE Reserviert* (wort: ARRAY OF CHAR): BOOLEAN;
	BEGIN
		wort := C2cpStrings.ToUpper(wort)$;
		IF (wort = 'ABSTRACT') OR (wort = 'ARRAY') OR (wort = 'BEGIN')
			OR (wort = 'BY') OR (wort = 'BOOLEAN') OR (wort = 'BYTE')
			OR (wort = 'CASE') OR (wort = 'CHAR') OR (wort = 'CLOSE')
			OR (wort = 'CONST') OR (wort = 'DIV') OR (wort = 'DO')
			OR (wort = 'ELSE') OR (wort = 'ELSIF') OR (wort = 'EMPTY')
			OR (wort = 'END') OR (wort = 'EXIT') OR (wort = 'EXTENSIBLE')
			OR (wort = 'FALSE') OR (wort = 'FOR') OR (wort = 'IF')
			OR (wort = 'IMPORT') OR (wort = 'IN') OR (wort = 'LONGINT')
			OR (wort = 'IS') OR (wort = 'LIMITED') OR (wort = 'HUGEINT')
			OR (wort = 'LOOP') OR (wort = 'MOD') OR (wort = 'MODULE')
			OR (wort = 'NIL') OR (wort = 'OF') OR (wort = 'OR')
			OR (wort = 'OUT') OR (wort = 'POINTER') OR (wort = 'PROCEDURE')
			OR (wort = 'REAL') OR (wort = 'RECORD') OR (wort = 'REPEAT')
			OR (wort = 'RETURN') OR (wort = 'SET') OR (wort = 'SHORTCHAR')
			OR (wort = 'INTEGER') OR (wort = 'THEN') OR (wort = 'TO')
			OR (wort = 'TRUE') OR (wort = 'TYPE') OR (wort = 'UNTIL')
			OR (wort = 'VAR') OR (wort = 'WHILE') OR (wort = 'WITH')
			THEN
			RETURN TRUE
		ELSIF (wort = 'AND') OR (wort = 'NOT') THEN (* Turbo Pascal *)
			RETURN TRUE
		ELSE
			RETURN FALSE
		END;
	END Reserviert;

	PROCEDURE Pruefen*;
		VAR
			i, j, n, doppelt: LONGINT;
			tg, tn, qualident, identifier, alias: C2cpStrings.DynString;
			utg, utn, uqualident, uidentifier: ARRAY 256 OF CHAR;
			alias2, number: ARRAY 256 OF CHAR;
	BEGIN
		(* 1. Teil: Alias Name bei Groß/Klein-Schreibung vergeben *)
		doppelt := 0;
		qualident := GetQualident(0); (* '' *)
		identifier := GetIdentifier(0) (* '' *);
		FOR i := 0 TO numberOfSym - 1 DO;
			n := AlphaOrder(i);
			tg := qualident;
			tn := identifier;
			qualident := GetQualident(n);
			identifier := GetIdentifier(n);
			utg := C2cpStrings.ToUpper(tg)$;
			uqualident := C2cpStrings.ToUpper(qualident)$;
			utn := C2cpStrings.ToUpper(tn)$;
			uidentifier := C2cpStrings.ToUpper(identifier)$;
			IF (utg = uqualident) & (utn = uidentifier) THEN
				doppelt := doppelt + 1;
				IF (GetSign(n) # 'S') & (GetAlias(n) = identifier) THEN
					number := C2cpStrings.IntToString(doppelt)$;
					alias2 := identifier + number;
					StdLog.String('   Hinweis: Alias-Name "');
					StdLog.String(qualident); StdLog.String('.'); StdLog.String(alias2);
					StdLog.String('" eingefügt.');
					StdLog.Ln;
					symtable[n].alias := C2cpKeytab.SearchKey(alias2);
				END;
			ELSE
				doppelt := 0;
			END;
		END;
		(* 2. Teil: Alias Name bei reservierten Wörter vergeben *)
		FOR i := 0 TO numberOfSym - 1 DO;
			n := AlphaOrder(i);
			qualident := GetQualident(n);
			identifier := GetIdentifier(n);
			IF (GetSign(n) # 'S') & Reserviert(identifier) & (GetAlias(n) = identifier)
				THEN
				alias2 := 'z' + identifier;
				StdLog.String('   Hinweis: Alias-Name "');
				StdLog.String(qualident); StdLog.String('.'); StdLog.String(alias2); StdLog.String('" eingefügt.');
				StdLog.Ln;
				symtable[n].alias := C2cpKeytab.SearchKey(alias2);
			END;
		END;
		(* 3. Teil: Ist der Programmname eindeutig? *)
		IF GetSign(1) # 'U' THEN
			identifier := GetIdentifier(1);
			j := Search('', identifier);
			IF (j # 0) & (GetQualident(j)$ = '') & (GetAlias(1) = identifier) THEN
				alias2 := 'x' + identifier;
				StdLog.String('   Hinweis: Alias-Name "'); StdLog.String(alias2); StdLog.String('" eingefügt.');
				StdLog.Ln;
				symtable[1].alias := C2cpKeytab.SearchKey(alias2);
			END;
			symtable[1].sign := 'M';
		END;
		(* 4. Teil: Ist der Alias Name auch in Name ohne Alias vorhanden ? *)
		FOR i := 0 TO numberOfSym - 1 DO;
			n := AlphaOrder(i);
			IF (GetLinenumber(n) # 0) & (GetSign(n) # 'R') THEN
				qualident := GetQualident(n);
				identifier := GetIdentifier(n);
				alias := GetAlias(n);
				j := Search(qualident, alias);
				IF (j # 0) & (j # n) & (GetAlias(j) = alias) THEN
					StdLog.String('   Warnung: Alias-Name "');
					StdLog.String(qualident); StdLog.String('.'); StdLog.String(alias);
					StdLog.String('" ist doppelt.');
					StdLog.Ln;
				END;
			END;
		END;
	END Pruefen;

	PROCEDURE FindNext* (): C2cpStrings.DynString;
		VAR nr: LONGINT; aliasname: C2cpStrings.DynString;
	BEGIN
		REPEAT
			pos := pos + 1;
			nr := AlphaOrder(pos);
		UNTIL (GetQualident(nr)$ # gru$) OR (GetSign(nr) = zei);
		IF GetQualident(nr)$ = gru$ THEN
			aliasname := GetAlias(nr);
		ELSE
			aliasname := C2cpKeytab.GetKeyword(0); (* := ''; nicht gefunden *)
		END;
		RETURN aliasname;
	END FindNext;

	PROCEDURE FindFirst* (qualident: ARRAY OF CHAR; zc: CHAR): C2cpStrings.DynString;
	BEGIN
		pos := SeekFor(qualident, '') - 1; (* vor dem ersten Eintrag *)
		gru := qualident$;
		zei := zc;
		RETURN FindNext(); (* erster Eintrag holen   *)
	END FindFirst;

	PROCEDURE ReadLine (VAR sc: TextMappers.Scanner; OUT s: ARRAY OF CHAR);
		VAR ch: CHAR; i: LONGINT;
	BEGIN
		i := 0; ch := sc.rider.char;
		WHILE ~(sc.rider.eot OR (ch = TextModels.line) OR (ch = TextModels.para) OR (i > 254)) DO
			s[i] := ch; INC(i);
			sc.rider.ReadChar(ch);
		END;
		s[i] := 0X;
		sc.Scan;
	END ReadLine;

	PROCEDURE Load* (IN path: ARRAY OF CHAR; filename: Files.Name);
		VAR
			md: TextModels.Model; sc: TextMappers.Scanner;
			vw: Views.View; loc: Files.Locator;
			linenumber: LONGINT; sign: CHAR; qualident, identifier, alias, ntype, info: ARRAY 256 OF CHAR;
	BEGIN
		StdLog.String('-- Symbol table: Loaded from '); StdLog.String(path); StdLog.String(filename); StdLog.Ln;
		loc := Files.dir.This(path);
		vw := Views.Old(Views.dontAsk, loc, filename, conv);
		IF vw # NIL THEN
			md := vw(TextViews.View).ThisModel();
			sc.ConnectTo(md);
			sc.SetOpts({TextMappers.returnCtrlChars});
			sc.Scan;
			WHILE ~ sc.rider.eot DO
				linenumber := 0; sign := ' '; qualident := ''; identifier := ''; alias := ''; ntype := ''; info := '';
				IF sc.type = TextMappers.para THEN sc.Scan END; 	(* skip rulers *)
				IF sc.type = TextMappers.int THEN linenumber := sc.int; sc.Scan END;
				sign := sc.string[0]; sc.Scan; (* IF sign = 00X THEN sign := ' ' END; *)
				IF sc.type = TextMappers.string THEN qualident := sc.string$; sc.Scan END;
				IF sc.type = TextMappers.real THEN 
					qualident := C2cpStrings.IntToString(SHORT(ENTIER(sc.real)))$; sc.Scan 
				END;
				IF sc.char = '.' THEN sc.Scan END;
				IF sc.type = TextMappers.string THEN identifier := sc.string$; sc.Scan END;
				IF sc.char = '/' THEN
					sc.Scan;
					IF sc.type = TextMappers.string THEN alias := sc.string$; sc.Scan END;
				END;
				IF sc.char = ':' THEN
					sc.Scan;
					IF sc.type = TextMappers.string THEN ntype := sc.string$; sc.Scan END;
				END;
				IF sc.char = ';' THEN ReadLine(sc, info) END;
				IF sc.type = TextMappers.line THEN sc.Scan END;
				Insert(qualident, identifier, alias, ntype, info, sign, linenumber);
			END;
			StdLog.String('-- Symbol table: There are'); StdLog.Int(numberOfSym); StdLog.String(' symbols');
			StdLog.String(' and'); StdLog.Int(C2cpKeytab.numberOfKey); StdLog.String(' keywords'); StdLog.Ln;
		ELSE
			StdLog.String('-- Symbol table: File not found'); StdLog.Ln;
		END;
	END Load;

	PROCEDURE WriteSaveRuler  (VAR fm:  TextMappers.Formatter);
		VAR ruler: TextRulers.Ruler;
	BEGIN
		ruler := TextRulers.dir.New(NIL);
		TextRulers.AddTab(ruler, 50 * Ports.mm);
		fm.WriteView(ruler);
	END WriteSaveRuler;

	PROCEDURE Save* (IN path: ARRAY OF CHAR; filename: Files.Name);
		VAR
			md: TextModels.Model; fm: TextMappers.Formatter;
			vw: Views.View; loc: Files.Locator;
			i, res, index, linenumber: LONGINT;
			sign: CHAR; qualident, identifier, alias, ntype, info: C2cpStrings.DynString;
	BEGIN
		StdLog.String('-- Symbol table: Save to '); StdLog.String(path); StdLog.String(filename); StdLog.Ln;
		md := TextModels.dir.New();
		fm.ConnectTo(md); fm.SetPos(0);
		WriteSaveRuler(fm);
		FOR i := 0 TO numberOfSym - 1 DO
			index := AlphaOrder(i);
			linenumber := GetLinenumber(index);
			sign := GetSign(index); IF sign = 00X THEN sign := ' ' END;
			qualident := GetQualident(index);
			identifier := GetIdentifier(index);
			alias := GetAlias(index);
			ntype := GetType(index);
			info := GetInfo(index);
			fm.WriteIntForm(linenumber, TextMappers.decimal, 5, TextModels.digitspace, TextMappers.hideBase);
			fm.WriteString(' '); fm.WriteChar(sign);
			fm.WriteString(' '); fm.WriteString(qualident);
			fm.WriteString('.'); fm.WriteString(identifier);
			IF alias$ # identifier$ THEN fm.WriteString('/'); fm.WriteString(alias) END;
			IF ntype$ # '' THEN fm.WriteString(':'); fm.WriteString(ntype) END;
			IF info$ # '' THEN fm.WriteString(';'); fm.WriteString(info) END;
			fm.WriteLn;
		END;
		vw := TextViews.dir.New(md);
		loc := Files.dir.This(path);
		Views.Register(vw, Views.dontAsk, loc, filename, conv, res);
		StdLog.String('-- Symbol table: There are'); StdLog.Int(numberOfSym); StdLog.String(' symbols');
		StdLog.String(' and'); StdLog.Int(C2cpKeytab.numberOfKey); StdLog.String(' keywords'); StdLog.Ln;
	END Save;

	PROCEDURE WriteSymRuler  (VAR fm:  TextMappers.Formatter);
		VAR ruler: TextRulers.Ruler;
	BEGIN
		ruler := TextRulers.dir.New(NIL);
		TextRulers.AddTab(ruler, 7 * Ports.mm); TextRulers.MakeRightTab(ruler);
		TextRulers.AddTab(ruler, 15 * Ports.mm); TextRulers.MakeRightTab(ruler);
		TextRulers.AddTab(ruler, 25 * Ports.mm); TextRulers.MakeRightTab(ruler);
		TextRulers.AddTab(ruler, 35 * Ports.mm); TextRulers.MakeRightTab(ruler);
		TextRulers.AddTab(ruler, 40 * Ports.mm);
		TextRulers.AddTab(ruler, 70 * Ports.mm);
		TextRulers.AddTab(ruler, 100 * Ports.mm);
		TextRulers.AddTab(ruler, 130 * Ports.mm);
		TextRulers.AddTab(ruler, 150 * Ports.mm);
		fm.WriteView(ruler);
	END WriteSymRuler;

	PROCEDURE ShowSymList*;
		VAR
			md: TextModels.Model; vw: TextViews.View; title: Views.Title;
			fm: TextMappers.Formatter; i, index: LONGINT;
	BEGIN
		title := 'Symbol table';
		md := TextModels.dir.New();
		fm.ConnectTo(md);
		vw := TextViews.dir.New(md);
		Views.OpenAux(vw, title); (* browser mode *)
		WriteSymRuler(fm);
		fm.WriteTab; fm.WriteString(' # ');
		fm.WriteTab; fm.WriteString('index');
		fm.WriteTab; fm.WriteString('line');
		fm.WriteTab; fm.WriteString('sign');
		fm.WriteTab; fm.WriteString('Qualident');
		fm.WriteTab; fm.WriteString('Identifier');
		fm.WriteTab; fm.WriteString('Alias');
		fm.WriteTab; fm.WriteString('Type');
		fm.WriteTab; fm.WriteString('Info');
		fm.WriteLn;
		FOR i := 0 TO numberOfSym - 1 DO
			index := AlphaOrder(i);
			fm.WriteTab; fm.WriteInt(i);
			fm.WriteTab; fm.WriteInt(index);
			fm.WriteTab; fm.WriteInt(GetLinenumber(index));
			fm.WriteTab; fm.WriteChar(GetSign(index));
			fm.WriteTab; fm.WriteString(GetQualident(index));
			fm.WriteTab; fm.WriteString(GetIdentifier(index));
			fm.WriteTab; fm.WriteString(GetAlias(index));
			fm.WriteTab; fm.WriteString(GetType(index));
			fm.WriteTab; fm.WriteString(GetInfo(index));
			fm.WriteLn;
		END;
		fm.WriteString('Ende.'); fm.WriteLn;
	END ShowSymList;

	PROCEDURE Dispose*;
	BEGIN
		numberOfSym := 0;
		C2cpKeytab.Dispose;
	END Dispose;

BEGIN
	numberOfSym := 0;
	conv := Converters.list;
	WHILE (conv # NIL) & (conv.fileType # "txt") DO
		conv := conv.next
	END;
END C2cpTable.

(*-----------------------------------------------------------------------------
--   C Biber
--   (C) Helmut Zinn   V94/04/06 - V95/12/19 - V05/05/11
-----------------------------------------------------------------------------*)

MODULE C2cpCBiber;

	

	IMPORT
		C2cpFiles, C2cpStrings, C2cpTable, StdLog;

	CONST
		NamedInteger = FALSE;

	VAR
		zurueck: INTEGER; (* Nächster Zustand nach Ende des Kommentars *)
		tiefe: INTEGER; (* Anzahl der offenen BEGIN's *)
		klammer: INTEGER; (* Anzahl offener '(' Klammer *)
		zu: INTEGER; (* Wenn offen < zu dann Klammer zu einfügen *)
		grenze: INTEGER; (* Wenn klammer <= grenze dann Bedingung einfügen *)
		parklam: INTEGER; (* Klammertiefe der Parameter eines Funktionaufrufs *)
		xarray: INTEGER; (* Dimensionszähler für Array *)
		union: INTEGER; (* Zähler für Union-Record *)
		enum: INTEGER; (* Zähler für Enum *)
		zeile: INTEGER; (* Zeilenzahl der Procedure und Function *)
		zaehler: INTEGER; (* Parameterzähler für 'main' & 'printf' *)
		ma: INTEGER; (* Position von '(' im term *)
		mi: INTEGER; (* Position von ':=' im term *)
		mk: INTEGER; (* Klammertiefe von ma *)
		typedef: BOOLEAN; (* Type oder Variable definieren? *)
		recdef: BOOLEAN; (* Recordtype oder Recordvariable definieren? *)
		onlyTypedef: BOOLEAN; (* Recordtype and Recordvariable definieren? *)
		zeiger: BOOLEAN; (* Die Variable ist ein Zeigertyp (POINTER) *)
		vkomma: BOOLEAN; (* Variable mit Komma aufzählen *)
		vbegin: BOOLEAN; (* BEGIN bis nach der VAR Definitionen zurückhalten *)
		anfang: BOOLEAN; (* Am Anfang einer neuen Zeile *)
		vorne: BOOLEAN; (* Am Anfang einer neuen Anweisung - vor := *)
		addklamm: BOOLEAN; (* Bei Condition Klammer hinzufügen *)
		delklamm: BOOLEAN; (* Bei Condition Klammer weglassen *)
		bitwise: BOOLEAN; (* Ist bitwise AND, OR, XOR oder NOT ? *)
		int: BOOLEAN; (* Ist der Ausdruck vom Type Integer ? *)
		isfile: BOOLEAN; (* Ist der Ausdruck vom Type FILE ? *)
		ischar: BOOLEAN; (* Ist der Ausdruck vom Type ARRAY OF CHAR ? *)
		default: BOOLEAN; (* Im default oder case Teil vom select ? *)
		isbreak: BOOLEAN; (* Ist ein break am Ende vom case Teil ? *)
		isgoto: BOOLEAN; (* Ist ein goto, halt oder return im case Teil ? *)
		dummy: BOOLEAN; (* Function mit dummy ? *)
		return: BOOLEAN; (* RETURN nach ; anhängen *)
		define: BOOLEAN; (* #define in FUNCTION oder CONST übersetzen *)
		ln: BOOLEAN; (* StdLog.Ln; anhängen *)
		alt: ARRAY 256 OF CHAR; (* Name der letzten Gruppe *)
		gruppe: ARRAY 256 OF CHAR; (* Name der zur Zeit aktiven Procedure *)
		name: ARRAY 256 OF CHAR; (* Name merken *)
		alias: ARRAY 256 OF CHAR; (* Alias-Name merken *)
		symtype: ARRAY 256 OF CHAR; (* Type der Variable nach der Symbol-Tabelle *)
		zeichen: CHAR; (* Kennzeichen - Type des Namens *)
		arrayname: ARRAY 256 OF CHAR; (* Arrayname merken *)
		arrayzeichen: CHAR; (* und sein Zeichen merken *)
		recname: ARRAY 256 OF CHAR; (* Recordname merken *)
		recpoin: ARRAY 256 OF CHAR; (* Recordpointername merken *)
		alloc: ARRAY 256 OF CHAR; (* Pointer Name für Memory Allocation *)
		varname: ARRAY 256 OF CHAR; (* Variablename merken *)
		sonder: ARRAY 256 OF CHAR; (* Ergänzungen zum Variabletype merken *)
		vartype: ARRAY 256 OF CHAR; (* Type der Variable nach dem Quellprogramm *)
		partype: ARRAY 256 OF CHAR; (* Type der letzen Variable in der Liste *)
		parvar: CHAR; (* Ist Parameter vom Type VAR ? *)
		labelnr: INTEGER; (* Label Zähler für GOTO *)
		labelname: ARRAY 256 OF CHAR; (* Aliasname des Labels *)
		term: ARRAY 512 OF CHAR; (* Für ++ oder -- sammeln *)
		plusplus: ARRAY 256 OF CHAR; (* Das nach dieser Anweisung folgende INC oder DEC *)
		follow: ARRAY 256 OF CHAR; (* Das nach der Klammer zu folgende Schlüsselwort *)
		blank: ARRAY 256 OF CHAR; (* Leerstellen vor Kommantar *)
		relation: ARRAY 256 OF CHAR; (* Letzter Vergleich ist <, <=, =, >=, > oder # *)
		index: INTEGER; (* Index 0..4 zum Merker für die FOR-Schleife *)
		merker: ARRAY 4 + 1 OF ARRAY 256 OF CHAR;

	CONST
		nstack = 16; (* maximale Stacktiefe *)

	VAR
		stack: INTEGER; (* Anzahl der gemerkten Werte *)
		art: ARRAY nstack OF CHAR; (* Schleifenart ' ' I ? F W R C *)
		wert: ARRAY nstack OF INTEGER; (* Tiefe beim Anfang der Schleife *)
		anw: ARRAY nstack OF ARRAY 256 OF CHAR; (* diese Anweisung vor END einfügen *)
		ende: ARRAY nstack OF BOOLEAN; (* END nach ; einfügen *)

	PROCEDURE Biber* (VAR s: C2cpFiles.Scanner; VAR zustand: INTEGER; VAR ergebnis: ARRAY OF CHAR);

		(*  alter Zustand     gelesenes Zeichen     Aufgabe     neuer Zustand  *)
		(*---------------------------------------------------------------------*)

		PROCEDURE Aufgabe (zusatz: ARRAY OF CHAR);
		BEGIN
			ergebnis := ergebnis + zusatz;
			IF index > 0 THEN merker[index] := merker[index] + zusatz END;
		END Aufgabe;

		PROCEDURE Rand (offset: INTEGER): C2cpStrings.DynString;
			VAR i: INTEGER; r: ARRAY 256 OF CHAR;
		BEGIN (* linker Rand in Abhängigkeit von der Blocktiefe *)
			r := '';
			FOR i := 1 TO tiefe + offset + 1 DO r := r + C2cpStrings.TAB END;
			RETURN C2cpStrings.MakeDynString(r);
		END Rand;

		PROCEDURE AddMerker (VAR s: ARRAY OF CHAR; a: ARRAY OF CHAR);
		BEGIN
			IF s # '' THEN s := s + ', ' END;
			s := s + a;
		END AddMerker;

		PROCEDURE MerkerAusgeben (s: ARRAY OF CHAR);
			VAR satz: ARRAY 256 OF CHAR;
		BEGIN
			WHILE s # '' DO
				satz := C2cpStrings.Trim(C2cpStrings.Parse(s, ', ')$)$;
				IF satz # '' THEN
					IF anfang THEN
						Aufgabe(Rand( - tiefe + 1))
					ELSE
						Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0))
					END;
					Aufgabe(satz); Aufgabe(';');
					anfang := FALSE;
				END;
			END;
		END MerkerAusgeben;

		PROCEDURE Push (a: CHAR; w: INTEGER; e: BOOLEAN);
		BEGIN
			IF stack < nstack THEN 
				INC(stack)
			ELSE
				StdLog.String(' ***  C2cpCBiber: Stacküberlauf  *** '); StdLog.Ln;
			END;
			art[stack] := a;
			wert[stack] := w;
			anw[stack] := '';
			ende[stack] := e;
		END Push;

		PROCEDURE Pop;
		BEGIN
			art[stack] := ' ';
			wert[stack] := - 1;
			anw[stack] := '';
			ende[stack] := FALSE;
			IF stack > 0 THEN DEC(stack) END;
		END Pop;

		PROCEDURE (*2*) ^Block;
		PROCEDURE (*8*) ^ParameterListe;
		PROCEDURE (*23*) ^ListVariable;
		PROCEDURE (*31*) ^StartAnweisung;
		PROCEDURE (*32*) ^Anweisung;
		PROCEDURE (*33*) ^StartCondition;
		PROCEDURE (*34*) ^Condition;
		PROCEDURE (*35*) ^Expression;
		PROCEDURE (*38*) ^EndAnweisung;
		PROCEDURE (*39*) ^NeueAnwZeile;

		PROCEDURE MerkeName;
			VAR i: INTEGER;
		BEGIN
			IF (s.class = C2cpFiles.cName) OR (s.s = '_') THEN
				name := s.s$; (* C Version des Namens holen. *)
				WHILE (s.nextCh = '_') OR C2cpStrings.IsAlpha(s.nextCh) OR C2cpStrings.IsDigit(s.nextCh) DO
					s.Scan;
					name := name + s.s;
				END;

				i := C2cpTable.Search(gruppe, name);
				alias := C2cpTable.GetAlias(i)$;
				symtype := C2cpTable.GetType(i)$;
				zeichen := C2cpTable.GetSign(i);
				C2cpTable.Merken(i, s.line);

				IF alias = '' THEN alias := name$ END; (* wenn Name nicht in Tabelle *)
				IF (zeichen = 'F') OR (zeichen = 'P') THEN parklam := klammer + 1 END;
				IF symtype = 'REAL' THEN int := FALSE
				ELSIF symtype = 'DOUBLE' THEN int := FALSE
				ELSIF symtype = 'TEXTFILE' THEN isfile := TRUE
				ELSIF symtype = 'ARRAY OF CHAR' THEN ischar := TRUE
				END;

				s.s := alias$;
				s.class := C2cpFiles.cName;
			END;
		END MerkeName;

		PROCEDURE Tausche;
			VAR wort: ARRAY 256 OF CHAR;
		BEGIN
			wort := s.s$;
			IF (s.s = '=') & (s.nextCh = '=') (* Zeichen zusammenfassen *)
			OR (s.s = '<') & (s.nextCh = '=')
			OR (s.s = '>') & (s.nextCh = '=')
			OR (s.s = '!') & (s.nextCh = '=')
			OR (s.s = '-') & (s.nextCh = '>')
			OR (s.s = '&') & (s.nextCh = '&')
			OR (s.s = '|') & (s.nextCh = '|')
			OR (s.s = '<') & (s.nextCh = '<')
			OR (s.s = '>') & (s.nextCh = '>')
			OR (s.s = '<') & (s.nextCh = ':')
			OR (s.s = ':') & (s.nextCh = '>')
			OR (s.s = '%') & (s.nextCh = ':') THEN
				s.Scan;
				wort := wort + s.s;
			END;
			IF wort = '==' THEN s.s := '=' (* und austauschen *)
			ELSIF wort = '=' THEN s.s := ':='
			ELSIF wort = '!=' THEN s.s := '#'
			ELSIF wort = '->' THEN s.s := '^.'
			ELSIF wort = '~' THEN s.s := '~'; bitwise := TRUE;
			ELSIF wort = '&' THEN s.s := ' & '; bitwise := TRUE;
			ELSIF wort = '|' THEN s.s := ' OR '; bitwise := TRUE;
			ELSIF wort = '^' THEN s.s := ' XOR '; bitwise := TRUE;
			ELSIF wort = '!' THEN s.s := '~'; bitwise := FALSE;
			ELSIF wort = '&&' THEN s.s := ' & '; bitwise := FALSE;
			ELSIF wort = '||' THEN s.s := ' OR '; bitwise := FALSE;
			ELSIF int & (wort = '/') THEN s.s := ' DIV '
			ELSIF wort = '%' THEN s.s := ' MOD '
			ELSIF wort = '<<' THEN s.s := ' SHL '
			ELSIF wort = '>>' THEN s.s := ' SHR '
			ELSIF wort = '<:' THEN s.s := '['
			ELSIF wort = ':>' THEN s.s := ']'
			ELSIF wort = '%:' THEN s.s := '#'
			ELSE s.s := wort$
			END;
			IF (s.s = '0') & (s.nextCh = 'x') THEN (* Hexzahl 0x1b -> 01BH *)
				s.Scan;
				s.s := '0' + C2cpStrings.ToUpper(C2cpStrings.Right(s.s, LEN(s.s$) - 1)) + 'H';
				s.class := C2cpFiles.cInteger;
			END;
			IF s.class = C2cpFiles.cString THEN
				IF s.s[0] = '"' THEN (* String Konstante *)
					wort := '';
					WHILE s.s # '' DO
						wort := wort + C2cpStrings.Parse(s.s, "'");
						IF s.s # '' THEN wort := wort + "''" END; (* ' verdoppeln *)
					END;
					s.s := "'" + C2cpStrings.Substr(wort, 1, LEN(wort$) - 2) + "'"; (* " -> ' *)
				ELSIF (LEN(s.s$) = 4) & (s.s[1] = '\') THEN
					CASE s.s[2] OF (* Char Konstante \x *)
					| '0': s.s := '0X';
					| 'n': s.s := '0DX';
					| 't': s.s := '09X';
					| '\': s.s := "'" + '\' + "'";
					ELSE; (* nichts tun *)
					END;
				END;
			END;
			(* Der Inhalt der Variable, auf die x zeigt *)
			IF (s.s = '*') & C2cpStrings.IsAlpha(s.nextCh) THEN (* *x --> x^ *)
				s.Scan;
				MerkeName;
				IF (zeichen = 'Z') OR (zeichen = 'X') OR (zeichen = 'U') THEN
				s.s := s.s + '^' ELSE s.s := '*' + s.s
				END;
				vorne := FALSE;
				zeiger := TRUE;
			END;
			(* Die Adresse der Variable x *)
			IF (wort = '&') & C2cpStrings.IsAlpha(s.nextCh) THEN (* &x --> @x *)
				s.Scan;
				MerkeName;
				IF parklam > 0 THEN s.s := '(*@*)' + s.s ELSE s.s := '@' + s.s END;
			END;
			IF (s.class = C2cpFiles.cInteger) & C2cpStrings.IsAlpha(s.nextCh) THEN
				s.Scan;
				s.s := wort + '(*' + s.s + '*)';
				s.class := C2cpFiles.cInteger;
			END;
		END Tausche;

		PROCEDURE Look; (* 1. Übersetzung des aktuelles Zeichens *)
		BEGIN
			IF define & (s.s = '\') THEN
				s.Scan;
				IF s.class = C2cpFiles.cSpace THEN s.Scan END;
				IF s.class = C2cpFiles.cEOL THEN s.Scan END;
			END;
			IF s.class = C2cpFiles.cSpace THEN
				IF s.nextCh = C2cpStrings.CR THEN s.Scan END;
			ELSE
				IF (s.class = C2cpFiles.cName) OR (s.s = '_') THEN MerkeName ELSE Tausche END;
			END;
		END Look;

		PROCEDURE LookAhead; (* wenn nächstes Zeichen Blank ist, dann hole Zeichen *)
		BEGIN
			IF C2cpStrings.IsSpace(s.nextCh) THEN s.Scan END; (* s.s = blank & s.nextCh # blank *)
			IF define & (s.nextCh = '\') THEN
				s.Scan;
				IF s.nextCh = ' ' THEN s.Scan END;
				IF s.nextCh = C2cpStrings.CR THEN s.Scan END;
			END;
		END LookAhead;

		PROCEDURE LookAhead2;
		BEGIN
			LookAhead;
			IF (s.nextCh = C2cpStrings.CR) & ~ define THEN (* auch CR überlesen *)
				s.Scan;
				LookAhead;
			END;
		END LookAhead2;

		PROCEDURE Skip; (* nächstes Zeichen holen, welches nicht Blank ist *)
		BEGIN (* nur wenn nächstes Zeichen kein Kommentar Anfang *)
			LookAhead;
			IF s.nextCh # '/' THEN
				s.Scan;
				Look; (* Namen mit '_' zusammenketten *)
			END;
		END Skip;

		PROCEDURE StorageClass;
		BEGIN
			WHILE (s.s = 'auto')
			OR (s.s = 'static')
			OR (s.s = 'register')
			OR (s.s = 'extern')
			OR (s.s = 'const')
			OR (s.s = 'volatile')
			OR (s.s = 'far')
			OR (s.s = 'interrupt')
			OR (C2cpTable.GetSign(C2cpTable.Search(gruppe, s.s)) = 'D')
			DO
				sonder := sonder + '(*' + s.s + '*)';
				Skip;
				IF s.class = C2cpFiles.cEOL THEN Skip END;
			END;
		END StorageClass;

		PROCEDURE MerkeType;
		BEGIN
			zeiger := FALSE;
			sonder := '';
			StorageClass;
			IF s.s = 'unsigned' THEN vartype := 'INTEGER(*word*)'
			ELSIF s.s = 'signed' THEN vartype := 'INTEGER'
			ELSIF s.s = 'short' THEN vartype := 'SHORTINT'
			ELSIF s.s = 'long' THEN vartype := 'LONGINT'
			ELSIF s.s = 'char' THEN vartype := 'CHAR'
			ELSIF s.s = 'int' THEN vartype := 'INTEGER'
			ELSIF s.s = 'float' THEN vartype := 'REAL'
			ELSIF s.s = 'double' THEN vartype := 'REAL(*double*)'
			ELSIF s.s = 'void' THEN vartype := 'POINTER'
			ELSIF s.s = 'bool' THEN vartype := 'BOOLEAN'
			ELSIF s.s = 'boolean' THEN vartype := 'BOOLEAN'
			ELSIF s.s = 'FILE' THEN vartype := 'TEXT(*FILE*)'
			ELSE vartype := s.s$
			END
		END MerkeType;

		PROCEDURE MerkeTypeKette;
		BEGIN
			StorageClass;
			IF s.s = 'short' THEN
				IF vartype = 'INTEGER(*word*)' THEN vartype := 'SHORTINT(*byte*)' ELSE vartype := 'SHORTINT' END;
				Skip;
			END;
			IF s.s = 'long' THEN
				IF vartype = 'INTEGER(*word*)' THEN vartype := 'LONGINT(*unsigned*)' ELSE vartype := 'LONGINT' END;
				Skip;
			END;
			IF s.s = 'char' THEN
				IF vartype = 'INTEGER(*word*)' THEN vartype := 'CHAR(*unsigned*)' ELSE vartype := 'CHAR(*signed*)' END;
				Skip;
			END;
			IF s.s = 'double' THEN
				vartype := 'REAL(*double*)';
				Skip;
			END;
			StorageClass;
			IF s.s = 'int'THEN Skip END; (* unsigned long/short int *)
			StorageClass;
			IF (s.s = '*') OR (s.s = (*&*) ' & ') THEN
				zeiger := TRUE;
				LookAhead2;
				name := '';
				alias := '?';
				IF (s.nextCh # ',') & (s.nextCh # ')') THEN Skip END;
			END;
		END MerkeTypeKette;

		PROCEDURE ConvertType;
			VAR strucname: ARRAY 256 OF CHAR;
		BEGIN
			IF recpoin = '' THEN
				IF zeiger THEN
					IF C2cpStrings.Substr(vartype, 0, 4)$ = 'CHAR' THEN vartype := 'ARRAY OF CHAR' END;
				ELSE
					IF (vartype = 'ARRAY OF CHAR') OR (vartype = 'STRING') THEN vartype := 'CHAR' END;
				END;
				IF (vartype = 'TEXT(*FILE*)') OR (vartype = 'TEXTFILE') THEN
					zeiger := FALSE;
				ELSIF vartype = 'POINTER' THEN
					zeiger := TRUE;
				ELSIF (vartype = 'ARRAY OF CHAR') OR (vartype = 'STRING') THEN
					(* nichts tun *)
				ELSIF zeiger THEN
					IF C2cpStrings.Left(vartype, 11)$ # 'POINTER TO ' THEN 
						vartype := 'POINTER TO ' + vartype 
					END;
				ELSE
					IF C2cpStrings.Left(vartype, 11)$ = 'POINTER TO ' THEN 
						vartype := C2cpStrings.After(vartype, 'POINTER TO ')$ 
					END;
				END;
				zeichen := 'V';
				IF zeiger THEN zeichen := 'Z' END;
				IF C2cpStrings.Left(C2cpTable.GetType(C2cpTable.Search(gruppe, vartype)), 11)$ = 'POINTER TO ' THEN
					zeichen := 'Z'
				END;
				IF (follow # '') & (C2cpStrings.Left(vartype, 11)$ = 'POINTER TO ') THEN zeichen := 'U' END;
				IF (follow # '') & (C2cpStrings.Left(s.s, 1)$ = '@') THEN zeichen := 'U' END;
				IF vartype = 'TEXT(*FILE*)' THEN zeichen := 'U' END;
				IF vartype = 'POINTER' THEN zeichen := 'X' END;
				IF C2cpStrings.Left(vartype, 11)$ = 'POINTER TO ' THEN
					strucname := C2cpTable.GetInfo(C2cpTable.Search(gruppe, C2cpStrings.After(vartype, 'POINTER TO ')))$;
					IF strucname # '' THEN
						vartype := C2cpTable.GetInfo(C2cpTable.Search(gruppe, strucname))$;
						zeichen := 'Z';
					END;
				END;
			ELSE
				vartype := recpoin$;
				zeichen := 'Z';
				recpoin := '';
				C2cpTable.Update(gruppe, varname, '', vartype, '', ' ', 0);
			END;
			IF typedef THEN zeichen := 'T' END;
		END ConvertType;

		PROCEDURE TypeNachschauen (): C2cpStrings.DynString;
			VAR i: INTEGER;
		BEGIN
			i := C2cpTable.Search(gruppe, name);
			IF i > 0 THEN
				zeichen := C2cpTable.GetSign(i);
				RETURN C2cpTable.GetType(i);
			ELSE
				RETURN C2cpStrings.MakeDynString(vartype);
			END;
		END TypeNachschauen;

		PROCEDURE ListVariableType (ntype: ARRAY OF CHAR): C2cpStrings.DynString;
			VAR t: ARRAY 256 OF CHAR;
		BEGIN
			t := '';
			IF xarray > 0 THEN t := t + ' OF '
			ELSIF zeichen = 'T' THEN t := t + ' = '
			ELSE t := t + ': '
			END;
			t := t + ntype;
			IF s.s # ':=' THEN
				IF union > 0 THEN t := t + ')' END;
				t := t + ';';
			END;
			xarray := 0;
			vkomma := FALSE;
			RETURN C2cpStrings.MakeDynString(t);
		END ListVariableType;

		PROCEDURE KlammerLesen;
		BEGIN
			IF s.s = '(' THEN (* eingeklammerte Variable lesen *)
				Skip;
				LookAhead;
				IF s.nextCh = ')' THEN s.Scan END;
				s.s := name$;
				s.class := C2cpFiles.cName;
			END;
		END KlammerLesen;

		PROCEDURE DeclareVariable;
		BEGIN
			MerkeType;
			xarray := 0;
			zeiger := FALSE;
			vkomma := FALSE;
			IF union > 0 THEN
				Aufgabe(C2cpStrings.IntToString(union)); Aufgabe(' : (');
				union := union + 1;
			END;
			typedef := FALSE;
			zustand := 23;
			IF (zeichen # 'T') & (s.s # 'enum') & (s.s = vartype) THEN
				IF s.class = C2cpFiles.cName THEN
					LookAhead2;
					s.s := vartype$;
					s.class := C2cpFiles.cName;
				END;
				IF ~(C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '*')) THEN
					IF sonder # '' THEN vartype := 'INTEGER'
					ELSE vartype := 'POINTER' END;
					ListVariable;
				END;
			END;
		END DeclareVariable;

		PROCEDURE AufgabeVariableType;
		BEGIN
			IF enum = 0 THEN 
				Aufgabe(ListVariableType(partype$)); 
			END;
			IF vkomma & NamedInteger THEN Aufgabe(', ') END;
			vkomma := FALSE
		END AufgabeVariableType;

		PROCEDURE DoBegin;
		BEGIN
			IF tiefe = 0 THEN
				Aufgabe(follow);
				follow := '';
				merker[1] := '';
				vbegin := TRUE;
				LookAhead2;
				zustand := 2;
			ELSE
				IF ende[stack] & (tiefe = wert[stack] + 1) THEN
					IF anw[stack] = '' THEN Pop ELSE ende[stack] := FALSE END;
				ELSE
					IF (tiefe = wert[stack]) & ((art[stack] = 'R') OR (art[stack] = 'C')) THEN
						IF anfang THEN
							LookAhead2;
							s.s := C2cpStrings.TAB;
							s.class := C2cpFiles.cSpace;
							Aufgabe(s.s);
						END;
					END;
					tiefe := tiefe + 1;
				END;
				IF anfang THEN zustand := 2 ELSE zustand := 28 END;
			END;
		END DoBegin;

		PROCEDURE DoBreak;
		BEGIN
			IF (art[stack] = 'C') & (tiefe = wert[stack] + 2) THEN
				IF ~ anfang THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
				Aufgabe('(* BREAK *)');
				isbreak := TRUE;
				IF ende[stack] THEN
					ende[stack] := FALSE;
					tiefe := tiefe - 1;
				END;
				LookAhead2;
				IF s.nextCh = ';' THEN s.Scan END;
				zustand := 39;
			ELSE
				Aufgabe('break');
			END;
		END DoBreak;

		PROCEDURE DoDefault;
		BEGIN
			IF (art[stack] = 'C') & ~ default THEN
				Aufgabe(C2cpStrings.TAB); Aufgabe('ELSE');
				IF ~ isbreak THEN
					name := 'L' + C2cpStrings.IntToString(labelnr);
					C2cpTable.Insert(gruppe, name, '', '', '', 'L', s.line);
					labelname := C2cpTable.GetAlias(C2cpTable.Search(gruppe, name))$;
					Aufgabe(C2cpStrings.CR); Aufgabe(Rand(2)); Aufgabe(labelname); Aufgabe(': ');
				END;
				Aufgabe(C2cpStrings.CR);
				Aufgabe(Rand(1)); Aufgabe('(* nothing to do *)' + C2cpStrings.CR);
				Aufgabe(Rand(0));
			END;
		END DoDefault;

		PROCEDURE DoEnd;
		BEGIN
			IF vbegin THEN
				vbegin := FALSE;
				tiefe := tiefe + 1;
				Aufgabe('BEGIN' + C2cpStrings.CR); Aufgabe(Rand( - 1));
			END;
			WHILE ende[stack] & (tiefe = wert[stack] + 1) DO
				IF anw[stack] # '' THEN (* END FOR *)
					MerkerAusgeben(anw[stack]);
				END;
				Pop;
				tiefe := tiefe - 1;
				Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)); Aufgabe('END;');
			END;
			IF (tiefe = wert[stack] + 1) & (anw[stack] # '') THEN (* END FOR *)
				MerkerAusgeben(anw[stack]);
				Pop;
			END;
			klammer := 0; (* offene Klammern schliessen *)
			tiefe := tiefe - 1;
			IF tiefe < 0 THEN tiefe := 0 END; (* fehlerhaftes END korrigieren *)
			IF recname # '' THEN (* END RECORD *)
				IF enum = 0 THEN Aufgabe('END;') END;
				gruppe := alt$;
				IF recdef THEN Skip END;
				IF recdef & (s.class = C2cpFiles.cName) THEN
					IF name # recname THEN
						IF onlyTypedef THEN
							C2cpTable.Insert(gruppe, recname, name, '', '', 'T', s.line);
							C2cpTable.Insert(gruppe, name, '', '', recname, 'T', s.line);
						ELSE
							C2cpTable.Insert(gruppe, recname, '', '', '', 'T', s.line);
							Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
							Aufgabe('VAR '); Aufgabe(name); Aufgabe(': '); Aufgabe(recname); Aufgabe(';');
							Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
						END;
					END;
					Skip;
					IF s.s = ',' THEN
						Skip;
						MerkeTypeKette;
						varname := name$;
						vartype := 'POINTER TO ' + recname$;
						Skip;
					ELSE
						varname := 'p' + name$;
						vartype := 'POINTER TO ' + name$;
					END;
					IF onlyTypedef THEN
						IF enum = 0 THEN
							C2cpTable.Insert(gruppe, recname, '', vartype, varname, 'T', s.line);
							C2cpTable.Insert(gruppe, varname, '', vartype, recname, 'T', s.line);
						END;
					ELSE
						C2cpTable.Insert(gruppe, recname, '', vartype, '', 'T', s.line);
						C2cpTable.Insert(gruppe, varname, '', vartype, '', 'T', s.line);
					END;
					zustand := 2;
				ELSE
					vartype := recname$;
					IF enum > 0 THEN LookAhead2 ELSE LookAhead END;
					IF s.nextCh = C2cpStrings.CR THEN
						zustand := 2;
					ELSIF s.nextCh = ';' THEN
						Skip;
						zustand := 2;
					ELSE
						Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
						typedef := FALSE;
						zustand := 23;
					END;
				END;
				typedef := FALSE;
				recdef := FALSE;
				recname := '';
				name := '';
				union := 0;
				enum := 0;
			ELSE (* END nach BEGIN *)
				IF art[stack] = 'C' THEN
					IF (*case-end*)(~ default & (tiefe = wert[stack] + 1)) THEN
						IF ~ anfang THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
						anfang := TRUE;
						IF ~ isbreak THEN
							labelnr := labelnr + 1;
							name := 'L' + C2cpStrings.IntToString(labelnr);
							C2cpTable.Insert(gruppe, name, '', '', '', 'L', s.line);
							labelname := C2cpTable.GetAlias(C2cpTable.Search(gruppe, name))$;
							IF s.s = '}' THEN Aufgabe(C2cpStrings.TAB) END;
							Aufgabe('GOTO '); Aufgabe(labelname);
							Aufgabe('; (* Warning: no break statement *)' + C2cpStrings.CR); Aufgabe(Rand(0));
						END;
					END;
					IF tiefe = wert[stack] THEN
						DoDefault;
						Pop;
					END;
				END;
				IF tiefe = 0 THEN zustand := 1 ELSE zustand := 38 END;
				IF ~ anfang THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
				IF (art[stack] = 'R') & (tiefe = wert[stack]) THEN
					LookAhead; (* END UNTIL *)
				ELSE (* alle andere END *)
					WHILE ende[stack] & (tiefe = wert[stack] + 1) DO
						tiefe := tiefe - 1;
						Aufgabe('END;' + C2cpStrings.CR); Aufgabe(Rand(0));
						DoDefault;
						Pop;
					END;
					IF s.class # C2cpFiles.cEOL THEN LookAhead END;
					IF (s.nextCh = C2cpStrings.CR) & ~ define THEN
						Skip;
						LookAhead;
						s.s := C2cpStrings.CR;
						s.class := C2cpFiles.cEOL;
					END;
					IF s.nextCh = 'e' THEN (* kommt else ? *)
						s.class := C2cpFiles.cSpace;
						zustand := 37;
					ELSE (* es kommt kein else *)
						Aufgabe('END');
						IF tiefe = 0 THEN Aufgabe(' '); Aufgabe(gruppe) END;
						Aufgabe(';');
						IF s.class = C2cpFiles.cEOL THEN Aufgabe(s.s) END;
					END;
				END;
			END;
		END DoEnd;

		PROCEDURE (*1*) StartBlock;
		BEGIN
			zurueck := 0; (* Nächster Zustand nach Ende des Kommentars *)
			tiefe := 0; (* Anzahl der offenen BEGIN's *)
			klammer := 0; (* Anzahl offener '(' Klammer *)
			zu := 0; (* Wenn offen < zu dann Klammer zu einfügen *)
			grenze := 1; (* Wenn klammer <= grenze dann Bedingung einfügen *)
			parklam := 0; (* Klammertiefe der Parameter eines Funktionaufrufs *)
			xarray := 0; (* Zähler für Array *)
			union := 0; (* Zähler für Union-Record *)
			enum := 0; (* Zähler für Enum *)
			zeile := 0; (* Zeilenzahl der Procedure und Function *)
			zaehler := 1; (* Parameterzähler für 'main' & 'printf' *)
			ma := 0; (* Position von '(' im term *)
			mi := 0; (* Position von ':=' im term *)
			mk := 0; (* Klammertiefe von ma *)
			typedef := FALSE; (* Type oder Variable definieren? *)
			recdef := FALSE; (* Recordtype oder Recordvariable definieren? *)
			onlyTypedef := FALSE; (* Recordtype and Recordvariable definieren? *)
			zeiger := FALSE; (* Die Variable ist ein Zeigertyp (POINTER) *)
			vkomma := FALSE; (* Variable mit Komma aufzählen *)
			vbegin := FALSE; (* BEGIN bis nach der VAR Definitionen zurückhalten *)
			anfang := TRUE; (* Am Anfang einer neuen Zeile *)
			vorne := TRUE; (* Am Anfang einer neuen Anweisung - vor := *)
			addklamm := FALSE; (* Bei Condition Klammer hinzufügen *)
			delklamm := FALSE; (* Bei Condition Klammer weglassen *)
			bitwise := FALSE; (* Ist bitwise AND, OR, XOR oder NOT ? *)
			int := TRUE; (* Ist der Ausdruck vom Type Integer ? *)
			isfile := FALSE; (* Ist der Ausdruck vom Type FILE ? *)
			ischar := FALSE; (* Ist der Ausdruck vom Type ARRAY OF CHAR ? *)
			default := FALSE; (* Im default oder case Teil vom select ? *)
			isbreak := FALSE; (* Ist ein break am Ende vom case Teil ? *)
			isgoto := FALSE; (* Ist ein goto, halt oder return im case Teil ? *)
			dummy := FALSE; (* Function mit dummy ? *)
			return := FALSE; (* RETURN nach ; anhängen *)
			define := FALSE; (* #define in FUNCTION oder CONST übersetzen *)
			ln := FALSE; (* StdLog.Ln; anhängen *)
			alt := ''; (* Name der letzten Gruppe *)
			gruppe := ''; (* Name der zur Zeit aktiven Procedure *)
			(* name := ''; *) (* Name merken - darf nicht gelöscht werden !!! *)
			(* alias := ''; *) (* Alias-Name merken - darf nicht gelöscht werden !!! *)
			(* symtype := ''; *) (* Type der Variable - darf nicht gelöscht werden !!! *)
			(* zeichen := ' '; *) (* Kennzeichen - darf nicht gelöscht werden !!! *)
			arrayname := ''; (* Arrayname merken *)
			arrayzeichen := 'C'; (* und sein Zeichen merken *)
			recname := ''; (* Recordname merken *)
			recpoin := ''; (* Recordpointername merken *)
			alloc := ''; (* Pointer Name für Memory Allocation *)
			varname := ''; (* Variablename merken *)
			sonder := ''; (* Ergänzungen zum Variabletype merken *)
			vartype := ''; (* Type der Variable nach dem Quellprogramm *)
			partype := ''; (* Type der letzen Variable in der Liste *)
			parvar := ' '; (* Ist Parameter vom Type VAR ? *)
			labelnr := 0; (* Label Zähler für GOTO *)
			labelname := ''; (* Aliasname des Labels *)
			term := ''; (* Für ++ oder -- sammeln *)
			plusplus := ''; (* Das nach dieser Anweisung folgende INC oder DEC *)
			follow := ''; (* Das nach der Klammer zu folgende Schlüsselwort *)
			blank := ' '; (* Leerstellen vor Kommantar *)
			relation := ''; (* Letzter Vergleich ist <, <=, =, >=, > oder # *)
			index := 0; (* Index 0..4 zum Merker für die FOR-Schleife *)
			stack := 0; (* Anzahl der gemerkten Werte *)
			Pop; (* Stack entleeren *)
			zustand := 2;
			Block;
		END StartBlock;

		PROCEDURE (*2*) Block;
		BEGIN
			IF s.s = '#' THEN zustand := 14
			ELSIF s.s = 'main' THEN DeclareVariable
			ELSIF s.s = 'auto' THEN DeclareVariable
			ELSIF s.s = 'static' THEN DeclareVariable
			ELSIF s.s = 'register' THEN DeclareVariable
			ELSIF s.s = 'extern' THEN DeclareVariable
			ELSIF s.s = 'signed' THEN DeclareVariable
			ELSIF s.s = 'unsigned' THEN DeclareVariable
			ELSIF s.s = 'short' THEN DeclareVariable
			ELSIF s.s = 'int' THEN DeclareVariable
			ELSIF s.s = 'long' THEN DeclareVariable
			ELSIF s.s = 'char' THEN DeclareVariable
			ELSIF s.s = 'float' THEN DeclareVariable
			ELSIF s.s = 'double' THEN DeclareVariable
			ELSIF s.s = 'BOOL' THEN DeclareVariable
			ELSIF s.s = 'FILE' THEN DeclareVariable
			ELSIF (s.class = C2cpFiles.cName) & (zeichen = 'T') THEN DeclareVariable
			ELSIF s.s = 'const' THEN DeclareVariable
			ELSIF s.s = 'volatile' THEN DeclareVariable
			ELSIF s.s = 'typedef' THEN zustand := 21
			ELSIF s.s = 'struct' THEN union := 0; enum := 0; zustand := 22;
			ELSIF s.s = 'union' THEN union := 1; enum := 0; zustand := 22;
			ELSIF s.s = 'enum' THEN union := 0; enum := 1; zustand := 22;
			ELSIF s.s = 'void' THEN DeclareVariable
			ELSIF s.s = 'inline' THEN LookAhead; Aufgabe('(* inline *)');
			ELSIF s.s = '{' THEN DoBegin
			ELSIF s.s = '}' THEN DoEnd
			ELSIF s.class = C2cpFiles.cSpace THEN Aufgabe(s.s)
			ELSIF s.class = C2cpFiles.cEOL THEN Aufgabe(s.s)
			ELSIF recname # ''  THEN DeclareVariable
			ELSIF tiefe > 0 THEN StartAnweisung
			ELSIF vbegin THEN StartAnweisung
			ELSIF s.class = C2cpFiles.cName THEN DeclareVariable
			ELSE Aufgabe(s.s)
			END
		END Block;

		PROCEDURE (*3*) Kommentar1;
		BEGIN
			IF C2cpStrings.Pos('*/', s.s) # - 1 THEN
				Aufgabe(C2cpStrings.Before(s.s, '*/'));
				Aufgabe('*)');
				Aufgabe(C2cpStrings.After(s.s, '*/'));
				zustand := zurueck;
			ELSE
				Aufgabe(s.s);
				IF (s.s = '*') & (s.nextCh = '/') THEN
					s.Scan;
					Aufgabe(')');
					zustand := zurueck;
				END;
			END;
		END Kommentar1;

		PROCEDURE (*4*) Kommentar2;
		BEGIN
			Aufgabe(s.s);
			IF s.nextCh = C2cpStrings.CR THEN
				Aufgabe(' *)');
				zustand := zurueck;
			END;
		END Kommentar2;

		PROCEDURE (*5*) (*6*) DoProcedure;
			VAR forward: BOOLEAN;
		BEGIN
			forward := FALSE;
			ConvertType;
			IF vartype = (*'void'*) 'POINTER' THEN
				zeichen := 'P';
				symtype := '';
			ELSE
				zeichen := 'F';
				symtype := vartype$;
			END;
			C2cpTable.Insert(gruppe, name, '', symtype, '', zeichen, 1);
			symtype := TypeNachschauen()$;
			IF zeichen = 'F' THEN follow := ListVariableType(symtype$)$ ELSE follow := ';' END;
			zustand := 7; (* mit FORWARD ... *)
			IF define THEN
				zustand := 10 (* ohne FORWARD ... *)
			ELSIF name = 'main' THEN
				Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
				zustand := 13
			ELSE (* ..... in xxx.DEF *)
				IF (s.line # C2cpTable.GetLinenumber(C2cpTable.Search(gruppe, name))) OR vbegin THEN
					Aufgabe('(* ');
					follow := follow + ' *)';
					forward := TRUE;
					zustand := 10;
				END;
			END;
			Aufgabe(sonder);
			Aufgabe('PROCEDURE ');
			IF forward THEN Aufgabe('^ ') END;
			alt := gruppe$;
			gruppe := name$;
			zeile := s.line;
			zaehler := 1;
		END DoProcedure;

		PROCEDURE (*7*) (*10*) ParameterAnfang;
		BEGIN
			Aufgabe(alias);
			parklam := 0; (* hier wird @ nicht in Klammer gesetzt *)
			IF s.s = '(' THEN
				IF s.nextCh = ')' THEN
					IF zeichen = 'F' THEN Aufgabe(' ()') END;
					Aufgabe(follow);
					follow := '';
					zustand := zustand + 2; (*9 oder 12*)
				ELSE
					Skip;
					IF s.s = 'void' THEN
						LookAhead;
						s.s := 'void';
						s.class := C2cpFiles.cName;
					END;
					IF (s.s$ = 'type') & (s.nextCh = ')') THEN 
						C2cpTable.Update('', gruppe, '', '', '', 'D', s.line);
						gruppe := alt$;
						follow := '';
						zustand := 16;
					ELSIF (s.s = 'void') & (s.nextCh = ')') THEN
						Aufgabe(follow);
						follow := '';
						zustand := zustand + 2; (*9 oder 12*)
					ELSE
						Aufgabe(' (');
						parvar := ' ';
						partype := '';
						zustand := zustand + 1; (*8 oder 11*)
						ParameterListe;
					END;
				END;
			END;
		END ParameterAnfang;

		PROCEDURE (*8*) (*11*) ParameterListe;
		BEGIN
			IF s.class = C2cpFiles.cName THEN
				MerkeType;
				LookAhead2;
				IF C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '*') OR (s.nextCh = '&') THEN
					Skip; (* Parameter: TYPE NAME *)
					MerkeTypeKette;
					ConvertType; (* Die Variable Zeiger wird in Tausche gesetzt *)
					C2cpTable.Insert(gruppe, name, '', vartype, '', zeichen, s.line);
					symtype := TypeNachschauen()$;
					IF zeichen = 'U' THEN
						Aufgabe('VAR ');
						IF C2cpStrings.Left(symtype, 11)$ = 'POINTER TO ' THEN 
							symtype := C2cpStrings.After(symtype, 'POINTER TO ')$ 
						END;
					END;
					IF zeichen = 'Z' THEN Aufgabe('VAR ') END;
					Aufgabe(alias); Aufgabe(': ');
					LookAhead2;
					IF s.nextCh = '[' THEN
						IF (zeichen # 'U') & (zeichen # 'Z') THEN
							zeichen := 'U';
							C2cpTable.Update(gruppe, name, '', '', '', zeichen, 0);
						END;
						Skip;
						IF s.nextCh = ']' THEN
							Skip;
							Aufgabe('ARRAY OF ');
						ELSE
							Aufgabe('(*[');
							WHILE s.s # ']' DO
								Skip;
								Aufgabe(s.s);
							END;
							Aufgabe('*)');
						END;
					END;
					Aufgabe(sonder); Aufgabe(symtype);
					IF s.nextCh = ',' THEN Aufgabe('; '); Skip; END;
				ELSE (* Parameter: nur Name *)
					IF define THEN
						vartype := 'INTEGER';
						C2cpTable.Insert(gruppe, name, '', vartype, '', 'V', s.line);
					END;
					symtype := TypeNachschauen()$;
					IF (zeichen = 'U') THEN
						IF C2cpStrings.Left(symtype, 11)$ = 'POINTER TO ' THEN 
							symtype := C2cpStrings.After(symtype, 'POINTER TO ')$ 
						END;
					END;
					IF (parvar # zeichen) OR (partype # symtype) THEN
						IF partype # '' THEN Aufgabe(': '); Aufgabe(partype); Aufgabe('; ') END;
						IF zeichen = 'U' THEN Aufgabe('VAR ') END;
						IF zeichen = 'Z' THEN Aufgabe('VAR ') END;
					ELSE
						IF partype # '' THEN Aufgabe(', ') END;
					END;
					Aufgabe(alias);
					parvar := zeichen;
					partype := symtype$;
					LookAhead2;
					IF s.nextCh # ',' THEN
						Aufgabe(': '); Aufgabe(partype);
					END;
				END; (* -------------------- *)
			ELSE
				IF s.s = ',' THEN LookAhead2 (* Komma und danach Leerstellen überlesen *)
				ELSE Aufgabe(s.s) END;
			END;
			IF s.nextCh = ')' THEN
				Aufgabe(')'); Aufgabe(follow);
				follow := '';
				zustand := zustand + 1; (*9 oder 12*)
			END;
		END ParameterListe;

		PROCEDURE (*9*) (*12*) ParameterEnde;
			VAR
				info, separator: ARRAY 256 OF CHAR;
		BEGIN
			LookAhead2;
			IF s.nextCh = ',' THEN (* FORWARD *)
				Skip;
				Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
				gruppe := alt$;
				typedef := FALSE;
				sonder := '';
				zustand := 23;
			ELSIF s.nextCh = ';' THEN (* FORWARD *)
				Skip;
				gruppe := alt$;
				zustand := 2;
			ELSE (* Hier fängt die neue Procedure an *)
				info := C2cpTable.GetInfo(C2cpTable.Search(alt, gruppe))$;
				IF info # '' THEN Aufgabe(' '); Aufgabe(info); Aufgabe(';') END;
				Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
				labelname := C2cpTable.FindFirst(gruppe, 'L')$; (* Label Liste *)
				separator := ' ';
				IF labelname # '' THEN
					Aufgabe('LABEL');
					WHILE labelname # '' DO
						Aufgabe(separator); Aufgabe(labelname);
						labelname := C2cpTable.FindNext()$;
						separator := ', ';
					END;
					Aufgabe(';' + C2cpStrings.CR); Aufgabe(Rand(0));
				END;
				IF define & (s.nextCh # '{') THEN
					tiefe := tiefe + 1;
					Aufgabe('BEGIN' + C2cpStrings.CR);
					IF s.nextCh = '(' THEN
						Skip;
						klammer := 1;
						delklamm := TRUE;
					END;
					s.class := C2cpFiles.cEOL;
					zustand := 32;
				ELSE
					define := FALSE;
					C2cpTable.Update(alt, gruppe, '', '', '', ' ', zeile);
					LookAhead2;
					IF s.nextCh # '{' THEN
						Aufgabe('(*-' + C2cpStrings.CR);
						follow := '-*)' + C2cpStrings.CR + Rand(0);
						s.class := C2cpFiles.cEOL;
					END;
					zustand := 2;
				END;
			END;
		END ParameterEnde;

		PROCEDURE (*13*) RunParameter;
		BEGIN
			Aufgabe(alias);
			parklam := 0; (* hier wird @ nicht in Klammer gesetzt *)
			LookAhead2;
			WHILE (zaehler < 4) & (s.nextCh # '{') & (s.nextCh # '/') DO
				Skip;
				IF (s.s = ',') OR (s.s = ')') THEN
					IF (name # 'main') & (name # 'void') THEN
						IF C2cpTable.GetAlias(C2cpTable.Search('', 'strlen'))$ = 'Length' THEN
							CASE zaehler OF (* Pascal orientiert *)
							| 1: C2cpTable.Insert(gruppe, name, 'ParamCount', 'INTEGER', '', 'F', s.line);
							| 2: C2cpTable.Insert(gruppe, name, 'ParamStr', 'STRING', '', 'F', s.line);
							| 3: C2cpTable.Insert(gruppe, name, 'EnvStr', 'STRING', '', 'F', s.line);
							END;
						ELSE
							CASE zaehler OF (* C orientiert *)
							| 1: C2cpTable.Insert(gruppe, name, 'argc', 'INTEGER', '', 'I', s.line);
							| 2: C2cpTable.Insert(gruppe, name, 'argv', 'CHAR', '', 'Z', s.line);
							| 3: C2cpTable.Insert(gruppe, name, 'envp', 'CHAR', '', 'Z', s.line);
							END;
						END;
						C2cpTable.Insert(gruppe, name, alias, symtype, '', zeichen, s.line);
					END;
					zaehler := zaehler + 1;
					IF s.s = ')' THEN zaehler := 4
					END
				END;
				LookAhead2;
			END;
			Aufgabe(follow);
			follow := '';
			IF s.nextCh = '/' THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
			ELSIF s.nextCh = '{' THEN ParameterEnde
			END;
		END RunParameter;

		PROCEDURE (*14*) Preprocessor;
		BEGIN
			IF s.s = 'include' THEN
				LookAhead;
				Aufgabe('(* #include ');
				zustand := 15;
			ELSIF s.s = 'define' THEN
				define := TRUE;
				zeiger := FALSE;
				zustand := 19;
			ELSIF s.s = 'asm' THEN
				C2cpTable.Update('', gruppe, '', '', 'ASSEMBLER', ' ', 0);
				Aufgabe('ASM');
				tiefe := tiefe + 1;
				zustand := 18;
			ELSE
				Aufgabe('(* #'); Aufgabe(s.s);
				zustand := 16;
			END;
		END Preprocessor;

		PROCEDURE (*15*) IncludeStart;
		BEGIN
			IF C2cpStrings.Left(s.s, 1)$ = "'" THEN (* "..." -> '...' in Tausche *)
				Aufgabe(C2cpStrings.Substr(s.s, 1, LEN(s.s$) - 2));
			ELSE
				IF s.s = '<' THEN Skip END;
				Aufgabe(name); (* weil Name und nicht Alias !!! *)
				IF s.nextCh = '.' THEN
					Skip;
					Aufgabe(s.s);
					Skip;
					Aufgabe(name); (* weil Name und nicht Alias !!! *)
				END;
				IF s.nextCh = '>' THEN Skip END;
			END;
			zustand := 17;
		END IncludeStart;

		PROCEDURE (*16*) (*20*) (*24*) CopyUntil (wort, anhang, abschluss: ARRAY OF CHAR);
			VAR merke: ARRAY 256 OF CHAR;
		BEGIN
			IF s.s = wort THEN (* Ende: Wort gefunden *)
				Aufgabe(anhang); Aufgabe(abschluss);
				define := FALSE;
				zustand := 2;
			ELSE
				IF s.nextCh = '/' THEN
					merke := s.s$;
					s.Scan;
					IF s.nextCh = '*' THEN (* Ende: Kommentar gefunden *)
						s.Scan;
						Aufgabe(anhang); Aufgabe(merke); Aufgabe('(*');
						define := FALSE;
						zustand := 3;
						zurueck := 2;
					ELSIF s.nextCh = '/' THEN (* Ende: Kommentar gefunden *)
						s.Scan;
						Aufgabe(anhang); Aufgabe(merke); Aufgabe('(* //');
						define := FALSE;
						zustand := 4;
						zurueck := 2;
					ELSE
						Look;
						Aufgabe(merke); Aufgabe(s.s);
					END;
				ELSE
					Aufgabe(s.s);
				END;
			END;
		END CopyUntil;

		PROCEDURE (*17*) IncludeName;
		BEGIN
			zustand := 16;
			CopyUntil(C2cpStrings.CR, ' *)', C2cpStrings.CR);
		END IncludeName;

		PROCEDURE (*18*) DoAsmBlock;
			VAR newline: BOOLEAN;
				a: ARRAY 256 OF CHAR;
		BEGIN
			newline := FALSE;
			IF s.class = C2cpFiles.cEOL THEN
				newline := TRUE;
				Skip;
			END;
			IF s.s = '#' THEN
				Skip; (* endasm *)
				IF ~ vbegin THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)); Aufgabe('END;') END;
				vbegin := FALSE;
				tiefe := tiefe - 1;
				zustand := 2;
			ELSE
				IF newline THEN
					Aufgabe(C2cpStrings.CR);
					IF s.nextCh # ':' THEN Aufgabe(Rand(0)) END;
				END;
				IF s.s = ';' THEN (* Kommentar gefunden *)
					Aufgabe('(* ;');
					zustand := 4;
					zurueck := 18;
				ELSE
					IF s.nextCh = ':' THEN
						a := C2cpStrings.ToUpper(s.s)$;
						IF (a # 'CS') & (a # 'SS') & (a # 'DS') & (a # 'ES') THEN 
							C2cpTable.Insert(gruppe, name, '@' + name, '', '', 'S', s.line) 
						END;
					END;
					Aufgabe(s.s);
				END;
			END;
		END DoAsmBlock;

		PROCEDURE (*19*) DoConstant;
		BEGIN
			IF s.class # C2cpFiles.cSpace THEN
				(* Achtung: Kein LookAhead hier, *)
				(* weil kein Leerzeichen zwischen Name und '(' sein darf! *)
				IF (zeichen = 'D') OR (zeichen = 'S') OR C2cpTable.Reserviert(s.s) THEN
					C2cpTable.Insert(gruppe, name, '', '', '', 'S', s.line);
					Aufgabe('(* #define '); Aufgabe(s.s);
					zustand := 16;
				ELSIF (zeichen = 'F') OR (s.nextCh = '(') OR (zeichen = 'P') OR (s.nextCh = '{') THEN
					vartype := symtype$;
					IF vartype = '' THEN vartype := 'INTEGER' END;
					IF zeichen = 'P' THEN term := '' ELSE term := alias + ' := ' END;
					DoProcedure;
					IF s.nextCh # '(' THEN (* FUNCTION ohne Parameter *)
						Aufgabe(alias);
						Aufgabe(follow);
						follow := '';
						zustand := 12;
					END;
				ELSE
					LookAhead;
					IF (s.nextCh = C2cpStrings.CR) OR (s.nextCh = '/') THEN
						Aufgabe('(* $DEFINE '); Aufgabe(name); Aufgabe(' *)');
						IF s.nextCh = '/' THEN Aufgabe(' ') END;
						define := FALSE;
						zustand := 2;
					ELSE
						vartype := '';
						C2cpTable.Insert(gruppe, name, '', vartype, '', 'C', s.line);
						Aufgabe('CONST '); Aufgabe(alias); Aufgabe(' = ');
						Skip;
						IF s.s = '(' THEN
							klammer := 1;
							delklamm := TRUE;
							term := '';
							zustand := 35;
						ELSIF (C2cpStrings.Right(s.s, 1)$ = 'H') & (LEN(s.s$) = 4) THEN
							s.s[3] := 'X'; (* change H to X - Konstante als CHAR statt INTEGER *)
							Aufgabe(s.s); (* !!! ist nicht immer sinnvoll !!! *)
							zustand := 20;
						ELSE
							Aufgabe(s.s);
							zustand := 20;
						END;
					END;
				END;
				vorne := FALSE;
				plusplus := '';
			END;
		END DoConstant;

		PROCEDURE (*21*) DeclareType;
		BEGIN
			IF (s.class # C2cpFiles.cSpace) & (s.class # C2cpFiles.cEOL) THEN
				MerkeType;
				IF vartype = 'struct' THEN
					typedef := TRUE;
					recdef := TRUE;
					union := 0;
					enum := 0;
					zustand := 22;
				ELSIF vartype = 'union' THEN
					typedef := TRUE;
					recdef := TRUE;
					union := 1;
					enum := 0;
					zustand := 22;
				ELSIF vartype = 'enum' THEN
					typedef := TRUE;
					recdef := TRUE;
					union := 0;
					enum := 1;
					zustand := 22;
				ELSE
					typedef := TRUE;
					recdef := FALSE;
					zustand := 23;
				END;
			END;
		END DeclareType;

		PROCEDURE (*22*) DeclareRecord;
		BEGIN
			IF (s.class # C2cpFiles.cSpace) & (s.class # C2cpFiles.cEOL) THEN
				zeiger := FALSE;
				IF s.s # '{' THEN
					LookAhead2;
					IF s.nextCh = '{' THEN Skip END;
				END;
				IF s.s = '{' THEN
					LookAhead;
					vartype := 'RECORD';
					IF enum # 0 THEN vartype := 'INTEGER(*enum*)' END;
					IF union # 0 THEN vartype := 'RECORD(*union*)' END;
					IF (name = 'struct') OR (name = 'union') OR (name = 'enum') THEN
						name := name + C2cpStrings.IntToString(s.line);
						alias := C2cpTable.GetAlias(C2cpTable.Search(gruppe, name))$;
						onlyTypedef := TRUE;
					ELSE
						onlyTypedef := FALSE;
					END;
					tiefe := tiefe + 1;
					recname := name$; (* weil Name und nicht Alias !!! *)
					C2cpTable.Insert(gruppe, recname, '', vartype, '', 'T', s.line);
					Aufgabe(sonder); Aufgabe('TYPE ');
					recpoin := C2cpTable.GetInfo(C2cpTable.Search(gruppe, recname))$;
					IF recpoin # '' THEN
						Aufgabe(recpoin); Aufgabe(' = POINTER TO '); 
						IF onlyTypedef THEN
							Aufgabe(alias); Aufgabe(';' + C2cpStrings.CR); Aufgabe(Rand(0));
							Aufgabe(alias); Aufgabe(' ='); 
						END;
						recpoin := '';
					ELSE
						Aufgabe(alias); Aufgabe(' ='); 
					END;
					IF enum = 0 THEN 
						Aufgabe(' RECORD ') 
					ELSE 
						IF NamedInteger THEN
							Aufgabe(' ( '); 
						ELSE
							Aufgabe(' INTEGER(*enum*);' + C2cpStrings.CR); Aufgabe(Rand(-1)); Aufgabe('CONST '); 
						END;
					END;
					IF union > 0 THEN
						Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)); Aufgabe('CASE union:INTEGER OF ');
					END;
					alt := gruppe$;
					gruppe := recname$;
					zustand := 2;
				ELSE
					recpoin := C2cpTable.GetInfo(C2cpTable.Search(alt, name))$;
					vartype := alias$;
					union := 0;
					enum := 0;
					zustand := 23;
				END;
			END;
		END DeclareRecord;

		PROCEDURE (*23*) ListVariable;
			VAR si, i, n: INTEGER;
		BEGIN
			KlammerLesen; (* eingeklammerte Variable lesen *)
			IF s.class = C2cpFiles.cSpace THEN
				blank := s.s$
			ELSIF s.class = C2cpFiles.cEOL THEN
				IF vkomma THEN AufgabeVariableType END;
				Aufgabe(s.s);
			ELSIF s.s = '*' THEN
				(* Die Variable Zeiger wird normalerweise in Tausche gesetzt *)
				(* Nur wenn Blanks zwischen * und Variablename ist, dann hier *)
				zeiger := TRUE;
			ELSIF (s.s = ',') & (zeichen # 'T') THEN
				vkomma := TRUE;
				zeiger := FALSE;
			ELSIF (s.s = ',') OR (s.s = ';') OR (s.s = ':=') THEN
				AufgabeVariableType;
				IF s.s = ',' THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
				IF s.s = ';' THEN zustand := 28 END;
				IF s.s = ':=' THEN zustand := 2 END;
			ELSIF s.class = C2cpFiles.cName THEN
				MerkeTypeKette;
				KlammerLesen; (* eingeklammerte Variable lesen *)
				varname := name$;
				IF symtype = '' THEN symtype := vartype$ END;
				IF partype = '' THEN partype := symtype$ END;
				LookAhead2;
				IF vkomma & ((partype # symtype) OR (xarray > 0) OR (s.nextCh = '(') OR (s.nextCh = '[')) THEN
					AufgabeVariableType; Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
				END;
				IF vkomma THEN
					IF NamedInteger OR (enum = 0) THEN Aufgabe(', ') END
				ELSIF (recname = '') & (s.nextCh # '(') THEN
					IF ~((zeichen = 'C') & (sonder = '(*const*)')) THEN Aufgabe(sonder) END;
					CASE zeichen OF
					| 'C': Aufgabe('CONST ');
					| 'T': Aufgabe('TYPE ');
					ELSE Aufgabe('VAR ');
					END;
				END;
				IF s.nextCh = '(' THEN (* 1. Function *)
					IF (zeichen <= ' ') OR (zeichen = 'F') OR (zeichen = 'P') THEN
						DoProcedure;
					ELSE
						Aufgabe(s.s); zustand := 1;
					END;
				ELSIF s.nextCh = '[' THEN (* 2. Aarry *)
					Skip;
					LookAhead2;
					ConvertType;
					IF C2cpStrings.Substr(vartype, 0, 4)$ = 'CHAR' THEN
						IF C2cpTable.GetAlias(C2cpTable.Search('', 'strlen'))$ = 'Length' THEN
							vartype := 'STRING'; (* Pascal *)
						ELSE
							IF s.nextCh = ']' THEN vartype := 'ARRAY OF CHAR' END; (* C *)
						END;
					END;
					C2cpTable.Insert(gruppe, varname, '', vartype, '1', zeichen, s.line);
					partype := TypeNachschauen()$;
					Aufgabe(alias);
					xarray := C2cpStrings.StringToInt(C2cpTable.GetInfo(C2cpTable.Search(gruppe, varname)));
					IF ~ zeiger & ((symtype = 'ARRAY OF CHAR') OR (symtype = 'STRING')) THEN
						Skip;
						IF s.s # ']' THEN
							Aufgabe('(* 0..');
							WHILE s.s # ']' DO
								Aufgabe(s.s);
								Skip;
							END;
							Aufgabe('-1 *)');
						END;
						LookAhead2;
						IF s.nextCh = '[' THEN Skip ELSE s.s := ']' END;
						xarray := 0;
					END;
					IF s.s # ']' THEN
						IF zeichen = 'T' THEN Aufgabe(' = ') ELSE Aufgabe(': ') END;
						Aufgabe('ARRAY ');
						IF s.nextCh = ']' THEN
							IF xarray = 1 THEN Aufgabe('xxx') ELSE Aufgabe(C2cpStrings.IntToString(xarray)) END;
						END;
						zustand := 25;
					END;
				ELSIF s.nextCh = ':' THEN (* 3. Bit *)
					Skip;
					Skip;
					si := 1; n := SHORT(s.i); FOR i := 1 TO n DO si := 2 * si END; si := si - 1; (* si := (1 SHL s.i) - 1*)
					vartype := '0..' + C2cpStrings.IntToString(si) + ' (* ' + s.s + ' Bits Wide *)';
					C2cpTable.Insert(gruppe, varname, '', vartype, '', zeichen, s.line);
					partype := TypeNachschauen()$;
					Aufgabe(alias);
				ELSE (* 4. Rest *)
					ConvertType;
					IF sonder = '(*const*)' THEN zeichen := 'C' END;
					C2cpTable.Insert(gruppe, varname, '', vartype, '', zeichen, s.line);
					partype := TypeNachschauen()$;
					Aufgabe(alias);
					IF ~NamedInteger & (enum > 0) THEN
						Aufgabe(' = '); Aufgabe(C2cpStrings.IntToString(enum)); INC(enum); Aufgabe('; ');
					END
				END;
			ELSIF s.s = '{' THEN
				C2cpTable.Insert(gruppe, varname, '', vartype, '', zeichen, s.line);
				Aufgabe(' = (');
				zustand := 24;
			ELSIF (s.s = '}') & (enum > 0) THEN
				IF NamedInteger THEN Aufgabe(' );') END;
				vkomma := FALSE;
				zeiger := FALSE;
				DoEnd; 
			ELSE
				Aufgabe(s.s);
			END;
			IF s.s = ':=' THEN
				(*
				zeichen := C2cpTable.GetSign(C2cpTable.Search(gruppe, varname));
				IF sonder = '(*const*)' THEN zeichen := 'C' END;
				IF zeichen # 'C' THEN zeichen := 'I' END;
				C2cpTable.Update(gruppe, varname, '', '', C2cpStrings.Str(xarray), zeichen, 0);
				*)
				arrayname := alias$;
				arrayzeichen := zeichen;
				xarray := 1;
				IF zeichen = 'C' THEN
					index := 0;
					Aufgabe(' =');
				ELSIF (zeichen = 'I')
				OR (zeichen = 'V') (* A, X, Z - darf nicht nach I geändert werden ! *)
				OR (zeichen = 'Z') & ((vartype = 'ARRAY OF CHAR') OR (vartype = 'STRING'))
				OR ~ vbegin (* nur bei vbegin darf der Merker benutzt werden *)
				THEN (* 'I' = (VAR)CONST ... : ... = ... ; *)
					index := 0; (* Das Zeichen wird nicht nach 'I' geändert *)
					Aufgabe('; ');
					LookAhead2;
					IF s.nextCh # '{' THEN Aufgabe(arrayname); Aufgabe(' := ') END;
					(* Aufgabe('; ' + alias + ' :='); *)
				ELSE (* 'U' = VAR ... ; BEGIN ... := ... ; *)
					Aufgabe('; (* '); (* nicht in den Merker *)
					index := 1; (* Ab hier wird der Merker benutzt .. *)
					Aufgabe(alias); Aufgabe(' :='); (* in den Merker *)
				END;
				zustand := 26;
			END;
		END ListVariable;

		PROCEDURE (*25*) ArrayVariable;
		BEGIN
			IF s.class # C2cpFiles.cSpace THEN
				IF s.s = ']' THEN
					LookAhead2;
					IF s.nextCh = '[' THEN
						Skip;
						Aufgabe(', ');
						LookAhead2;
						IF s.nextCh = ']' THEN
							IF xarray = 1 THEN Aufgabe('xxx') ELSE Aufgabe(C2cpStrings.IntToString(xarray)) END;
						END;
					ELSE
						xarray := 1;
						zustand := 23;
					END;
				ELSE
					Aufgabe(s.s);
				END;
			END;
		END ArrayVariable;

		PROCEDURE (*26*) InitVariable;
		BEGIN
			IF s.s = '{' THEN klammer := klammer + 1 END;
			IF s.s = '}' THEN klammer := klammer - 1 END;
			IF (klammer = 0) & (s.s = ',') THEN
				IF index # 0 THEN
					merker[1] := merker[1] + ', ';
					index := 0;
					Aufgabe(' *)');
				ELSE
					Aufgabe(';');
					zeichen := C2cpTable.GetSign(C2cpTable.Search(gruppe, varname));
					IF sonder = '(*const*)' THEN zeichen := 'C' END;
					IF zeichen # 'C' THEN zeichen := 'I' END;
					C2cpTable.Update(gruppe, varname, '', '', C2cpStrings.IntToString(xarray), zeichen, 0);
					xarray := 0;
				END;
				zustand := 27;
			ELSIF s.s = ',' THEN
				IF arrayzeichen = 'C' THEN;
					Aufgabe(', ');
				ELSE
					Aufgabe('; ');
					IF xarray MOD 4 = 0 THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)); END;
					Aufgabe(arrayname); Aufgabe('['); Aufgabe(C2cpStrings.IntToString(xarray)); Aufgabe('] := ');
				END;
				xarray := xarray + 1;
			ELSIF s.s = ';' THEN
				IF index # 0 THEN
					merker[1] := merker[1] + ', ';
					index := 0;
					Aufgabe(' *)');
				ELSE
					Aufgabe(';');
					zeichen := C2cpTable.GetSign(C2cpTable.Search(gruppe, varname));
					IF sonder = '(*const*)' THEN zeichen := 'C' END;
					IF zeichen # 'C' THEN zeichen := 'I' END;
					C2cpTable.Update(gruppe, varname, '', '', C2cpStrings.IntToString(xarray), zeichen, 0);
					xarray := 0;
				END;
				zustand := 28;
			ELSE
				IF arrayzeichen = 'C' THEN;
					IF s.s = '{' THEN Aufgabe('(')
					ELSIF s.s = '}' THEN Aufgabe(')')
					ELSE Aufgabe(s.s)
					END;
				ELSE
					IF s.s = '{' THEN
						Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
						Aufgabe(arrayname); Aufgabe('['); Aufgabe(C2cpStrings.IntToString(xarray - 1)); Aufgabe('] := ');
					ELSIF s.s = '}' THEN (* Aufgabe(')') *)
					ELSE Aufgabe(s.s)
					END;
				END
			END;
		END InitVariable;

		PROCEDURE (*27*) NeueVarZeile;
		BEGIN
			IF s.class = C2cpFiles.cSpace THEN
				IF s.nextCh # C2cpStrings.CR THEN Aufgabe(s.s) END;
				zustand := 27;
			ELSE
				IF s.class # C2cpFiles.cEOL THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
				zustand := 23;
				ListVariable;
			END;
		END NeueVarZeile;

		PROCEDURE (*28*) NeueBlockZeile;
		BEGIN
			int := TRUE;
			isfile := FALSE;
			ischar := FALSE;
			typedef := FALSE;
			sonder := '';
			IF s.class = C2cpFiles.cSpace THEN
				IF s.nextCh # C2cpStrings.CR THEN Aufgabe(s.s) END;
				zustand := 28;
			ELSE
				IF s.class = C2cpFiles.cEOL THEN
					Aufgabe(s.s);
					zustand := 2;
				ELSE
					Aufgabe(C2cpStrings.CR); 
					IF (tiefe = 0) & (C2cpStrings.Left(follow, 3)$ = '-*)') THEN
						IF s.s # '{' THEN Aufgabe(Rand(0)); Aufgabe('--- ') END;
					ELSE
						IF s.s = '}' THEN Aufgabe(Rand( - 1)) ELSE Aufgabe(Rand(0)) END;
					END;
					anfang := TRUE;
					zustand := 2;
					IF s.class # C2cpFiles.cSpace THEN Block END;
				END;
			END;
		END NeueBlockZeile;

		PROCEDURE DoIf;
		BEGIN
			LookAhead2;
			IF ~ anfang THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
			Push('I', tiefe, TRUE);
			term := 'IF ';
			ma := 4;
			follow := ' THEN ';
			zustand := 33;
		END DoIf;

		PROCEDURE DoElse;
		BEGIN
			Skip;
			IF s.class = C2cpFiles.cEOL THEN Skip END;
			Push('I', tiefe, TRUE);
			tiefe := tiefe + 1;
			Aufgabe('ELSE ');
			NeueAnwZeile;
		END DoElse;

		PROCEDURE ExFragezeichen;
			VAR bedingung: ARRAY 256 OF CHAR;
		BEGIN
			Push('?', tiefe, TRUE);
			tiefe := tiefe + 1;

			bedingung := term$;
			WHILE C2cpStrings.Pos(':=', bedingung) # - 1 DO bedingung := C2cpStrings.After(bedingung, ':=')$ END;
			WHILE C2cpStrings.Pos(',', bedingung) # - 1 DO bedingung := C2cpStrings.After(bedingung, ',')$ END;

			term := C2cpStrings.Before(term, bedingung)$;
			bedingung := C2cpStrings.Trim(bedingung)$;

			IF define THEN
				IF (C2cpStrings.Left(bedingung, 1)$ = '(') & (bedingung[LEN(bedingung$) - 1] = ')') THEN
					bedingung := C2cpStrings.Trim(C2cpStrings.Substr(bedingung, 1, LEN(bedingung$) - 2))$;
				END;
			ELSE
				WHILE (C2cpStrings.Left(bedingung, 1)$ = '(') DO
					bedingung := C2cpStrings.Trim(C2cpStrings.Substr(bedingung, 1, LEN(bedingung$) - 1))$;
				END;
				WHILE (bedingung[LEN(bedingung$) - 1] = ')') DO
					bedingung := C2cpStrings.Trim(C2cpStrings.Substr(bedingung, 0, LEN(bedingung$) - 1))$;
				END;
			END;

			Aufgabe('IF '); Aufgabe(bedingung);
			IF relation = '' THEN Aufgabe(' # 0') END;
			Aufgabe(' THEN'); Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
		END ExFragezeichen;

		PROCEDURE DoFor;
		BEGIN
			LookAhead2;
			IF ~ anfang THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
			IF s.nextCh = '(' THEN
				Skip;
				klammer := 1;
			END;
			Aufgabe('(* FOR ');
			Push('F', tiefe, TRUE);
			term := '';
			index := 1;
			merker[1] := '';
			merker[2] := '';
			merker[3] := '';
			merker[4] := '';
			follow := 'DO *)';
			zustand := 35;
		END DoFor;

		PROCEDURE ForAnweisung;
			VAR sv, sa, sb, sr, si: ARRAY 256 OF CHAR;
		BEGIN
			index := 0;
			merker[1] := C2cpStrings.Trim(C2cpStrings.Before(merker[1], ';'))$;
			merker[2] := C2cpStrings.Trim(C2cpStrings.Before(merker[2], ';'))$;
			merker[3] := C2cpStrings.Trim(C2cpStrings.Before(merker[3], 'DO'))$;
			sv := C2cpStrings.Trim(C2cpStrings.Before(merker[1], ':='))$;
			sa := C2cpStrings.Trim(C2cpStrings.After(merker[1], ':='))$;
			sb := C2cpStrings.Trim(C2cpStrings.After(merker[2], relation))$;
			sr := C2cpStrings.Trim(C2cpStrings.Before(merker[3], '('))$;
			si := C2cpStrings.Trim(C2cpStrings.After(merker[3], sr))$;
			IF (C2cpStrings.After(sa, ':=')$ = '')
			 & (sv = C2cpStrings.Trim(C2cpStrings.Before(merker[2], relation))$)
			 & (relation # '=')
			 & ((sr = 'INC') OR (sr = 'DEC'))
			 & ('(' + sv + ')' = si) THEN
				Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
				Aufgabe('FOR '); Aufgabe(sv); Aufgabe(' := '); Aufgabe(sa); Aufgabe(' TO '); Aufgabe(sb);
				IF sr = 'INC' THEN
					IF relation = '<' THEN Aufgabe('-1') END;
				ELSE
					IF relation = '>' THEN Aufgabe('+1') END;
					Aufgabe(' BY -1');
				END;
			ELSE
				MerkerAusgeben(merker[1]);
				MerkerAusgeben(merker[4]);
				IF merker[2] = '' THEN merker[2] := 'TRUE' END;
				Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
				Aufgabe('WHILE '); Aufgabe(merker[2]);
				anw[stack] := merker[3] + ', ' + merker[4];
			END;
			Aufgabe(' DO');
		END ForAnweisung;

		PROCEDURE DoWhile;
		BEGIN
			LookAhead2;
			IF (art[stack] = 'R') & (tiefe = wert[stack]) THEN
				term := 'UNTIL ~(';
				ma := 9;
				Pop;
				follow := ')';
				zustand := 33;
			ELSE
				IF ~ anfang THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
				term := 'WHILE ';
				ma := 7;
				Push('W', tiefe, TRUE);
				follow := ' DO ';
				zustand := 33;
			END;
		END DoWhile;

		PROCEDURE DoRepeat;
		BEGIN
			IF ~ anfang THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
			Aufgabe('REPEAT');
			Push('R', tiefe, FALSE);
			follow := '';
			zustand := 39;
		END DoRepeat;

		PROCEDURE DoSwitch;
		BEGIN
			IF ~ anfang THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
			Push('C', tiefe, FALSE);
			LookAhead2;
			IF s.nextCh = '(' THEN
				s.Scan;
				klammer := klammer + 1;
			END;
			term := 'CASE ';
			follow := ' OF ';
			isbreak := TRUE;
			zustand := 32;
		END DoSwitch;

		PROCEDURE DoCase;
			VAR merke: ARRAY 256 OF CHAR;
		BEGIN
			merke := s.s$;
			ende[stack] := FALSE;
			IF tiefe > wert[stack] + 1 THEN (* Wenn kein END war *)
				default := FALSE;
				DoEnd;
				Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
			END;

			IF merke = 'case' THEN (* s.s = 'case' *)
				Aufgabe('| ');
				LookAhead2; (* Leerstelle überlesen *)
				WHILE s.s # ':' DO
					s.Scan;
					Look;
					Aufgabe(s.s);
				END;
				default := FALSE;
			ELSE (* s.s = 'default' *)
				WHILE s.s # ':' DO s.Scan END;
				Aufgabe('ELSE');
				default := TRUE;
			END;

			LookAhead2;
			IF s.nextCh = '{' THEN
				Skip;
				ende[stack] := FALSE;
			ELSE
				ende[stack] := TRUE;
			END;
			tiefe := tiefe + 1;
			IF ~ isbreak THEN
				name := 'L' + C2cpStrings.IntToString(labelnr);
				C2cpTable.Insert(gruppe, name, '', '', '', 'L', s.line);
				labelname := C2cpTable.GetAlias(C2cpTable.Search(gruppe, name))$;
				Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)); Aufgabe(labelname); Aufgabe(': ');
			END;
			Aufgabe(' ');
			isbreak := FALSE;
			zustand := 39;
		END DoCase;

		PROCEDURE DoReturn;
			VAR i: INTEGER;
		BEGIN
			LookAhead2;
			IF s.nextCh = ';' THEN
				Skip;
				Aufgabe('RETURN;');
				zustand := 38;
			ELSE
				name := gruppe$;
				i := C2cpTable.Search('', name);
				alias := C2cpTable.GetAlias(i)$;
				symtype := C2cpTable.GetType(i)$;
				zeichen := C2cpTable.GetSign(i);
				IF zeichen = 'P' THEN
					IF symtype = '' THEN symtype := 'INTEGER' END;
					C2cpTable.Update('', name, '', symtype, '', 'F', 0);
				END;
				IF ~ anfang THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
				term := alias + ' := ';
				vorne := FALSE;
				return := TRUE;
				zustand := 35;
				IF s.nextCh = '(' THEN
					Skip;
					klammer := 1;
				END;
			END;
			IF (art[stack] = 'C') & (tiefe = wert[stack] + 2) THEN
				isbreak := TRUE;
				isgoto := TRUE;
			END;
		END DoReturn;

		PROCEDURE DoGoto;
		BEGIN
			Skip;
			Aufgabe('GOTO '); Aufgabe(s.s);
			C2cpTable.Insert(gruppe, name, '', '', '', 'L', s.line);
			IF (art[stack] = 'C') & (tiefe = wert[stack] + 2) THEN
				isbreak := TRUE;
				isgoto := TRUE;
			END;
		END DoGoto;

		PROCEDURE DoHalt;
		BEGIN
			Aufgabe('HALT ');
			LookAhead2;
			IF (art[stack] = 'C') & (tiefe = wert[stack] + 2) THEN
				isbreak := TRUE;
				isgoto := TRUE;
			END;
		END DoHalt;

		PROCEDURE DoInOut (z1, z2: INTEGER; a2: ARRAY OF CHAR);
		BEGIN
			ln := FALSE;
			IF C2cpTable.GetAlias(C2cpTable.Search('', 'FILE'))$ = 'TEXTFILE' THEN
				Aufgabe(s.s); Aufgabe(' '); (* C orientiert *)
				grenze := 0; (* scanf -> scanf & printf -> printf *)
				IF s.s[0] = 'f' THEN zaehler := 2 ELSE zaehler := 1 END;
				LookAhead2;
				zustand := z1;
			ELSE (* Pascal orientiert *)
				Aufgabe(a2); (* scanf -> READ & printf -> StdLog. *)
				zustand := z2;
			END;
		END DoInOut;

		PROCEDURE ExNew (hinweis: ARRAY OF CHAR);
		BEGIN
			LookAhead2;
			IF s.nextCh = '(' THEN
				Skip;
				grenze := klammer;
				klammer := klammer + 1;
				IF mi = 0 THEN
					alloc := C2cpStrings.Trim(C2cpStrings.Before(term, ':='))$;
					term := '';
				ELSE
					IF term[ma - 1] = '(' THEN
						grenze := grenze - 1;
						alloc := C2cpStrings.Substr(term, ma, mi - ma - 1)$;
					ELSE
						alloc := C2cpStrings.Substr(term, ma - 1, mi - ma)$;
					END;
					term := C2cpStrings.Substr(term, 0, ma - 1)$;
				END;
				term := term + 'GetMem' + hinweis + ' (' + alloc + ', ';
				IF mi # 0 THEN mi := LEN(term$) END;
				LookAhead2;
			END;
		END ExNew;

		PROCEDURE ExFree;
		BEGIN
			alloc := 'FreeMem';
			term := term + alloc + ' ';
			grenze := 1;
		END ExFree;

		PROCEDURE DoString;
			VAR
				op, merke: ARRAY 256 OF CHAR;
		BEGIN
			IF C2cpTable.GetAlias(C2cpTable.Search('', 'strlen'))$ = 'Length' THEN
				op := C2cpStrings.Right(s.s, 3)$; (* Pascal orientiert *)
				Skip;
				IF s.s = '(' THEN
					klammer := klammer + 1;
					Skip;
				END;
				merke := '';
				WHILE s.s # ',' DO
					merke := merke + s.s;
					Skip;
				END;
				Aufgabe(merke); Aufgabe(' := ');
				IF op = 'cat' THEN Aufgabe(merke); Aufgabe(' + ') END;
				Skip;
				WHILE (s.s # ',') & (s.s # ')') DO
					Expression;
					Skip;
				END;
				IF s.s = ',' THEN
					Aufgabe('Left('); Aufgabe(term); Aufgabe(', ');
				ELSE
					Aufgabe(term);
					klammer := klammer - 1;
				END;
				term := '';
			ELSE (* C orientiert *)
				zustand := 35;
				Expression;
			END;
		END DoString;

		PROCEDURE DoStringCopy;
		BEGIN
			IF C2cpTable.GetAlias(C2cpTable.Search('', 'strlen'))$ = 'Length' THEN
				Aufgabe(s.s); (* Pascal orientiert *)
				Skip;
				Aufgabe(' '); Aufgabe(s.s);
				Skip;
				Aufgabe(s.s); Aufgabe(', ');
				Skip;
				IF s.s = ',' THEN Aufgabe('+0') END;
				Aufgabe(s.s);
				IF s.s # ',' THEN LookAhead2 END;
			ELSE (* C orientiert *)
				zustand := 35;
				Expression;
			END;
		END DoStringCopy;

		PROCEDURE DoIntr;
			VAR merke: ARRAY 256 OF CHAR; ch: ARRAY 2 OF CHAR;
		BEGIN
			merke := '';
			IF s.s = 'Intr' THEN ch := ',' ELSE ch := '(' END;
			Aufgabe(s.s); Aufgabe(' ');
			WHILE s.s # ch DO
				Skip;
				Aufgabe(s.s);
			END;
			WHILE s.s # ')' DO
				IF (s.class = C2cpFiles.cName) & (alias # merke) THEN
					IF merke = '' THEN
						Aufgabe(s.s);
						merke := alias$; (* muß alias sein und nicht s.s *)
					ELSE
						C2cpTable.Update(gruppe, name, merke, 'Registers', '', 'V', 0);
					END;
				END;
				Skip;
			END;
			Aufgabe(s.s);
			LookAhead2;
		END DoIntr;

		PROCEDURE ExKey;
			VAR wort: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTable.Insert('3', 'conio', 'Terminal', '', '', 'M', 1);
			LookAhead2; IF s.nextCh = '(' THEN Skip END;
			Skip; wort := s.s$;
			LookAhead2; IF s.nextCh = ')' THEN Skip END;

			IF wort = '1' THEN (* bioskey(1) *)
				LookAhead2;
				IF (s.nextCh = '!') OR (s.nextCh = '=') THEN
					Skip;
					wort := s.s$;
					Skip;
				END;
				IF wort = '=' THEN term := term + '~' END; (* ==  = 0  nicht .. *)
				term := term + 'KeyPressed'; (* !=  # 0  gedrückt *)
				relation := '#';
			ELSE
				IF (term = '') & (zustand = 35) THEN
					term := 'dummy := ';
					C2cpTable.Insert(gruppe, 'dummy', '', 'INTEGER', '', 'V', s.line);
				END;
				term := term + 'ORD(ReadKey)'; (* bioskey(0) *)
			END;
		END ExKey;

		PROCEDURE ExParamStr;
		BEGIN
			term := term + s.s;
			LookAhead2;
			IF s.nextCh = '[' THEN
				Skip;
				term := term + '(';
				Skip;
				WHILE s.s # ']' DO
					term := term + s.s;
					Skip;
				END;
				term := term + ')';
			END;
		END ExParamStr;

		PROCEDURE ExPort;
		BEGIN
			term := term + 'Port';
			LookAhead2;
			IF s.nextCh = '(' THEN
				Skip;
				term := term + '[';
				Skip;
				WHILE (s.s # ')') & (s.s # ',') DO
					term := term + s.s;
					Skip;
				END;
				term := term + ']';
				IF s.s = ',' THEN
					term := term + ' := ';
					klammer := klammer + 1;
					delklamm := TRUE;
				END;
			END;
		END ExPort;

		PROCEDURE DoRandomize;
		BEGIN
			Aufgabe(s.s); Aufgabe('; (* ');
			Skip;
			WHILE s.s # ';' DO
				Aufgabe(s.s);
				Skip;
			END;
			Aufgabe(' *)');
		END DoRandomize;

		PROCEDURE ExRandom;
		BEGIN
			term := term + s.s;
			LookAhead2;
			IF s.nextCh = '(' THEN
				Skip;
				term := term + s.s;
				LookAhead2;
				IF s.nextCh = ')' THEN
					Skip;
					term := term + '$7FFF' + s.s;
				ELSE
					klammer := klammer + 1;
				END;
			END;
		END ExRandom;

		PROCEDURE ExAssign;
			VAR token: ARRAY 256 OF CHAR;
		BEGIN
			token := s.s$;
			IF (s.s = '+') OR (s.s = '-') OR
			(s.s = '*') OR (s.s = '/') OR
			(s.s = (*/*) ' DIV ') OR (s.s = (*%*) ' MOD ') OR
			(s.s = (*&*) ' & ') OR (s.s = (*|*) ' OR ') OR
			(s.s = (*^*) ' XOR ') OR (s.s = (*!*) ' ~ ') OR
			(s.s = (*<<*)' SHL ') OR (s.s = (*>>*)' SHR ') THEN
				s.Scan; (* '=' holen *)
				term := term + ':= ' + term + token;
			ELSE
				term := term + token;
			END;
		END ExAssign;

		PROCEDURE LastTerm (): C2cpStrings.DynString; (* für z.B.: i++ oder i-- *)
			VAR
				i: INTEGER;
				offen: INTEGER;
				teil: ARRAY 256 OF CHAR;
		BEGIN
			offen := 0;
			teil := term$;
			i := LEN(teil$) - 1;
			WHILE (i >= 0) & ((offen > 0) OR C2cpStrings.IsAlpha(teil[i]) OR C2cpStrings.IsDigit(teil[i])
				OR (teil[i] = '.') OR (teil[i] = '^')
				OR (teil[i] = ')') OR (teil[i] = ']') OR (teil[i] = '_')) DO
				IF (teil[i] = '(') OR (teil[i] = '[') THEN offen := offen - 1 END;
				IF (teil[i] = ')') OR (teil[i] = ']') THEN offen := offen + 1 END;
				i := i - 1;
			END;
			RETURN C2cpStrings.Trim(C2cpStrings.Right(teil, LEN(teil$) - i - 1));
		END LastTerm;

		PROCEDURE NextTerm (): C2cpStrings.DynString; (* für z.B.: ++i oder --i *)
			VAR
				offen: INTEGER;
				teil: ARRAY 256 OF CHAR;
		BEGIN
			offen := 0;
			teil := '';
			REPEAT
				s.Scan;
				Look;
				IF (s.s = '(') OR (s.s = '[') THEN offen := offen + 1 END;
				IF (s.s = ')') OR (s.s = ']') THEN offen := offen - 1 END;
				teil := teil + s.s;
			UNTIL (offen = 0) & ~ C2cpStrings.IsAlpha(s.nextCh)
			 & (s.nextCh # '[') & (s.nextCh # '.')
			 & (s.nextCh # '^') & (s.nextCh # '-');
			RETURN C2cpStrings.Trim(teil);
		END NextTerm;

		PROCEDURE ExDoppelt;
			VAR
				op, opop, teil, plus: ARRAY 256 OF CHAR;
				first: BOOLEAN;
		BEGIN
			op := s.s$;
			opop := '(*' + op + op + '*)';
			s.Scan; (* 2. Plus oder Minus Zeichen holen *)
			IF C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '(') THEN (* ++i oder --i *)
				first := TRUE;
				teil := NextTerm()$;
				term := term + opop + teil;
			ELSE (* i++ oder i-- *)
				first := FALSE;
				teil := LastTerm()$;
				term := term + opop;
			END;
			IF C2cpStrings.Left(teil, 1)$ = '*' THEN teil := C2cpStrings.Right(teil, LEN(teil$) - 1)$ END;
			IF C2cpStrings.Right(teil, 1)$ = '^' THEN teil := C2cpStrings.Left(teil, LEN(teil$) - 1)$ END;
			IF ((zeichen = 'X') OR (zeichen = 'Z')) & (symtype # 'ARRAY OF CHAR') THEN
				IF C2cpStrings.Left(symtype, 11)$ # 'POINTER TO ' THEN
					symtype := C2cpTable.GetType(C2cpTable.Search(gruppe, symtype))$
				END;
				IF first THEN
					plus := opop + teil
				ELSE
					plus := teil + opop
				END;
				plus := plus + ':=PTR(SEG(' + teil + '^),OFS(' + teil + '^)' + op;
				IF symtype = '' THEN
					plus := plus + '1)'
				ELSE
					plus := plus + 'SizeOf(' + C2cpStrings.After(symtype, 'POINTER TO ') + '))'
				END;
			ELSE
				IF op = '-' THEN
					plus := 'DEC'
				ELSE
					plus := 'INC'
				END;
				plus := plus + '(' + teil + ')';
			END;
			LookAhead2;
			IF vorne & ((s.nextCh = ';') OR (s.nextCh = ',') OR (s.nextCh = ')')) THEN
				IF index > 0 THEN
					ergebnis := ergebnis + term;
					merker[index] := merker[index] + plus
				ELSE
					Aufgabe(plus);
				END;
				term := '';
			ELSE
				IF first THEN
					IF index > 0 THEN
						merker[index] := merker[index] + plus + ', ';
					ELSE
						IF C2cpStrings.Left(term, 6)$ = 'UNTIL ' THEN Aufgabe(C2cpStrings.TAB) END;
						Aufgabe(plus); Aufgabe('; ');
					END;
					IF (C2cpStrings.Left(term, 3)$ = 'IF ')
					OR (C2cpStrings.Left(term, 6)$ = 'WHILE ')
					OR (C2cpStrings.Left(term, 6)$ = 'UNTIL ') THEN
						Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
						IF (C2cpStrings.Left(term, 6)$ = 'WHILE ') THEN AddMerker(anw[stack], plus) END;
					END;
				ELSE
					IF index > 0 THEN plusplus := plusplus + ', ' + plus
					ELSE plusplus := plusplus + '; ' + plus END;
				END;
				IF s.class = C2cpFiles.cSpace THEN term := term + s.s END;
			END;
		END ExDoppelt;

		PROCEDURE StartSammeln;
		BEGIN
			vorne := TRUE;
			dummy := FALSE;
			zu := 0;
			term := '';
			plusplus := '';
			IF index = 0 THEN relation := '' END;
		END StartSammeln;

		PROCEDURE EndSammeln;
			VAR vari, expr: ARRAY 512 OF CHAR; reverse: BOOLEAN;
		BEGIN
			IF index = 2 THEN
				merker[4] := merker[4] + merker[2];
				merker[2] := '';
			END;
			ma := 0;
			mi := 0;
			mk := 0;
			parklam := 0;
			int := TRUE;
			isfile := FALSE;
			ischar := FALSE;
			term := C2cpStrings.Trim(term)$;
			expr := term$;
			reverse := FALSE;
			WHILE C2cpStrings.Pos(':=', expr) # - 1 DO
				vari := C2cpStrings.Parse(expr, ':=')$;
				IF C2cpStrings.Pos(',', term) # - 1 THEN reverse := TRUE END;
				IF C2cpStrings.Pos('(', vari) # - 1 THEN reverse := TRUE END;
			END;
			IF define OR return OR reverse THEN
				WHILE C2cpStrings.Pos(':=', term) # - 1 DO (* ... ':=' rückwärts auflösen *)
					expr := term$;
					WHILE C2cpStrings.Pos(':=', expr) # - 1 DO vari := C2cpStrings.Parse(expr, ':=')$ END;
					term := C2cpStrings.Before(term, ':=' + expr)$;
					WHILE (C2cpStrings.Pos(':=', term) # - 1) & (C2cpStrings.Pos('(', vari) # - 1) DO
						vari := C2cpStrings.After(vari, '(')$;
					END;
					Aufgabe(C2cpStrings.Trim(vari + ':=' + expr));
					IF C2cpStrings.Pos(':=', term) = - 1 THEN term := '' ELSE Aufgabe('; ') END;
				END;
			ELSE
				WHILE C2cpStrings.Pos(':=', term) # - 1 DO (* ... ':='  vorwärts auflösen *)
					Aufgabe(C2cpStrings.Parse(term, ':=') + ':=');
					IF C2cpStrings.Pos(':=', term) # - 1 THEN Aufgabe(expr); Aufgabe('; ') END;
				END;
			END;
			Aufgabe(term);
			IF plusplus # '' THEN
				IF index > 0 THEN merker[index] := merker[index] + plusplus
				ELSE Aufgabe(plusplus) END;
			END;
			IF s.s = ',' THEN Aufgabe(', ')
			ELSIF s.s = ':' THEN Aufgabe(': ')
			ELSIF s.s = ';' THEN Aufgabe('; ')
			ELSIF s.s = 'else' THEN Aufgabe('; ')
			(*
			ELSIF s.s = ')' THEN (* kein Zeichen *)
			ELSIF s.class = cEOL THEN (* kein Zeichen *)
			*)
			END;
			IF s.s = ';' THEN
				IF index > 0 THEN index := index + 1 END;
				IF return THEN
					Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)); Aufgabe('RETURN;');
					return := FALSE;
				END;
				IF define THEN
					LookAhead;
					IF s.class = C2cpFiles.cEOL THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
				END;
			END;
			IF index > 0 THEN
				LookAhead2;
				Aufgabe(' ');
			END;
			zustand := 38;
		END EndSammeln;

		PROCEDURE EndDefine;
		BEGIN
			EndSammeln;
			IF (s.s = ')') & ~ delklamm THEN Aufgabe(s.s) END;
			Aufgabe(';');
			IF tiefe > 0 THEN
				DoEnd;
				IF s.class = C2cpFiles.cSpace THEN Aufgabe(s.s) END;
			END;
			zeiger := FALSE;
			delklamm := FALSE;
			define := FALSE;
			gruppe := alt$;
			zustand := 2;
			IF tiefe > 0 THEN zustand := 1 END; (* Init Variable weil fehlerhafter Tiefe *)
		END EndDefine;

		PROCEDURE ExDoppelpunkt;
		BEGIN
			IF art[stack] = '?' THEN (* xxx ? xxx : xxx *)
				Aufgabe(C2cpStrings.Trim(term)); Aufgabe(';'); Aufgabe(C2cpStrings.CR); Aufgabe(Rand( - 1));
				Aufgabe('ELSE'); Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
				term := C2cpStrings.Before(term, ':=') + ':=';
			ELSIF vorne THEN (* Label xxx: *)
				C2cpTable.Insert(gruppe, name, '', '', '', 'L', s.line);
				EndSammeln;
			ELSE (* : *)
				term := term + s.s;
			END;
		END ExDoppelpunkt;

		PROCEDURE TermAufteilen;
			VAR i: INTEGER; teil: ARRAY 256 OF CHAR; termleft, termright: C2cpStrings.DynString;
		BEGIN
			IF (ma > 0) & (ma < mi) THEN
				teil := C2cpStrings.Substr(term, ma - 1, LEN(term$) - ma + 1)$;
				IF teil[0] = '(' THEN (* '(' am Anfang *)
					teil := C2cpStrings.After(teil, '(')$;
					FOR i := 1 TO mk - 1 DO teil := C2cpStrings.After(teil, '(')$ END;
					IF (C2cpStrings.Left(teil, 1)$ = '(') & (C2cpStrings.Right(teil, 1)$ = ')') THEN
						teil := C2cpStrings.Substr(teil, 1, LEN(teil$) - 2)$;
					END;
					termleft := C2cpStrings.Substr(term, 0, ma - 1); (*'F'*)
					termright := C2cpStrings.Substr(term, ma, mi - ma - 1); (*'F'*)
					term := termleft + termright; (*'F'*)
					IF C2cpStrings.Pos(':=', teil) = - 1 THEN
						termleft := C2cpStrings.Substr(term, 0, ma - 1); (*'P'*)
						termright := C2cpStrings.After(term, '('); (*'P'*)
						term := termleft + termright; (*'P'*)
					END;
				ELSIF C2cpStrings.Pos('(', teil) = - 1 THEN (* keine '(' und nur ':=' *)
					term := C2cpStrings.Substr(term, 0, mi - 1)$;
				ELSIF C2cpStrings.Pos(':=', teil) = - 1 THEN (* kein ':=' *)
					termleft := C2cpStrings.Substr(term, 0, ma - 1);
					termright := C2cpStrings.After(C2cpStrings.Substr(term, ma, mi - ma - 1), '(');
					term := termleft + termright;
				ELSIF C2cpStrings.Pos(':=', teil) < C2cpStrings.Pos('(', teil) THEN (* ':=' vor '(' *)
					term := C2cpStrings.Substr(term, 0, mi - 1)$;
				ELSE (* '(' vor ':=' *)
					IF C2cpStrings.Pos('(', teil) # C2cpStrings.Pos('(*', teil) THEN (* if not '( *' *)
						teil := C2cpStrings.After(teil, '(')$;
					END;
					term := C2cpStrings.Trim(C2cpStrings.Substr(term, 0, mi - 1)) + s.s;
				END;
				IF C2cpStrings.Left(term, 6)$ = 'UNTIL ' THEN Aufgabe(C2cpStrings.TAB) END;
				Aufgabe(C2cpStrings.Trim(teil));
				IF C2cpStrings.Left(term, 6)$ = 'WHILE ' THEN AddMerker(anw[stack], teil) END;
				IF plusplus # '' THEN
					plusplus := C2cpStrings.Trim(C2cpStrings.After(plusplus, ';'))$;
					Aufgabe(';'); Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
					IF C2cpStrings.Left(term, 6)$ = 'UNTIL ' THEN Aufgabe(C2cpStrings.TAB) END;
					Aufgabe(plusplus);
					IF C2cpStrings.Left(term, 6)$ = 'WHILE ' THEN AddMerker(anw[stack], plusplus) END;
					plusplus := '';
				END;
				IF index = 0 THEN Aufgabe(';' + C2cpStrings.CR); Aufgabe(Rand(0)) ELSE Aufgabe(', ') END;
				mi := 0;
			END
		END TermAufteilen;

		PROCEDURE ExKlammer;
			VAR info: ARRAY 256 OF CHAR;
		BEGIN
			IF (klammer = 0) & (term # '') THEN
				vorne := FALSE;
			END;
			IF (s.s = '(') & (s.nextCh = ')') THEN (* () überlesen *)
				Skip;
			ELSE
				IF s.s = '(' THEN klammer := klammer + 1 END;
				IF s.s = ')' THEN klammer := klammer - 1 END;
				IF klammer < parklam THEN parklam := 0 END;
				IF klammer < 0 THEN klammer := 0 END;
				IF (klammer <= mk) & (ma > 0) & (ma < mi) THEN
					TermAufteilen;
				ELSIF (klammer = 0) & (return OR (follow # '')) THEN
					EndSammeln; (* Klammer Ende *)
					IF (art[stack] = '?') & (tiefe = wert[stack] + 1) THEN
						Pop; (* ((a==0) ? b : c) *)
						tiefe := tiefe - 1;
						Aufgabe(';' + C2cpStrings.CR); Aufgabe(Rand(0)); Aufgabe('END');
					END;
					Aufgabe(follow);
					addklamm:= FALSE;
					delklamm := FALSE;
					IF (C2cpStrings.Pos('DO', follow) # - 1) OR (C2cpStrings.Pos('THEN', follow) # - 1) THEN
						IF index > 0 THEN ForAnweisung END;
						tiefe := tiefe + 1;
						zustand := 39;
					ELSE
						zustand := 31;
					END;
					term := '';
					plusplus := '';
					follow := '';
				ELSIF (klammer = 0) & define & (s.nextCh # ';') THEN
					EndDefine;
				ELSE
					IF (alloc # '') & (s.s = ')') & (klammer <= grenze + mk) THEN
						IF alloc = 'FreeMem' THEN
							alloc := C2cpStrings.Trim(C2cpStrings.After(term, '('))$;
							IF C2cpTable.Search(gruppe, alloc) = 0 THEN
								C2cpTable.Insert(gruppe, alloc, '', 'POINTER', '???', 'X', s.line);
							END;
							term := C2cpStrings.Trim(term) + ', ' + C2cpTable.GetInfo(C2cpTable.Search(gruppe, alloc));
						ELSE
							info := C2cpStrings.Trim(C2cpStrings.After(term, ','))$;
							IF C2cpTable.Search(gruppe, alloc) = 0 THEN
								C2cpTable.Insert(gruppe, alloc, '', 'POINTER', info, 'X', s.line);
							ELSE
								IF C2cpTable.GetType(C2cpTable.Search(gruppe, alloc))$ = 'ARRAY OF CHAR' THEN
									vartype := 'POINTER';
									zeichen := 'X';
								ELSE (* nicht ändern *)
									vartype := '';
									zeichen := ' ';
								END;
								C2cpTable.Update(gruppe, alloc, '', vartype, info, zeichen, 0);
							END;
						END;
						alloc := '';
					END;
					IF (s.s = '(') & (zustand = 35) & (zeichen # 'F') & (zeichen # 'P') THEN
						zustand := 36 (* Test auf Type Cast ausführen *)
					ELSE (* kein Test auf Type Cast *)
						zeichen := ' ';
						IF s.s = ')' THEN
							IF delklamm & ((klammer = 0) OR (zustand = 34)) THEN
								delklamm := FALSE
							ELSE
								term := C2cpStrings.Trim(term) + s.s
							END;
						ELSE
							term := term + s.s;
							IF s.s = '(' THEN
								IF ma = 0 THEN ma := LEN(term$) END;
								LookAhead;
							END;
						END;
					END;
				END;
			END;
		END ExKlammer;

		PROCEDURE ConvertToFunction;
		BEGIN
			IF (s.class = C2cpFiles.cName) & (zeichen = 'F') & (term = '') & (klammer = 0) & (zustand = 35) THEN
				term := 'dummy := ';
				C2cpTable.Insert(gruppe, 'dummy', '', symtype, '', 'V', s.line);
				dummy := TRUE;
			END;
		END ConvertToFunction;

		PROCEDURE ConvertToProcedure;
			VAR termleft, termright: C2cpStrings.DynString;
		BEGIN
			IF (s.class = C2cpFiles.cName) & (zeichen = 'P')
			& (C2cpStrings.Pos(':=', term) # - 1) & (C2cpStrings.Trim(C2cpStrings.After(term, ':='))$ = '') THEN
				term := C2cpStrings.Trim(C2cpStrings.Before(term, ':='))$;
				IF ma > 0 THEN
					IF C2cpStrings.Left(term, 9)$ = 'UNTIL ~' THEN
						termleft := C2cpStrings.Before(term, '(');
						termright := C2cpStrings.After(term, '(');
						term := termleft + '(' + s.s + ' ' + termright;
					ELSE
						termleft := C2cpStrings.Before(term, '(');
						termright := C2cpStrings.After(term, '(');
						term := termleft + '(' + s.s + ' (' + termright;
					END;
					mi := LEN(term$) + 1;
				ELSE
					term := s.s + ' (' + term;
					mi := 0;
				END;
				LookAhead2;
				IF s.nextCh = '(' THEN
					Skip;
					LookAhead2;
					IF s.nextCh = ')' THEN
						Skip;
					ELSE
						s.s := ',';
						klammer := klammer + 1;
					END;
				ELSE
					s.s := ')';
				END;
			END;
		END ConvertToProcedure;

		PROCEDURE StartAsm;
		BEGIN
			Aufgabe('ASM'); Aufgabe(C2cpStrings.CR); Aufgabe(Rand(1));
			LookAhead2;
			zustand := 29;
		END StartAsm;

		PROCEDURE (*29*) DoAsm;
		BEGIN
			IF s.class = C2cpFiles.cEOL THEN (* Ende der ASM Anweisung *)
				Aufgabe(s.s);
				zustand := 30;
			ELSE
				IF s.nextCh = '/' THEN
					Aufgabe(s.s);
					s.Scan;
					IF s.nextCh = '*' THEN (* Kommentar gefunden *)
						s.Scan;
						Aufgabe('(* /*');
						zustand := 3;
						zurueck := 29;
					ELSIF s.nextCh = '/' THEN (* Kommentar gefunden *)
						s.Scan;
						Aufgabe('(* //');
						zustand := 4;
						zurueck := 29;
					ELSE
						Look;
						Aufgabe(s.s);
					END;
				ELSE
					Aufgabe(s.s);
				END;
			END;
		END DoAsm;

		PROCEDURE (*30*) EndAsm;
		BEGIN
			IF s.s = 'asm' THEN
				LookAhead2;
				Aufgabe(C2cpStrings.TAB);
				zustand := 29;
			ELSE
				IF s.s = '}' THEN
					Aufgabe(C2cpStrings.TAB); Aufgabe('END;'); Aufgabe(C2cpStrings.CR); Aufgabe(Rand( - 1))
				ELSE
					Aufgabe('END;'); Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0))
				END;
				StartAnweisung;
			END;
		END EndAsm;

		PROCEDURE (*31*) StartAnweisung;
			VAR i: INTEGER;
		BEGIN
			IF vbegin THEN
				i := C2cpTable.Search(gruppe, 'dummy');
				IF i > 0 THEN
					alias := C2cpTable.GetAlias(i)$;
					Aufgabe('VAR '); Aufgabe(alias); Aufgabe(': '); Aufgabe(C2cpTable.GetType(i)); Aufgabe(';');
					Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
				END;
				vbegin := FALSE;
				anfang := FALSE;
				tiefe := tiefe + 1;
				Aufgabe('BEGIN');
				MerkerAusgeben(merker[1]);
				Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
				anfang := TRUE;
			END;
			StartSammeln;
			IF index = 2 THEN (* 2. Anweisung in FOR *)
				zustand := 33;
				StartCondition;
			ELSE (* normale Anweisung *)
				zustand := 32;
				Anweisung;
			END;
		END StartAnweisung;

		PROCEDURE (*32*) Anweisung;
		BEGIN
			IF s.s = '{' THEN DoBegin
			ELSIF s.s = '}' THEN DoEnd
			ELSIF s.s = 'if' THEN DoIf
			ELSIF s.s = 'else' THEN DoElse
			ELSIF s.s = 'for' THEN DoFor
			ELSIF s.s = 'while' THEN DoWhile
			ELSIF s.s = 'do' THEN DoRepeat
			ELSIF s.s = 'switch' THEN DoSwitch
			ELSIF s.s = 'case' THEN DoCase
			ELSIF s.s = 'break' THEN DoBreak
			ELSIF s.s = 'default' THEN DoCase
			ELSIF s.s = 'return' THEN DoReturn
			ELSIF s.s = 'goto' THEN DoGoto
			ELSIF s.s = 'scanf' THEN DoInOut(40, 41, 'READ')
			ELSIF s.s = 'fscanf' THEN DoInOut(40, 41, 'READ')
			ELSIF s.s = 'printf' THEN DoInOut(43, 44, 'StdLog.')
			ELSIF s.s = 'fprintf' THEN DoInOut(43, 44, 'StdLog.')
			ELSIF s.s = 'sprintf' THEN zustand := 46
			ELSIF s.s = 'cin' THEN zustand := 47
			ELSIF s.s = 'cout' THEN zustand := 48
			ELSIF s.s = 'exit' THEN DoHalt
			ELSIF s.s = 'strcat' THEN DoString
			ELSIF s.s = 'strncat' THEN DoString
			ELSIF s.s = 'strcpy' THEN DoString
			ELSIF s.s = 'strncpy' THEN DoStringCopy
			ELSIF s.s = 'Intr' THEN DoIntr
			ELSIF s.s = 'MsDos' THEN DoIntr
			ELSIF s.s = 'Randomize' THEN DoRandomize
			ELSIF s.s = 'randomize' THEN Aufgabe('Randomize')
			ELSIF s.s = 'asm' THEN StartAsm
			ELSIF s.s = '#' THEN zustand := 14
			ELSIF s.class = C2cpFiles.cSpace THEN Aufgabe(s.s)
			ELSIF define & (s.class = C2cpFiles.cEOL) THEN EndDefine
			ELSIF ~ define & (s.class = C2cpFiles.cEOL) THEN Aufgabe(s.s)
			ELSE
				zustand := 35;
				Expression;
			END;
		END Anweisung;

		PROCEDURE (*33*) StartCondition;
		BEGIN
			IF s.class # C2cpFiles.cSpace THEN
				IF (index = 0) & (s.s = '(') THEN
					IF (C2cpStrings.Pos('THEN', follow) = - 1) (* wenn nicht THEN *)
						 & (C2cpStrings.Pos('DO', follow) = - 1) (* und nicht DO *)
						 & (follow # ')') THEN (* und nicht UNTIL *)
						term := term + s.s;
					END;
					klammer := klammer + 1;
					Skip;
				ELSE
					ma := 1;
				END;
				parklam := 0;
				grenze := 1;
				vorne := FALSE; (* Beim Vergleich kann ++ & -- nicht allein sein *)
				addklamm := FALSE;
				delklamm := FALSE;
				relation := '';
				zustand := 34;
				Condition;
			END;
		END StartCondition;

		PROCEDURE (*34*) Condition;
			VAR termleft, termright: C2cpStrings.DynString;
		BEGIN
			IF s.class # C2cpFiles.cSpace THEN
				IF s.s = '(' THEN
					ExKlammer;
				ELSIF (s.s = ')') OR (s.s = ';') OR (s.s = ',') & (klammer <= grenze) THEN
					IF klammer <= grenze THEN
						TermAufteilen;
						IF ~ bitwise & (relation = '') THEN
							IF term # '' THEN term := term + ' # 0' END;
							relation := '#';
						END;
						IF addklamm THEN
							term := term + ')';
							addklamm := FALSE;
						END;
						IF bitwise & (relation = '') THEN
							IF term # '' THEN term := term + ' # 0' END;
							relation := '#';
						END;
					END;
					IF s.s = ')' THEN ExKlammer
					ELSIF s.s = ';' THEN EndSammeln
					ELSE
						term := term + ', ';
						ma := LEN(term$) + 1;
						relation := '';
					END;
				ELSIF s.class = C2cpFiles.cEOL THEN
					Aufgabe(C2cpStrings.Trim(term)); Aufgabe(s.s);
					term := '';
					ma := 0;
				ELSIF s.s = '~' THEN
					term := term + s.s;
					IF ~ bitwise THEN
						LookAhead2;
						IF s.nextCh = '(' THEN
							s.Scan;
							ExKlammer;
						ELSE
							term := term + '(';
							addklamm := TRUE;
						END;
						grenze := klammer;
					END;
					ma := LEN(term$) + 1;
				ELSIF (s.s = ' & ') OR (s.s = ' OR ') OR (s.s = ' XOR ') THEN
					IF ~ addklamm & (ma > 0) & (term # '') & (term[ma - 1] # '(') THEN
						(* term := C2cpStrings.Left(term, ma - 1) + '(' + C2cpStrings.Right(term, LEN(term$) - ma + 1); *)
						termleft := C2cpStrings.Left(term, ma - 1);
						termright := C2cpStrings.Right(term, LEN(term$) - ma + 1);
						term := termleft + '(' + termright;
						addklamm := TRUE;
					END;
					IF bitwise THEN
						term := term + s.s;
					ELSE
						IF relation = '' THEN term := term + ' # 0' END;
						IF addklamm THEN
							term := term + ')';
							addklamm := FALSE;
						END;
						IF (term # '') & (term[LEN(term$) - 1] # ')') THEN
							term := term + ')';
							delklamm := TRUE;
						END;
						term := term + s.s;
						LookAhead2;
						IF mi = 0 THEN ma := LEN(term$) + 1 END;
						IF (s.nextCh # '(') & (s.nextCh # '!' (*'NOT'*)) THEN
							term := term + '(';
							addklamm := TRUE;
						END;
					END;
					relation := '';
				ELSE
					Expression;
				END;
			END;
		END Condition;

		PROCEDURE (*35*) Expression;
		BEGIN
			IF s.class = C2cpFiles.cName THEN
				ConvertToFunction;
				IF symtype = 'BOOLEAN' THEN relation := '#' END;
				IF s.s = 'sizeof' THEN
					C2cpTable.Insert('', s.s, 'SizeOf', 'INTEGER', '', 'F', s.line);
				END;
			ELSE
				IF s.s = '[' THEN klammer := klammer + 1 END;
				IF s.s = ']' THEN klammer := klammer - 1 END;
				IF klammer < 0 THEN klammer := 0 END;
			END;
			IF s.s = '(' THEN ExKlammer
			ELSIF s.s = ')' THEN ExKlammer
			ELSIF s.s = 'else' THEN EndSammeln; EndAnweisung;
			ELSIF s.s = '}' THEN EndSammeln; DoEnd;
			ELSIF s.s = ';' THEN EndSammeln
			ELSIF s.s = '?' THEN ExFragezeichen
			ELSIF s.s = ':' THEN ExDoppelpunkt
			ELSIF s.s = 'malloc' THEN ExNew('')
			ELSIF s.s = 'calloc' THEN ExNew('(*0*)')
			ELSIF s.s = 'realloc' THEN ExNew('(*r*)')
			ELSIF s.s = 'free' THEN ExFree
			ELSIF s.s = 'struct' THEN term := term + '{' + s.s + '}'
			ELSIF s.s = 'bioskey' THEN ExKey
			ELSIF s.s = 'ParamStr' THEN ExParamStr
			ELSIF s.s = 'inportb' THEN ExPort
			ELSIF s.s = 'outportb' THEN ExPort
			ELSIF s.s = 'Random' THEN ExRandom
			ELSIF define & (s.class = C2cpFiles.cEOL) THEN EndDefine
			ELSIF ~ define & (s.class = C2cpFiles.cEOL) THEN s.class := C2cpFiles.cSpace
			ELSIF s.nextCh = '=' THEN ExAssign
			ELSIF (s.s = '+') & (s.nextCh = '+') THEN ExDoppelt
			ELSIF (s.s = '-') & (s.nextCh = '-') THEN ExDoppelt
			ELSIF (s.s = ',') & (index # 0) THEN EndSammeln
			ELSIF (s.s = '<') OR (s.s = '<=') OR (s.s = '=')
				OR (s.s = '>') OR (s.s = '>=') OR (s.s = '#') THEN
				term := term + ' ' + s.s + ' ';
				relation := s.s$;
			ELSE
				IF (s.class = C2cpFiles.cName) THEN
					IF zeichen = 'U' THEN
						IF C2cpStrings.Right(s.s, 1)$ = '^' THEN s.s := C2cpStrings.Left(s.s, LEN(s.s$) - 1)$ END;
					END;
					IF zeichen = 'X' THEN (* nicht bei 'Z' *)
						IF (C2cpStrings.Right(s.s, 1)$ # '^') & (parklam > 0) THEN s.s := s.s + '^' END;
					END;
					IF C2cpTable.Reserviert(s.s) THEN
						s.s := C2cpStrings.ToUpper(s.s) + ' ';
						IF C2cpStrings.Right(term, 1)$ # '(' THEN s.s := ' ' + s.s END;
					END;
					IF (s.s = 'NULL') OR (s.s = 'cNULL') THEN
						IF isfile THEN s.s := 'NULL'
						ELSIF ischar THEN s.s := 'cNULL'
						ELSE s.s := 'NIL'
						END;
					END;
					ConvertToProcedure;
				ELSE
					IF dummy & (klammer = 0) THEN
						Aufgabe(term); Aufgabe(';' + C2cpStrings.CR); Aufgabe(Rand(0));
						term := 'dummy';
						dummy := FALSE;
					END;
				END;
				IF (s.s = '.') OR (s.s = '^.') THEN
					IF ((zeichen = 'U') OR (zeichen = 'V')) OR ((zeichen = 'X') & (C2cpStrings.Right(term, 1)$ = '^')) THEN
						s.s := '.'
					END;
					term := term + s.s;
					alt := gruppe$;
					name := symtype$; (* Recordtype als Gruppenname setzen und ... *)
					IF C2cpStrings.Left(name, 1)$ = '^' THEN name := C2cpStrings.After(name, '^')$ END;
					gruppe := C2cpTable.GetInfo(C2cpTable.Search(gruppe, name))$;
					Skip; (* nächstes Wort über Recordnamen umwandeln. *)
					gruppe := alt$;
				END;
				IF (s.s = ':=') & (ma > 0) & (mi = 0) THEN
					mi := LEN(term$) + 1;
					mk := klammer - 1;
					term := term + ' ' + s.s + ' ';
				ELSE
					term := term + s.s;
				END;
				IF (s.nextCh = '.') & (symtype = 'Registers') THEN
					alt := gruppe$;
					gruppe := symtype$;
					Skip;
					term := term + s.s;
					Skip;
					IF ((s.s = 'x') OR (s.s = 'h')) & (s.nextCh = '.') THEN
						Skip;
						Skip;
					END;
					term := term + s.s;
					gruppe := alt$;
				END;
				IF (s.s = ':=') OR (s.s = ',') THEN
					vorne := FALSE;
					IF define & (klammer = 0) THEN
						LookAhead;
						IF s.class = C2cpFiles.cSpace THEN term := term + s.s END;
						IF s.nextCh = '(' THEN
							Skip;
							klammer := 1;
							delklamm := TRUE;
						END;
					END;
				ELSE
					IF (klammer < zu) & ~ C2cpStrings.IsAlpha(s.nextCh) & (s.nextCh # '.')
						THEN
						term := term + ')';
						zu := 0;
					END;
				END;
				IF s.class = C2cpFiles.cName THEN
					IF zeichen = 'P' THEN term := term + ' ' END;
					IF zeichen = 'F' THEN LookAhead END;
				END;
			END;
		END Expression;

		PROCEDURE (*36*) TypeCast;
			VAR wort: ARRAY 256 OF CHAR;
		BEGIN
			wort := '';
			zustand := 35;
			IF s.class = C2cpFiles.cSpace THEN
				Skip;
			END;
			IF s.s = 'unsigned' THEN Skip END;
			IF (s.s = 'char') OR (s.s = 'int') OR (s.s = 'double') OR ((s.class = C2cpFiles.cName) & (zeichen = 'T')) THEN
				klammer := klammer - 1;
				wort := s.s$;
				Skip;
				IF s.s = 'far' THEN
					Skip;
				END;
				IF (wort = 'char') & (s.s = '*') THEN
					wort := 'ARRAY OF CHAR';
					Skip;
				END;
				IF (s.s = ')') & (s.nextCh # ',') & (s.nextCh # ')') THEN
					(* Type Cast Umwandlung *)
					IF alloc # '' THEN
						LookAhead2;
					ELSE
						IF wort = 'char' THEN wort := 'CHR'
						ELSIF wort = 'int' THEN wort := 'ENTIER'
						ELSIF wort = 'double' THEN wort := 'LONG'
						ELSIF parklam > 0 THEN wort := '{' + wort + '}'
						END;
						term := term + wort;
						LookAhead2;
						IF s.nextCh # '(' THEN
							term := term + '(';
							zu := klammer + 1;
						END;
					END;
				ELSE (* keine Type Cast Umwandlung *)
					term := term + '{(' + wort + s.s;
					WHILE s.s # ')' DO
						Skip;
						term := term + s.s
					END;
					term := term + '}';
					(*
					klammer := klammer + 1;
					offen := offen + 1;
					term := term + '(' + wort;
					Expression;
					*)
				END;
			ELSE (* keine Type Cast Umwandlung *)
				IF ma = 0 THEN
					ma := LEN(term$) + 1;
				END;
				term := term + '(' + wort;
				Expression;
			END;
		END TypeCast;

		PROCEDURE (*37*) ElseAnweisung;
		BEGIN
			IF s.s = 'else' THEN
				Skip;
				IF s.class = C2cpFiles.cEOL THEN Skip END;
				IF s.s = 'if' THEN
					anfang := TRUE;
					StartSammeln;
					DoIf;
					term := 'ELSIF ';
					ma := 7;
				ELSE
					Push('I', tiefe, TRUE);
					tiefe := tiefe + 1;
					Aufgabe('ELSE');
					NeueAnwZeile;
				END;
			ELSE
				Aufgabe(';'); Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
				StartAnweisung;
			END;
		END ElseAnweisung;

		PROCEDURE (*38*) EndAnweisung;
		BEGIN
			IF s.class = C2cpFiles.cSpace THEN
				Aufgabe(s.s);
			ELSE
				IF ende[stack] & (tiefe = wert[stack] + 1) THEN
					IF s.class = C2cpFiles.cEOL THEN LookAhead END;
					IF s.class = C2cpFiles.cSpace THEN Skip END;
					WHILE ende[stack] & (tiefe = wert[stack] + 1) DO
						IF anw[stack] # '' THEN (* END FOR *)
							MerkerAusgeben(anw[stack]);
						END;
						Pop;
						tiefe := tiefe - 1;
						Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0));
						IF s.s # 'else' THEN Aufgabe('END;') END;
					END;
					IF s.s = 'else' THEN
						ElseAnweisung
					ELSE
						IF s.class # C2cpFiles.cEOL THEN
							anfang := TRUE;
							Aufgabe(C2cpStrings.CR);
							IF s.s = '}' THEN Aufgabe(Rand( - 1))
							ELSE Aufgabe(Rand(0)) END;
						END;
						StartAnweisung;
					END;
				ELSE
					StartAnweisung
				END;
			END;
		END EndAnweisung;

		PROCEDURE (*39*) NeueAnwZeile;
		BEGIN
			IF s.class = C2cpFiles.cSpace THEN
				IF (s.nextCh # C2cpStrings.CR) & (s.nextCh # '{') THEN Aufgabe(s.s) END;
				zustand := 39;
			ELSIF define & (s.class = C2cpFiles.cEOL) THEN
				EndDefine;
			ELSE
				IF s.class = C2cpFiles.cEOL THEN Skip END;
				IF s.s = '{' THEN
					anfang := FALSE;
					DoBegin;
				ELSE
					Aufgabe(C2cpStrings.CR);
					IF s.s = '}' THEN Aufgabe(Rand( - 1)) ELSE Aufgabe(Rand(0)) END;
					anfang := TRUE;
					zustand := 31;
					IF s.class # C2cpFiles.cSpace THEN StartAnweisung END;
				END;
			END;
		END NeueAnwZeile;

		PROCEDURE EndInOut;
		BEGIN
			IF ln THEN Aufgabe('Ln'); ln := FALSE; END;
			zustand := 35; (* Ende von scanf oder printf *)
			IF define & (s.nextCh # ';') THEN
				s.s := ' ';
				term := '';
				EndDefine;
			END;
		END EndInOut;

		PROCEDURE (*40*) DoScanf;
		BEGIN
			zustand := 35;
			Expression;
		END DoScanf;

		PROCEDURE (*41*) DoScanf1;
		BEGIN
			(*parklam := klammer + 1;*) (* wenn man (*@*) in der Ausgabe sehen möchte *)
			parklam := 0; (* ... nicht sehen möchte *)
			IF s.class = C2cpFiles.cSpace THEN Skip END;
			Aufgabe(' '); Aufgabe(s.s);
			zustand := 42;
		END DoScanf1;

		PROCEDURE (*42*) DoScanf2;
		BEGIN
			IF s.class = C2cpFiles.cString THEN
				Aufgabe('(*'); Aufgabe(s.s); Aufgabe('*)');
				IF s.nextCh = ',' THEN Skip END; (* , nach "..." unterdrücken *)
			ELSE
				IF C2cpStrings.Left(s.s, 1)$ = '@' THEN s.s := C2cpStrings.Right(s.s, LEN(s.s$) - 1)$ END;
				Aufgabe(s.s); (* & bzw. @ vor Variable unterdrücken *)
			END;

			IF s.s = ')' THEN EndInOut END; (* Ende von scanf *)
		END DoScanf2;

		PROCEDURE (*43*) DoPrintf;
		BEGIN
			IF s.s = ',' THEN
				zaehler := zaehler - 1;
			END;
			IF (s.s = ')') & (klammer = 1) THEN
				IF zaehler > 0 THEN
					term := C2cpStrings.Trim(term) + ', dummy';
					C2cpTable.Insert(gruppe, 'dummy', '', 'INTEGER', '', 'V', s.line);
				END;
				zustand := 35; (* Ende von printf *)
			END;
			Expression;
			IF (grenze > 0) & (klammer <= grenze) THEN
				term := term + '^';
				grenze := 0;
			END;
			IF (s.class = C2cpFiles.cName) & (symtype = 'ARRAY OF CHAR') & (zaehler < 1) THEN
				IF (s.nextCh = '(') OR (s.nextCh = '[') THEN 
					grenze := klammer
				ELSE 
					term := term + '^' 
				END;
			END;
		END DoPrintf;

		PROCEDURE (*44*) DoPrintf1;
			VAR
				kanal, merke, teil, rest: ARRAY 256 OF CHAR;
		BEGIN
			kanal := '';
			IF s.class = C2cpFiles.cSpace THEN Skip END;
			IF s.s = '(' THEN Skip END;
			IF s.class = C2cpFiles.cName THEN
				kanal := s.s$;
				Skip;
				IF s.s = ',' THEN Skip END;
			END;
			IF s.class = C2cpFiles.cString THEN
				merke := '';
				IF kanal # '' THEN kanal := '(*' + kanal + '*)' END;
				rest := C2cpStrings.Substr(s.s, 1, LEN(s.s$) - 2)$;
				Skip;
				WHILE rest # '' DO (* Start für alle '\n' *)
					IF C2cpStrings.Pos('\n', rest) # - 1 THEN ln := TRUE END;
					teil := C2cpStrings.Parse(rest, '\n')$; (* Der Teil vor '\n' *)
					IF teil # '' THEN
						merke := '';
						WHILE teil # '' DO (* Start für alle '%' *)
							Aufgabe(merke);
							merke := C2cpStrings.Parse(teil, '%')$; (* Merke vor '%' *)
							IF merke # '' THEN
								Aufgabe('String('); Aufgabe(kanal); Aufgabe("'"); Aufgabe(merke); Aufgabe("'"); Aufgabe(')');
								merke := '; StdLog.';
							END;
							IF (teil # '') & (s.s = ',') THEN
								Aufgabe(merke); Aufgabe('Int('); Aufgabe(kanal);
								(* Variablename einfügen *)
								Skip; IF s.class = C2cpFiles.cEOL THEN Skip END;
								WHILE (s.s # ',') & (s.s # ')') DO
									Expression;
									s.Scan;
									Look;
								END;
								Aufgabe(C2cpStrings.Trim(term));
								term := '';
								teil := C2cpStrings.Right(teil, LEN(teil$) - 1)$; (* %x *)
								Aufgabe(')');
							END;
							merke := '; StdLog.';
						END; (* -Ende für alle '%' *)
						IF ln THEN merke := merke + 'Ln'; ln := FALSE ELSE merke := '' END;
					ELSE
						merke := 'Ln'; ln := FALSE; (* Es war nur \n vorhanden *)
					END;
					IF rest # '' THEN Aufgabe(merke); Aufgabe('; StdLog.') END;
				END; (* -Ende für alle '\n' *)
				IF s.s = ')' THEN
					Aufgabe(merke);
					EndInOut; (* Ende von printf *)
				ELSE (*s.s = ','*)
					Aufgabe('); StdLog.String(');
					zustand := 45;
				END;
			ELSE (* Parameterliste ohne Format-String *)
				Aufgabe('String('); Aufgabe(kanal);
				zustand := 45;
				IF s.s = ')' THEN
					Aufgabe(s.s); EndInOut;
				ELSE
					Aufgabe('); StdLog.String('); Aufgabe(s.s);
				END;
			END;
		END DoPrintf1;

		PROCEDURE (*45*) DoPrintf2;
		BEGIN
			IF s.s = ',' THEN
				Aufgabe('); StdLog.String(');
			ELSE
				Aufgabe(s.s);
			END;
			IF s.s = ')' THEN EndInOut END; (* Ende von printf *)
		END DoPrintf2;

		PROCEDURE (*46*) DoSprint;
			VAR
				merke, rest: ARRAY 256 OF CHAR;
		BEGIN
			merke := '';
			IF s.class = C2cpFiles.cSpace THEN Skip END;
			IF s.s = '(' THEN Skip END;
			WHILE s.s # ',' DO (* Ergebnis Variable holen *)
				Aufgabe(s.s);
				s.Scan;
				Look;
			END;
			Aufgabe(' := ');
			Skip;
			IF s.class = C2cpFiles.cString THEN
				rest := C2cpStrings.Substr(s.s, 1, LEN(s.s$) - 2)$;
				Skip;
				WHILE rest # '' DO (* Start für alle '%' *)
					Aufgabe(merke);
					merke := C2cpStrings.Parse(rest, '%')$; (* Merke vor '%' *)
					IF merke # '' THEN
						Aufgabe("'"); Aufgabe(merke); Aufgabe("'");
						merke := ' + ';
					END;
					IF (rest # '') & (s.s = ',') THEN
						Aufgabe(merke); (* Variablename einfügen *)
						Skip; IF s.class = C2cpFiles.cEOL THEN Skip END;
						WHILE (s.s # ',') & (s.s # ')') DO
							Expression;
							Skip;
						END;
						Aufgabe(term);
						term := '';
						rest := C2cpStrings.Right(rest, LEN(rest$) - 1)$; (* %x *)
					END;
					merke := ' + ';
				END; (* -Ende für alle '%' *)
			END;
			IF s.s # ')' THEN Aufgabe(merke); Aufgabe(s.s) END;
			zustand := 35; (* Ende von sprintf *)
		END DoSprint;

		PROCEDURE (*47*) DoCin;
		BEGIN
			IF s.class = C2cpFiles.cSpace THEN Skip END;
			IF s.s = ' SHR ' (*>>*) THEN Skip ELSE Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
			Aufgabe('READ (');
			WHILE (s.s # ';') & (s.s # ' SHR ') (*>>*) DO
				Aufgabe(s.s);
				Skip;
			END;
			Aufgabe(')');
			IF s.s # ';' THEN Aufgabe(';') ELSE EndSammeln END;
		END DoCin;

		PROCEDURE (*48*) DoCout;
			VAR teil: ARRAY 256 OF CHAR;
		BEGIN
			IF s.class = C2cpFiles.cSpace THEN Skip END;
			IF s.s = ' SHL ' (*<<*) THEN Skip ELSE Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
			IF s.class = C2cpFiles.cString THEN
				s.s := C2cpStrings.Substr(s.s, 1, LEN(s.s$) - 2)$;
				WHILE s.s # '' DO (* für alle '\n' *)
					IF C2cpStrings.Pos('\n', s.s) # - 1 THEN ln := TRUE END;
					teil := C2cpStrings.Parse(s.s, '\n')$; (* der Teil vor '\n' *)
					IF teil # '' THEN
						Aufgabe('StdLog.String('); Aufgabe("'"); Aufgabe(teil); Aufgabe("'"); Aufgabe(')');
					END;
					IF ln THEN
						Aufgabe('StdLog.Ln'); ln := FALSE;
						IF s.s # '' THEN Aufgabe(';') END;
					END;
					IF s.s # '' THEN Aufgabe(C2cpStrings.CR); Aufgabe(Rand(0)) END;
				END;
				Skip;
			ELSE
				Aufgabe('StdLog.String');
				Aufgabe(' (');
				WHILE (s.s # ';') & (s.s # ' SHL ') (*<<*) DO
					Aufgabe(s.s);
					Skip;
				END;
				Aufgabe(')');
			END;
			IF s.s # ';' THEN Aufgabe(';') ELSE EndSammeln END;
		END DoCout;

	BEGIN
		ergebnis := '';
		(* 1. Leerstellen am Ende der Zeile überlesen *)
		(* 2. Wenn nicht im Kommentar, dann auf Kommentar prüfen *)
		(* 3. Wenn nicht im Kommentar, dann Namen zusammenketten & Tauschen *)
		(* 4. Aufgabe über Zustand ermitteln *)
		(* 5. Wenn neue Zeile und nicht in Kommentar dann Linker Rand festlegen *)
		IF (s.class = C2cpFiles.cSpace) & (s.nextCh = C2cpStrings.CR) THEN s.Scan END;
		IF (zustand # 3) & (zustand # 4) THEN
			IF (s.s = '/') & (s.nextCh = '*') THEN
				IF vkomma OR (enum > 0) THEN
					AufgabeVariableType;
					Aufgabe(blank);
				END;
				s.Scan;
				s.s := '(*';
				zurueck := zustand;
				zustand := 3;
			END;
			IF (s.s = '/') & (s.nextCh = '/') THEN
				IF vkomma OR (enum > 0) THEN
					AufgabeVariableType;
					Aufgabe(blank);
				END;
				s.Scan;
				s.s := '(* //';
				zurueck := zustand;
				zustand := 4;
			END;
		END;
		IF (zustand # 3) & (zustand # 4) & (zustand # 16) THEN
			Look;
		END;
		CASE zustand OF
		| 1: StartBlock; (* Init & Block *)
		| 2: Block; (* Block ohne Init *)
		| 3: Kommentar1; (* /* .. */ *)
		| 4: Kommentar2; (* // .. CR *)
		| 5: DoProcedure; (* PROCEDURE or FUNCTION name *)
		| 6: DoProcedure; (* PROCEDURE or FUNCTION name *)
		| 7: ParameterAnfang; (* PROC & FUNC Anfang für .DEF *)
		| 8: ParameterListe; (* PROC & FUNC Liste für .DEF *)
		| 9: ParameterEnde; (* PROC & FUNC Ende für .DEF *)
		| 10: ParameterAnfang; (* PROCEDURE & FUNCTION Anfang *)
		| 11: ParameterListe; (* PROCEDURE & FUNCTION Liste *)
		| 12: ParameterEnde; (* PROCEDURE & FUNCTION Ende *)
		| 13: RunParameter; (* Parameter von 'main' *)
		| 14: Preprocessor; (* # *)
		| 15: IncludeStart; (* #include Start *)
		| 16: CopyUntil(C2cpStrings.CR, ' *)', C2cpStrings.CR); (* #include Ende *)
		| 17: IncludeName; (* #include Name & Ende *)
		| 18: DoAsmBlock; (* #asm .. #endasm *)
		| 19: DoConstant; (* FUNCTION, DEFINE oder CONST ? *)
		| 20: CopyUntil(C2cpStrings.CR, ';', C2cpStrings.CR); (* #define - CONST *)
		| 21: DeclareType; (* TYPE *)
		| 22: DeclareRecord; (* RECORD *)
		| 23: ListVariable; (* TYPE, VAR ODER FUNCTION *)
		| 24: CopyUntil('}', '', ')'); (* enum type *)
		| 25: ArrayVariable; (* VAR *)
		| 26: InitVariable; (* INIT VAR *)
		| 27: NeueVarZeile; (* CR+LF einfügen - Var Def *)
		| 28: NeueBlockZeile; (* CR+LF einfügen - Block Ebene *)
		| 29: DoAsm; (* Assembler Zeile *)
		| 30: EndAsm; (* Weiter Assembler Zeile ? *)
		| 31: StartAnweisung; (* für +=, -=, *=, etc. *)
		| 32: Anweisung; (* IF, FOR, WHILE, etc. *)
		| 33: StartCondition; (* <=, AND, etc. *)
		| 34: Condition; (* <=, AND, etc. *)
		| 35: Expression; (* für ++ & -- *)
		| 36: TypeCast; (* (type) *)
		| 37: ElseAnweisung; (* kein ; vor ELSE *)
		| 38: EndAnweisung; (* END einfügen *)
		| 39: NeueAnwZeile; (* CR+LF einfügen - Anweisung *)
		| 40: DoScanf; (* scanf *)
		| 41: DoScanf1; (* READ 1. Teil *)
		| 42: DoScanf2; (* READ 2. Teil *)
		| 43: DoPrintf; (* printf *)
		| 44: DoPrintf1; (* StdLog. 1. Teil *)
		| 45: DoPrintf2; (* StdLog. 2. Teil *)
		| 46: DoSprint; (* sprintf *)
		| 47: DoCin; (* cin *)
		| 48: DoCout; (* cout *)
		ELSE
			StdLog.String('>>> Ungültige Zustand '); StdLog.Int(zustand);
			StdLog.String(' in Zeile '); StdLog.Int(s.line); StdLog.String(' <<<'); StdLog.Ln;
			StartBlock;
		END;
		IF s.class = C2cpFiles.cEOL THEN
			anfang := TRUE;
			LookAhead;
			IF (tiefe = 0) & (C2cpStrings.Left(follow, 3)$ = '-*)') THEN
				Aufgabe(Rand(0));
				IF s.nextCh # '{' THEN Aufgabe('--- ') END;
			ELSIF (zustand = 3) OR (s.nextCh = '/') THEN
				Aufgabe(Rand(0));
				IF (tiefe = 0) & (s.class = C2cpFiles.cSpace) & ~ vbegin THEN Aufgabe(s.s) END
			ELSIF s.nextCh = '#' THEN Aufgabe(Rand(0)) (* Rand für Preprocessor *)
			ELSIF s.nextCh = '}' THEN Aufgabe(Rand( - 1))
			ELSIF isgoto THEN Aufgabe(Rand( - 1))
			ELSIF (zustand = 8) OR (zustand = 11) THEN Aufgabe(Rand(+8))
			ELSIF (zustand = 21) OR (zustand = 22) THEN (* keinen weiteren Rand *)
			ELSE Aufgabe(Rand(0))
			END;
			isgoto := FALSE;
		ELSE
			IF (ergebnis # '') OR (term # '') THEN anfang := FALSE END;
		END;
	END Biber;

END C2cpCBiber.

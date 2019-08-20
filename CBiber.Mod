(*---------------------------------------------------------------------------
--   C Biber (as busy as a beaver - hard worker)
--   © Helmut Zinn   1994..2016
---------------------------------------------------------------------------*)

MODULE C2cpCBiber;

	

	IMPORT
		C2cpFiles, C2cpStrings, C2cpTable, C2cpTrace, StdLog;

	CONST
		UseNamedInteger = FALSE;

	VAR
		back: INTEGER; (* next state behind comment *)
		deepness: INTEGER; (* number of open BEGIN which are not closed with END *)
		bracket: INTEGER; (* number of open '(' bracket *)
		endbracket: INTEGER; (* bracket deepness during the start of define *)
		closed: INTEGER; (* if bracket < closed then insert bracket *)
		bound: INTEGER; (* if bracket <= bound then insert condition *)
		xarray: INTEGER; (* dimension of array *)
		dim: INTEGER; (* dimension of pointer array *)
		record: INTEGER; (* counter for nested records *)
		union: INTEGER; (* counter for union record *)
		enum: INTEGER; (* counter for enum *)
		lineNumber: INTEGER; (* source line number at procedure or function start *)
		parameterCount: INTEGER; (* parameter counter for 'printf' *)
		ma: INTEGER; (* '(' position inside term *)
		mi: INTEGER; (* ':=' position inside term *)
		mk: INTEGER; (* bracket deepness of ma *)
		classdef: BOOLEAN; (* Is it class definition? *)
		typedef: BOOLEAN; (* TRUE = type definition - FALSE = variable definition *)
		recdef: BOOLEAN; (* Is it record type or record variable definition? *)
		vardef: BOOLEAN; (* Is it variable definition? *)
		asterix: BOOLEAN; (* Is it the address of a variable ? * -> @ *)
		pointer: BOOLEAN; (* Is the variable a pointer? *)
		basetype: BOOLEAN; (* Is it a pointer to basetype? *)
		omitted: BOOLEAN; (* Is the record name missing ? *)
		vcomma: BOOLEAN; (* specify variable with comma *)
		vbegin: BOOLEAN; (* hold BEGIN until VAR definition is done *)
		anfang: BOOLEAN; (* Are we at the beginning of a new line? *)
		vorne: BOOLEAN; (* At the beginning of a statement in front of := *)
		addBracket: BOOLEAN; (* in condition add bracket *)
		delBracket: BOOLEAN; (* in condition delete bracket *)
		bitwise: BOOLEAN; (* Is it bitwise AND, OR, XOR or NOT ? *)
		int: BOOLEAN; (* Is the expression from type INTEGER ? *)
		isfile: BOOLEAN; (* Is the expression from from type FILE ? *)
		ischar: BOOLEAN; (* Is the expression from type ARRAY OF CHAR ? *)
		default: BOOLEAN; (* Is it the default part of select statement ? *)
		isbreak: BOOLEAN; (* Is at the end of case a break statement ? *)
		isgoto: BOOLEAN; (* Is a goto, halt or return statement inside the case ? *)
		dummy: BOOLEAN; (* add dummy variable to the function call *)
		return: BOOLEAN; (* behind the ';' insert a RETURN statement *)
		define: BOOLEAN; (* translate '#define' in FUNCTION or CONST *)
		defineConst: BOOLEAN; (* translate '#define' in CONST *)
		defineFunction: BOOLEAN; (* change translation of '#define' from CONST to FUNCTION *)
		ln: BOOLEAN; (* insert the statement StdLog.Ln; *)
		alt: ARRAY 256 OF CHAR; (* name of the last group *)
		namespace: ARRAY 256 OF CHAR; (* namespace name *)
		class: ARRAY 256 OF CHAR; (* class name *)
		group: ARRAY 256 OF CHAR; (* group  = name of the current translated procedure *)
		name: ARRAY 256 OF CHAR; (* name *)
		alias: ARRAY 256 OF CHAR; (* alias name *)
		symtype: ARRAY 256 OF CHAR; (* type of the variable found in the symbol table *)
		sign: CHAR; (* type of the name *)
		constname: ARRAY 256 OF CHAR; (* constant name *)
		arrayname: ARRAY 256 OF CHAR; (* array name *)
		arraysign: CHAR; (* sign of the array name *)
		recname: ARRAY 256 OF CHAR; (* record name *)
		recpoin: ARRAY 256 OF CHAR; (* record pointer name *)
		alloc: ARRAY 256 OF CHAR; (* pointer name for memory allocation *)
		varname: ARRAY 256 OF CHAR; (* variable name or variable name of assigment *)
		special: ARRAY 256 OF CHAR; (* supplement to the variable type *)
		vartype: ARRAY 256 OF CHAR; (* type of the variable in the source program *)
		partype: ARRAY 256 OF CHAR; (* type of the last variable in parameter list *)
		parvar: CHAR; (* Is it a VAR parameter ? *)
		labelnr: INTEGER; (* Label counter for GOTO statements *)
		labelname: ARRAY 256 OF CHAR; (* the alias name of the label *)
		term: ARRAY 512 OF CHAR; (* using for ++ and -- statements *)
		plusplus: ARRAY 256 OF CHAR; (* behind this statement follows an INC or DEC *)
		follow: ARRAY 256 OF CHAR; (* behind the bracket following keyword *)
		blank: ARRAY 256 OF CHAR; (* spaces in front of comment *)
		relation: ARRAY 256 OF CHAR; (* last relation is <, <=, =, >=, > or # *)
		index: INTEGER; (* index 0..4 to the marker - used by the FOR statement *)
		marker: ARRAY 4 + 1 OF ARRAY 256 OF CHAR;

	CONST
		nstack = 16; (* max stack deepness *)

	VAR
		stack: INTEGER; (* position of top of the stack  0 = stack is empty *)
		art: ARRAY nstack OF CHAR; (* kind of loop ' ' I ? F W R C *) (* V *)
		wert: ARRAY nstack OF INTEGER; (* the deep at the beginning of the  loop *)
		anw: ARRAY nstack OF ARRAY 256 OF CHAR; (* insert this statement in front of the END *) (* group *)
		ende: ARRAY nstack OF BOOLEAN; (* insert END behind ';' *)

	PROCEDURE Biber* (VAR s: C2cpFiles.Scanner; VAR state: INTEGER; VAR outcome: ARRAY OF CHAR);

		(*  current state    Token read           Task       next state     *)
		(*  alter Zustand    gelesenes Zeichen    Aufgabe    neuer Zustand  *)
		(* ---------------------------------------------------------------- *)

		PROCEDURE Output (adding: ARRAY OF CHAR);
		BEGIN
			C2cpTrace.Sub(100);
			outcome := outcome + adding;
			IF index > 0 THEN marker[index] := marker[index] + adding END;
		END Output;

		PROCEDURE Margin (offset: INTEGER): C2cpStrings.String;
			VAR i: INTEGER; r: ARRAY 256 OF CHAR;
		BEGIN (* left margin depending on block deepness *)
			C2cpTrace.Sub(101);
			r := '';
			FOR i := 1 TO deepness + offset + 1 DO r := r + C2cpStrings.tab END;
			RETURN C2cpStrings.NewString(r);
		END Margin;

		PROCEDURE AddMarker (VAR s: ARRAY OF CHAR; a: ARRAY OF CHAR);
		BEGIN
			C2cpTrace.Sub(102);
			IF s # '' THEN s := s + ', ' END;
			s := s + a;
		END AddMarker;

		PROCEDURE OutputMarker (s: ARRAY OF CHAR);
			VAR satz: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(103);
			WHILE s # '' DO
				satz := C2cpStrings.Trim(C2cpStrings.Parse(s, ', ')$)$;
				IF satz # '' THEN
					IF anfang THEN
						Output(Margin( - deepness + 1))
					ELSE
						Output(C2cpStrings.line); Output(Margin(0))
					END;
					Output(satz); Output(';');
					anfang := FALSE;
				END;
			END;
		END OutputMarker;

		PROCEDURE Push (a: CHAR; w: INTEGER; e: BOOLEAN);
		BEGIN
			C2cpTrace.Sub(104);
			IF stack < nstack THEN
				INC(stack)
			ELSE
				StdLog.String(' ***  C2cpCBiber: Stack overflow  *** '); StdLog.Ln;
			END;
			art[stack] := a;
			wert[stack] := w;
			anw[stack] := '';
			ende[stack] := e;
		END Push;

		PROCEDURE Pop;
		BEGIN
			C2cpTrace.Sub(105);
			art[stack] := ' ';
			wert[stack] := - 1;
			anw[stack] := '';
			ende[stack] := FALSE;
			IF stack > 0 THEN DEC(stack) END;
		END Pop;

		PROCEDURE (*2*) ^Block;
		PROCEDURE (*8*) ^ParameterList;
		PROCEDURE (*26*) ^ListVariable;
		PROCEDURE (*35*) ^DoEnd;
		PROCEDURE (*67*) ^EndSammeln;
		PROCEDURE (*75*) ^StartStatement;
		PROCEDURE (*76*) ^Statement;
		PROCEDURE (*77*) ^StartCondition;
		PROCEDURE (*78*) ^Condition;
		PROCEDURE (*79*) ^Expression;
		PROCEDURE (*82*) ^EndStatement;
		PROCEDURE (*83*) ^NewStatementLine;
		PROCEDURE (*99*) ^BlockStatement;

		PROCEDURE MemorizeName;
			VAR i: INTEGER;
		BEGIN
			C2cpTrace.Sub(106);
			IF (s.class = C2cpFiles.cName) OR (s.s = '_') THEN
				name := s.s$; (* get C version of name *)
				WHILE (s.nextCh = '_') OR C2cpStrings.IsAlpha(s.nextCh) OR C2cpStrings.IsDigit(s.nextCh) DO
					s.Scan;
					name := name + s.s;
				END;
				(* *)
				i := C2cpTable.Search(group, name);
				alias := C2cpTable.GetAlias(i)$;
				symtype := C2cpTable.GetType(i)$;
				sign := C2cpTable.GetSign(i);
				C2cpTable.Merken(i, s.lineNumber);
				(* *)
				IF alias = '' THEN alias := name$ END; (* if name is not in table *)
				IF symtype = 'REAL' THEN int := FALSE
				ELSIF symtype = 'REAL(*long*)' THEN int := FALSE
				ELSIF symtype = 'TEXTFILE' THEN isfile := TRUE
				ELSIF symtype = 'ARRAY OF CHAR' THEN ischar := TRUE
				END;
				(* *)
				s.s := alias$;
				s.class := C2cpFiles.cName;
			END;
		END MemorizeName;

		PROCEDURE Swap;
			VAR token: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(107);
			token := s.s$;
			IF (s.s = '=') & (s.nextCh = '=') (* merge character to token *)
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
				token := token + s.s;
			END;
			IF token = '==' THEN s.s := '=' (* and exchange *)
			ELSIF token = '=' THEN s.s := ':='
			ELSIF token = '!=' THEN s.s := '#'
			ELSIF token = '->' THEN s.s := '.'
			ELSIF token = '~' THEN s.s := '~'; bitwise := TRUE;
			ELSIF token = '&' THEN s.s := ' & '; bitwise := TRUE;
			ELSIF token = '|' THEN s.s := ' OR '; bitwise := TRUE;
			ELSIF token = '^' THEN s.s := ' XOR '; bitwise := TRUE;
			ELSIF token = '!' THEN s.s := '~'; bitwise := FALSE;
			ELSIF token = '&&' THEN s.s := ' & '; bitwise := FALSE;
			ELSIF token = '||' THEN s.s := ' OR '; bitwise := FALSE;
			ELSIF int & (token = '/') THEN s.s := ' DIV '
			ELSIF token = '%' THEN s.s := ' MOD '
			ELSIF token = '<<' THEN s.s := ' SHL '
			ELSIF token = '>>' THEN s.s := ' SHR '
			ELSIF token = '<:' THEN s.s := '['
			ELSIF token = ':>' THEN s.s := ']'
			ELSIF token = '%:' THEN s.s := '#'
			ELSE s.s := token$
			END;
			IF define & (token = '=') THEN defineFunction := TRUE END; 
			IF (s.s = '0') & (s.nextCh = 'x') THEN (* hex number 0x1b -> 01BH *)
				s.Scan;
				s.s := '0' + C2cpStrings.ToUpper(C2cpStrings.Right(s.s, LEN(s.s$) - 1)) + 'H';
				s.class := C2cpFiles.cInteger;
			END;
			IF s.class = C2cpFiles.cString THEN
				IF s.s[0] = '"' THEN (* string constant *)
					token := '';
					WHILE s.s # '' DO
						token := token + C2cpStrings.Parse(s.s, "'");
						IF s.s # '' THEN token := token + "''" END; (* double '  *)
					END;
					s.s := "'" + C2cpStrings.SubStr(token, 1, LEN(token$) - 2) + "'"; (* " -> ' *)
				ELSIF (LEN(s.s$) = 4) & (s.s[1] = '\') THEN
					CASE s.s[2] OF (* character constant \x *)
					| '0': s.s := '0X';
					| 'n': s.s := '0DX';
					| 't': s.s := '09X';
					| '\': s.s := "'" + '\' + "'";
					ELSE; (* do nothing *)
					END;
				END;
			END;
			(* pointer variable *)
			IF (s.s = '*') & (C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '_')) THEN (* *x --> x^ *)
				s.Scan;
				MemorizeName;
				IF (sign = 'Z') OR (sign = 'X') OR (sign = 'U') THEN
				s.s := s.s + '^' ELSE s.s := '*' + s.s
				END;
				vorne := FALSE;
				pointer := TRUE;
				INC(dim);
			END;
			(* The address of variable x *)
			IF (token = '&') & (C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '_')) THEN (* &x --> @x *)
				s.Scan;
				MemorizeName;
				asterix := TRUE;
			END;
			IF (s.class = C2cpFiles.cInteger) & (C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '_')) THEN
				s.Scan;
				s.s := token + '(*' + s.s + '*)';
				s.class := C2cpFiles.cInteger;
			END;
		END Swap;

		PROCEDURE Look; (* first translation of current token *)
		BEGIN
			C2cpTrace.Sub(108);
			IF s.s = '\' THEN
				IF ~define THEN Output(C2cpStrings.line) END;
				s.Scan;
				IF s.class = C2cpFiles.cSpace THEN s.Scan END;
				IF s.class = C2cpFiles.cEOL THEN s.Scan END;
			END;
			IF s.class = C2cpFiles.cSpace THEN
				IF s.nextCh = C2cpStrings.line THEN s.Scan END;
			ELSE
				IF (s.class = C2cpFiles.cName) OR (s.s = '_') THEN MemorizeName ELSE Swap END;
			END;
		END Look;

		PROCEDURE LookAhead; (* if next character is blank then scan *)
		BEGIN
			C2cpTrace.Sub(109);
			IF C2cpStrings.IsSpace(s.nextCh) THEN s.Scan END; (* s.s = blank & s.nextCh # blank *)
			IF s.nextCh = '\' THEN
				IF ~define THEN Output(C2cpStrings.line) END;
				s.Scan;
				IF C2cpStrings.IsSpace(s.nextCh) THEN s.Scan END;
				IF s.nextCh = C2cpStrings.line THEN s.Scan END;
				IF C2cpStrings.IsSpace(s.nextCh) THEN s.Scan END;
				s.s := C2cpStrings.line;
				s.class := C2cpFiles.cEOL;
			END;
		END LookAhead;

		PROCEDURE LookAhead2;
		BEGIN
			C2cpTrace.Sub(110);
			LookAhead;
			IF (s.nextCh = C2cpStrings.line) & ~ define THEN (* skip over line too *)
				s.Scan;
				LookAhead;
			END;
		END LookAhead2;

		PROCEDURE SkipSpace; (* get next character, which is not blank *)
		BEGIN
			C2cpTrace.Sub(111);
			LookAhead;
			IF s.nextCh # '/' THEN (* if next character is not beginning of comment *)
				s.Scan;
				Look; (* connect name with '_' *)
			END;
		END SkipSpace;

		PROCEDURE SkipEol; (* get next charater, which are not blank & not line *)
		BEGIN
			C2cpTrace.Sub(112);
			LookAhead;
			IF s.nextCh # '/' THEN (* if next character is not beginning of comment *)
				s.Scan;
				IF s.class = C2cpFiles.cEOL THEN 
					s.Scan; 
					IF s.class = C2cpFiles.cSpace THEN s.Scan END;
				END;
				Look; (* connect name with '_' *)
			END;
		END SkipEol;

		PROCEDURE StorageClass;
		BEGIN
			C2cpTrace.Sub(113);
			classdef := FALSE;
			WHILE (s.s = 'auto')
				OR (s.s = 'public')
				OR (s.s = 'class')
				OR (s.s = 'new') & C2cpStrings.IsSpace(s.nextCh)
				OR (s.s = 'static')
				OR (s.s = 'register')
				OR (s.s = 'extern')
				OR (s.s = 'local')
				OR (s.s = 'const')
				OR (s.s = 'volatile')
				OR (s.s = 'far')
				OR (s.s = 'FAR')
				OR (s.s = 'inline')
				OR (s.s = 'interrupt')
				OR (C2cpTable.GetSign(C2cpTable.Search(group, s.s)) = 'D')
				DO
				special := special + '(*' + s.s + '*)';
				IF s.s = 'class' THEN classdef := TRUE END;
				SkipEol;
			END;
		END StorageClass;

		PROCEDURE MemorizeType;
		BEGIN
			C2cpTrace.Sub(114);
			pointer := FALSE; dim := 0;
			special := '';
			StorageClass;
			basetype := TRUE;
			IF s.s = 'unsigned' THEN vartype := 'INTEGER(*word*)'
			ELSIF s.s = 'signed' THEN vartype := 'INTEGER'
			ELSIF s.s = 'short' THEN vartype := 'SHORTINT'
			ELSIF s.s = 'long' THEN vartype := 'LONGINT'
			ELSIF s.s = 'char' THEN vartype := 'CHAR'
			ELSIF s.s = 'int' THEN vartype := 'INTEGER'
			ELSIF s.s = 'float' THEN vartype := 'REAL'
			ELSIF s.s = 'double' THEN vartype := 'REAL'
			ELSIF s.s = 'void' THEN vartype := 'POINTER'
			ELSIF s.s = 'bool' THEN vartype := 'BOOLEAN'
			ELSIF s.s = 'boolean' THEN vartype := 'BOOLEAN'
			ELSIF s.s = 'FILE' THEN vartype := 'TEXT(*FILE*)'
			ELSE vartype := s.s$; basetype := FALSE
			END;
			IF s.s = 'string' THEN
				vartype := 'CHAR'; pointer := TRUE; INC(dim);
				IF s.nextCh = '[' THEN s.Scan; IF s.nextCh = ']' THEN s.Scan END END;
			END;
		END MemorizeType;

		PROCEDURE MemorizeTypeChain;
		BEGIN
			C2cpTrace.Sub(115);
			StorageClass;
			IF s.s = 'short' THEN
				IF vartype = 'INTEGER(*word*)' 
				THEN vartype := 'SHORTINT(*byte*)' 
				ELSE vartype := 'SHORTINT' 
				END;
				SkipEol;
			END;
			IF s.s = 'long' THEN
				IF vartype = 'INTEGER(*word*)' 
				THEN vartype := 'LONGINT(*unsigned*)' 
				ELSE vartype := 'LONGINT' 
				END;
				SkipEol;
			END;
			IF s.s = 'char' THEN
				IF vartype = 'INTEGER(*word*)' 
				THEN vartype := 'CHAR(*unsigned*)' 
				ELSE vartype := 'CHAR(*signed*)' 
				END;
				SkipEol;
			END;
			IF s.s = 'double' THEN
				vartype := 'REAL(*long*)';
				SkipEol;
			END;
			StorageClass;
			IF s.s = 'int' THEN SkipEol END; (* unsigned long/short int *)
			StorageClass;
			IF (s.s = '*') OR (s.s = ' & ') THEN
				pointer := TRUE; INC(dim);
				LookAhead2;
				name := '';
				alias := '?';
				IF (s.nextCh # ',') & (s.nextCh # ')') THEN SkipEol END;
			END;
		END MemorizeTypeChain;

		PROCEDURE ConvertType;
			VAR i: INTEGER; strucname: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(116);
			IF recpoin = '' THEN
				IF pointer THEN
					IF C2cpStrings.SubStr(vartype, 0, 4)$ = 'CHAR' THEN vartype := 'ARRAY OF CHAR' END;
				ELSE
					IF vartype = 'ARRAY OF CHAR' THEN vartype := 'CHAR' END;
				END;
				IF (vartype = 'TEXT(*FILE*)') OR (vartype = 'TEXTFILE') THEN
					pointer := FALSE; basetype:=FALSE; dim := 0;
				ELSIF vartype = 'POINTER' THEN
					pointer := TRUE;
				ELSIF vartype = 'ARRAY OF CHAR' THEN
					(* do nothing *)
				ELSIF pointer THEN
					IF C2cpStrings.Left(vartype, 11)$ # 'POINTER TO ' THEN
						IF basetype THEN
							FOR i := 1 TO dim  DO
								vartype := 'ARRAY OF ' + vartype;
							END;
						END;
						vartype := 'POINTER TO ' + vartype
					END;
				ELSE
					IF C2cpStrings.Left(vartype, 11)$ = 'POINTER TO ' THEN
						vartype := C2cpStrings.After(vartype, 'POINTER TO ')$
					END;
				END;
				sign := 'V';
				IF pointer  THEN sign := 'Z' END;
				IF C2cpStrings.Left(C2cpTable.GetType(C2cpTable.Search(group, vartype)), 11)$ = 'POINTER TO '
				THEN
					sign := 'Z'
				END;
				IF (follow # '') & (C2cpStrings.Left(vartype, 11)$ = 'POINTER TO ') THEN sign := 'U' END;
				IF (follow # '') & asterix THEN sign := 'U' END;
				IF vartype = 'TEXT(*FILE*)' THEN sign := 'U' END;
				IF vartype = 'POINTER' THEN sign := 'X' END;
				IF C2cpStrings.Left(vartype, 11)$ = 'POINTER TO ' THEN
					i := C2cpTable.Search(group, C2cpStrings.After(vartype, 'POINTER TO '));
					strucname := C2cpTable.GetInfo(i)$;
					IF strucname # '' THEN
						vartype := C2cpTable.GetInfo(C2cpTable.Search(group, strucname))$;
						sign := 'Z';
					END;
				END;
			ELSE
				vartype := recpoin$;
				sign := 'Z';
				recpoin := '';
				C2cpTable.Update(group, varname, '', vartype, '', ' ', 0);
			END;
			IF typedef & ~vardef THEN
				sign := 'T';
				C2cpTable.Update(group, varname, '', vartype, '', sign, s.lineNumber); (* force TYPE over CONST *)
			END;
		END ConvertType;

		PROCEDURE CheckUpType (): C2cpStrings.String;
			VAR i: INTEGER;
		BEGIN
			C2cpTrace.Sub(117);
			i := C2cpTable.Search(group, name);
			IF i > 0 THEN
				sign := C2cpTable.GetSign(i);
				RETURN C2cpTable.GetType(i);
			ELSE
				RETURN C2cpStrings.NewString(vartype);
			END;
		END CheckUpType;

		PROCEDURE ListVariableType (ntype, follow: ARRAY OF CHAR): C2cpStrings.String;
			VAR t: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(118);
			t := '';
			IF xarray > 0 THEN t := t + ' OF '
			ELSIF sign = 'T' THEN t := t + ' = '
			ELSE t := t + ': '
			END;
			t := t + ntype;
			IF s.s # ':=' THEN
				t := t + follow$;
				IF union > 0 THEN t := t + '(*' + C2cpStrings.IntToString(union) + '*)'; INC(union) END;
			END;
			xarray := 0;
			vcomma := FALSE;
			RETURN C2cpStrings.NewString(t);
		END ListVariableType;

		PROCEDURE ReadParenthesiseVariable;
		BEGIN
			C2cpTrace.Sub(119);
			IF s.s = '(' THEN
				SkipSpace;
				LookAhead;
				IF s.nextCh = ')' THEN s.Scan END;
				s.s := name$;
				s.class := C2cpFiles.cName;
				pointer := FALSE; basetype:=FALSE; dim := 0;
			END;
		END ReadParenthesiseVariable;

		PROCEDURE OutputVariableType;
		BEGIN
			C2cpTrace.Sub(120);
			IF enum = 0 THEN
				Output(ListVariableType(partype$, ';'));
			END;
			IF vcomma & UseNamedInteger THEN Output(', ') END;
			vcomma := FALSE
		END OutputVariableType;

		PROCEDURE DoDefault;
		BEGIN
			C2cpTrace.Sub(121);
			IF (art[stack] = 'C') & ~ default THEN
				Output(C2cpStrings.tab); Output('ELSE');
				IF ~ isbreak THEN
					name := 'L' + C2cpStrings.IntToString(labelnr);
					C2cpTable.Insert(group, name, '', '', '', 'L', s.lineNumber);
					labelname := C2cpTable.GetAlias(C2cpTable.Search(group, name))$;
					Output(C2cpStrings.line); Output(Margin(2)); Output(labelname); Output(': ');
				END;
				Output(C2cpStrings.line);
				Output(Margin(1)); Output('(* nothing to do *)' + C2cpStrings.line);
				Output(Margin(0));
			END;
		END DoDefault;

		PROCEDURE OutputName (IN name, type: ARRAY OF CHAR);
		BEGIN
			C2cpTrace.Sub(122);
			Output(C2cpStrings.line); Output(Margin(0));
			Output(special);
			IF typedef THEN
				Output('TYPE '); Output(name); Output(' = ');
			ELSE
				Output('VAR '); Output(name); Output(': ');
			END;
			Output(type); Output(';');
		END OutputName;

		PROCEDURE (*123*) CopyUntil (IN word, append, completion: ARRAY OF CHAR);
			VAR merke: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(123);
			IF s.s = word THEN (* End of copy: word found *)
				Output(append); Output(completion);
				define := FALSE;
				defineConst := FALSE;
				defineFunction := FALSE;
				state := 2;
			ELSE
				IF s.nextCh = '/' THEN
					merke := s.s$;
					s.Scan;
					IF s.nextCh = '*' THEN (* End of copy: comment found *)
						s.Scan;
						Output(append); Output(merke); Output('(*');
						define := FALSE;
						defineConst := FALSE;
						defineFunction := FALSE;
						state := 3;
						back := 2;
					ELSIF s.nextCh = '/' THEN (* End of copy: comment found *)
						s.Scan;
						Output(append); Output(merke); Output('(* //');
						define := FALSE;
						defineConst := FALSE;
						defineFunction := FALSE;
						state := 4;
						back := 2;
					ELSE
						Look;
						Output(merke); Output(s.s);
					END;
				ELSE
					Output(s.s);
				END;
			END;
		END CopyUntil;

		PROCEDURE (*1*) StartBlock;
		BEGIN
			C2cpTrace.Sub(1);
			back := 0; (* next state behind comment *)
			deepness := 0; (* number of open BEGIN which are not closed with END *)
			bracket := 0; (* number of open '(' bracket *)
			endbracket := 0; (* bracket deepness during the start of define *)
			closed := 0; (* if bracket < closed then insert bracket *)
			bound := 1; (* if bracket <= bound then insert condition *)
			xarray := 0; (* dimension of array *)
			dim := 0; (* dimension of pointer array *)
			record := 0; (* counter for nested records *)
			union := 0; (* counter for union record *)
			enum := 0; (* counter for enum *)
			lineNumber := 0; (* source line number at procedure or function start *)
			parameterCount := 1; (* parameter counter for 'printf' *)
			ma := 0; (* '(' position inside term *)
			mi := 0; (* ':=' position inside term *)
			mk := 0; (* bracket deepness of ma *)
			classdef := FALSE; (* Is it class definition? *)
			typedef := FALSE; (* TRUE = type definition - FALSE = variable definition *)
			recdef := FALSE; (* Is it record type or record variable definition? *)
			vardef := FALSE; (* Is it variable definition? *)
			asterix := FALSE; (* Is it the address of a variable ? * -> @ *)
			pointer := FALSE; (* Is the variable a pointer? *)
			basetype := FALSE; (* Is it a pointer to basetype? *)
			omitted := FALSE; (* Is the record name missing ? *)
			vcomma := FALSE; (* specify variable with comma *)
			vbegin := FALSE; (* hold BEGIN until VAR definition is done *)
			anfang := TRUE; (* Are we at the beginning of a new line? *)
			vorne := TRUE; (* At the beginning of a statement in front of := *)
			addBracket := FALSE; (* in condition add bracket *)
			delBracket := FALSE; (* in condition delete bracket *)
			bitwise := FALSE; (* Is it bitwise AND, OR, XOR or NOT ? *)
			int := TRUE; (* Is the expression from type INTEGER ? *)
			isfile := FALSE; (* Is the expression from from type FILE ? *)
			ischar := FALSE; (* Is the expression from type ARRAY OF CHAR ? *)
			default := FALSE; (* Is it the default part of select statement ? *)
			isbreak := FALSE; (* Is at the end of case a break statement ? *)
			isgoto := FALSE; (* Is a goto, halt or return statement inside the case ? *)
			dummy := FALSE; (* add dummy variable to the function call *)
			return := FALSE; (* behind the ';' insert a RETURN statement *)
			define := FALSE; (* #define in FUNCTION oder CONST übersetzen *)
			defineConst := FALSE; (* translate '#define' in FUNCTION or CONST *)
			defineFunction := FALSE; (* change translation of '#define' from CONST to FUNCTION *)
			ln := FALSE; (* insert the statement StdLog.Ln; *)
			alt := ''; (* name of the last group *)
			(* namespace := ''; *) (* namespace name - not permitted to delete !!! *)
			(* class := ''; *) (* class name - not permitted to delete !!! *)
			group := ''; (* group  = name of the current translated procedure *)
			(* name := ''; *) (* name - not permitted to delete !!! *)
			(* alias := ''; *) (* alias name - not permitted to delete !!! *)
			(* symtype := ''; *) (* type of the variable found in the symbol table - not permitted to delete !!! *)
			(* sign := ' '; *) (* type of the name - not permitted to delete !!! *)
			constname := ''; (* constant name *)
			arrayname := ''; (* array name *)
			arraysign := 'C'; (* sign of the array name *)
			recname := ''; (* record name *)
			recpoin := ''; (* record pointer name *)
			alloc := ''; (* pointer name for memory allocation *)
			varname := ''; (* variable name or variable name of assigment *)
			special := ''; (* supplement to the variable type *)
			vartype := ''; (* type of the variable in the source program *)
			partype := ''; (* type of the last variable in parameter list *)
			parvar := ' '; (* Is it a VAR parameter ? *)
			labelnr := 0; (* Label counter for GOTO statements *)
			labelname := ''; (* the alias name of the label *)
			term := ''; (* using for ++ and -- statements *)
			plusplus := ''; (* behind this statement follows an INC or DEC *)
			follow := ''; (* behind the bracket following keyword - insert follow behind the bracket *)
			blank := ' '; (* spaces in front of comment *)
			relation := '';  (* last relation is <, <=, =, >=, > or # *)
			index := 0; (* index 0..4 to the marker - used by the FOR statement *)
			stack := 0; (* number of memorise data *)
			Pop; (* empty stack *)
			state := 2;
			Block;
		END StartBlock;

		PROCEDURE (*2*) Block;
		BEGIN
			C2cpTrace.Sub(2);
			BlockStatement;
		END Block;

		PROCEDURE (*3*) Comment1;
		BEGIN
			C2cpTrace.Sub(3);
			IF C2cpStrings.Pos('*/', s.s) # - 1 THEN
				Output(C2cpStrings.Before(s.s, '*/'));
				Output('*)');
				Output(C2cpStrings.After(s.s, '*/'));
				state := back;
			ELSE
				Output(s.s);
				IF (s.s = '*') & (s.nextCh = '/') THEN
					s.Scan;
					Output(')');
					state := back;
				END;
			END;
		END Comment1;

		PROCEDURE (*4*) Comment2;
		BEGIN
			C2cpTrace.Sub(4);
			IF s.s = C2cpStrings.line THEN
				Output(' *)');
				Output(s.s);
				state := back;
			ELSE
				Output(s.s);
				IF s.nextCh = C2cpStrings.line THEN
					Output(' *)');
					state := back;
				END;
			END;
		END Comment2;

		(*5 not used*)

		PROCEDURE (*6*) DoProcedure;
			VAR forward: BOOLEAN;
		BEGIN
			C2cpTrace.Sub(6);
			forward := FALSE;
			ConvertType;
			IF vartype = (*'void'*) 'POINTER' THEN
				sign := 'P';
				symtype := '';
			ELSE
				sign := 'F';
				symtype := vartype$;
			END;
			IF class # '' THEN follow := ', NEW;' ELSE follow := ';' END;
			C2cpTable.Insert(group, name, '', symtype, '', sign, 1);
			symtype := CheckUpType()$;
			IF sign = 'F' THEN follow := ListVariableType(symtype$, follow$)$ END;
			state := 7; (* with FORWARD ... *)
			IF define OR (class # '') THEN
				Output(C2cpStrings.line); Output(Margin(0));
				state := 10 (* without FORWARD ... *)
			ELSIF (s.lineNumber # C2cpTable.GetLinenumber(C2cpTable.Search(group, name))) OR vbegin THEN
				Output('(* '); (* ..... in xxx.DEF *)
				follow := follow + ' *)';
				forward := TRUE;
				state := 10;
			ELSIF name = 'main' THEN
				Output(C2cpStrings.line); Output(Margin(0));
				state := 13
			END;
			Output(special);
			Output('PROCEDURE ');
			IF class # '' THEN Output('(s: '); Output(class); Output(') ') END;
			IF forward THEN Output('^ ') END;
			alt := group$;
			group := name$;
			lineNumber := s.lineNumber;
		END DoProcedure;

		PROCEDURE (*7*) (*10*) ParameterStart;
		BEGIN
			IF state = 7 THEN C2cpTrace.Sub(7) ELSE C2cpTrace.Sub(10) END;
			Output(alias);
			IF s.s = '(' THEN
				IF s.nextCh = ')' THEN
					IF sign = 'F' THEN Output(' ()') END;
					Output(follow);
					follow := '';
					state := state + 2; (*9 or 12*)
				ELSE
					SkipSpace;
					IF s.s = 'void' THEN
						LookAhead;
						s.s := 'void';
						s.class := C2cpFiles.cName;
					END;
					IF (s.s$ = 'type') & (s.nextCh = ')') THEN
						C2cpTable.Update('', group, '', '', '', 'D', s.lineNumber);
						group := alt$;
						follow := '';
						state := 16;
					ELSIF (s.s = 'void') & (s.nextCh = ')') THEN
						Output(follow);
						follow := '';
						state := state + 2; (*9 or 12*)
					ELSE
						Output(' (');
						parvar := ' ';
						partype := '';
						state := state + 1; (*8 or 11*)
						ParameterList;
					END;
				END;
			END;
		END ParameterStart;

		PROCEDURE (*8*) (*11*) ParameterList;
		BEGIN
			IF state = 8 THEN C2cpTrace.Sub(8) ELSE C2cpTrace.Sub(11) END;
			IF s.class = C2cpFiles.cName THEN
				MemorizeType;
				LookAhead2;
				IF C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '_') OR (s.nextCh = '*') OR (s.nextCh = '&') THEN
					SkipEol; (* Parameter: TYPE NAME *)
					MemorizeTypeChain;
					ConvertType; (* the variable pointer sets inside Swap *)
					C2cpTable.Insert(group, name, '', vartype, '', sign, s.lineNumber);
					symtype := CheckUpType()$;
					IF sign = 'U' THEN
						Output('VAR ');
						IF C2cpStrings.Left(symtype, 11)$ = 'POINTER TO ' THEN
							symtype := C2cpStrings.After(symtype, 'POINTER TO ')$
						END;
					END;
					IF sign = 'Z' THEN Output('VAR ') END;
					Output(alias); Output(': ');
					LookAhead2;
					IF s.nextCh = '[' THEN
						IF (sign # 'U') & (sign # 'Z') THEN
							sign := 'U';
							C2cpTable.Update(group, name, '', '', '', sign, 0);
						END;
						SkipEol;
						IF s.nextCh = ']' THEN
							SkipEol;
							Output('ARRAY OF ');
						ELSE
							Output('(*[');
							WHILE s.s # ']' DO
								SkipEol;
								Output(s.s);
							END;
							Output('*)');
						END;
					END;
					Output(special); Output(symtype);
					IF s.nextCh = ',' THEN Output('; '); SkipSpace; END;
				ELSE (* Parameter: Name only *)
					IF define THEN
						vartype := 'INTEGER';
						C2cpTable.Insert(group, name, '', vartype, '', 'V', s.lineNumber);
					END;
					symtype := CheckUpType()$;
					IF (sign = 'U') THEN
						IF C2cpStrings.Left(symtype, 11)$ = 'POINTER TO ' THEN
							symtype := C2cpStrings.After(symtype, 'POINTER TO ')$
						END;
					END;
					IF define OR (parvar # sign) OR (partype # symtype) THEN
						IF partype # '' THEN Output(': '); Output(partype); Output('; ') END;
						IF define OR (sign = 'U') OR (sign = 'Z') THEN Output('VAR ') END;
					ELSE
						IF partype # '' THEN Output(', ') END;
					END;
					Output(alias);
					parvar := sign;
					partype := symtype$;
					LookAhead2;
					IF s.nextCh # ',' THEN
						Output(': '); Output(partype);
					END;
				END; (* -------------------- *)
			ELSE
				IF s.s = ',' THEN LookAhead2 (* skip blank behind comma *)
				ELSE Output(s.s) END;
			END;
			IF s.nextCh = ')' THEN
				Output(')'); Output(follow);
				follow := '';
				state := state + 1; (*9 oder 12*)
			END;
		END ParameterList;

		PROCEDURE (*9*) (*12*) ParameterEnd;
			VAR
				info, separator: ARRAY 256 OF CHAR;
		BEGIN
			IF state = 9 THEN C2cpTrace.Sub(9) ELSE C2cpTrace.Sub(12) END;
			LookAhead2;
			IF s.nextCh = ',' THEN (* FORWARD *)
				SkipEol;
				Output(C2cpStrings.line); Output(Margin(0));
				group := alt$;
				typedef := FALSE;
				special := '';
				state := 26;
			ELSIF (s.nextCh = ';') OR (s.nextCh = ')') THEN (* FORWARD *)
				SkipSpace;
				group := alt$;
				state := 2;
			ELSE (* Here starts the new procedure *)
				info := C2cpTable.GetInfo(C2cpTable.Search(alt, group))$;
				IF info # '' THEN Output(' '); Output(info); Output(';') END;
				Output(C2cpStrings.line); Output(Margin(0));
				labelname := C2cpTable.FindFirst(group, 'L')$; (* label list *)
				separator := ' ';
				IF labelname # '' THEN
					Output('LABEL');
					WHILE labelname # '' DO
						Output(separator); Output(labelname);
						labelname := C2cpTable.FindNext()$;
						separator := ', ';
					END;
					Output(';' + C2cpStrings.line); Output(Margin(0));
				END;
				IF define & (s.nextCh # '{') THEN
					INC(deepness);
					Output('BEGIN' + C2cpStrings.line);
					IF s.nextCh = '(' THEN
						SkipEol;
						bracket := 1;
						endbracket := 1;
						delBracket := TRUE;
					END;
					s.s := C2cpStrings.line;
					s.class := C2cpFiles.cEOL;
					state := 76;
				ELSE
					C2cpTable.Update(alt, group, '', '', '', ' ', lineNumber);
					LookAhead2;
					IF s.nextCh # '{' THEN
						Output('(*-' + C2cpStrings.line);
						follow := '-*)' + C2cpStrings.line + Margin(0);
						s.s := C2cpStrings.line;
						s.class := C2cpFiles.cEOL;
					ELSE
						s.s := ' ';
						s.class := C2cpFiles.cSpace;
					END;
					state := 2;
				END;
			END;
		END ParameterEnd;

		PROCEDURE (*13*) RunParameter;
		BEGIN
			C2cpTrace.Sub(13);
			state := 10;
			ParameterStart;
		END RunParameter;

		PROCEDURE (*14*) Preprocessor;
		BEGIN
			C2cpTrace.Sub(14);
			IF s.class = C2cpFiles.cSpace THEN s.Scan END;
			IF s.s = 'include' THEN
				LookAhead;
				Output('(* #include ');
				state := 15;
			ELSIF s.s = 'define' THEN
				define := TRUE;
				xarray := 0;
				pointer := FALSE; basetype:=FALSE; dim := 0;
				state := 18;
			ELSIF s.s = 'asm' THEN
				C2cpTable.Update('', group, '', '', 'ASSEMBLER', ' ', 0);
				Output('ASM');
				INC(deepness);
				state := 17;
			ELSE
				Output('(* #'); Output(s.s);
				state := 16;
			END;
		END Preprocessor;

		PROCEDURE (*15*) IncludeStart;
		BEGIN
			C2cpTrace.Sub(15);
			IF C2cpStrings.Left(s.s, 1)$ = "'" THEN (* "..." -> '...' in Swap *)
				Output(C2cpStrings.SubStr(s.s, 1, LEN(s.s$) - 2));
			ELSE
				IF s.s = '<' THEN SkipEol END;
				Output(name); (* use name and not alias !!! *)
				IF s.nextCh = '.' THEN
					SkipEol;
					Output(s.s);
					SkipEol;
					Output(name); (* use name and not alias !!! *)
				END;
				IF s.nextCh = '>' THEN SkipSpace END;
			END;
			state := 99;
		END IncludeStart;

		PROCEDURE (*16*) (*99*) IncludeName;
		BEGIN
			IF state = 99 THEN C2cpTrace.Sub(99) ELSE C2cpTrace.Sub(16) END;
			state := 16;
			CopyUntil(C2cpStrings.line, ' *)', C2cpStrings.line); (* end of #include *)
		END IncludeName;

		PROCEDURE (*17*) DoAsmBlock;
			VAR newline: BOOLEAN;
				a: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(17);
			newline := FALSE;
			IF s.class = C2cpFiles.cEOL THEN
				newline := TRUE;
				SkipEol;
			END;
			IF s.s = '#' THEN
				SkipSpace; (* end asm *)
				IF ~ vbegin THEN Output(C2cpStrings.line); Output(Margin(0)); Output('END;') END;
				vbegin := FALSE;
				DEC(deepness);
				state := 2;
			ELSE
				IF newline THEN
					Output(C2cpStrings.line);
					IF s.nextCh # ':' THEN Output(Margin(0)) END;
				END;
				IF s.s = ';' THEN (* found comment *)
					Output('(* ;');
					state := 4;
					back := 17;
				ELSE
					IF s.nextCh = ':' THEN
						a := C2cpStrings.ToUpper(s.s)$;
						IF (a # 'CS') & (a # 'SS') & (a # 'DS') & (a # 'ES') THEN
							C2cpTable.Insert(group, name, '', '', '', 'S', s.lineNumber)
						END;
					END;
					Output(s.s);
				END;
			END;
		END DoAsmBlock;

		PROCEDURE (*18*) DoDefine;
		BEGIN
			C2cpTrace.Sub(18);
			IF s.class # C2cpFiles.cSpace THEN
				(* Caution: Here is no LookAhead, *)
				(* because blank is not allowed between name and '(' bracket ! *)
				defineConst := TRUE;
				IF (sign = 'D') OR (sign = 'S') OR C2cpTable.Reserviert(s.s) THEN
					C2cpTable.Insert(group, name, '', '', '', 'S', s.lineNumber);
					Output('(* #define '); Output(s.s);
					state := 16;
				ELSIF (sign = 'F') OR (s.nextCh = '(') OR (sign = 'P') OR (s.nextCh = '{') THEN
					defineConst := FALSE;
					vartype := symtype$;
					IF vartype = '' THEN vartype := 'INTEGER' END;
					return := sign = 'F';
					DoProcedure;
					IF s.nextCh # '(' THEN (* FUNCTION without parameter *)
						Output(alias);
						IF sign = 'F' THEN Output('()') END;
						Output(follow);
						follow := '';
						state := 12;
					END;
				ELSE
					LookAhead;
					IF (s.nextCh = C2cpStrings.line) OR (s.nextCh = '/') THEN
						Output('(* $DEFINE '); Output(name); Output(' *)');
						IF s.nextCh = '/' THEN Output(' ') END;
						define := FALSE;
						defineConst := FALSE;
						defineFunction := FALSE;
						state := 2;
					ELSE
						constname := '';
						IF (s.nextCh = '(') & (sign # 'C') THEN constname := name$ END;
						vartype := '';
						C2cpTable.Insert(group, name, '', vartype, '', 'C', s.lineNumber);
						Output('CONST '); Output(alias); Output(' = ');
						SkipEol;
						IF s.s = '(' THEN
							bracket := 1;
							delBracket := TRUE;
							term := '';
							state := 79;
						ELSIF (C2cpStrings.Right(s.s, 1)$ = 'H') & (LEN(s.s$) = 4) THEN
							s.s[3] := 'X'; (* change H to X - constant is CHAR instead of INTEGER *)
							Output(s.s); (* !!! is not always useful !!! *)
							state := 19;
						ELSE
							Output(s.s);
							state := 19;
						END;
					END;
				END;
				vorne := FALSE;
				plusplus := '';
			END;
		END DoDefine;

		PROCEDURE (*19*) DoConstant;
		BEGIN
			C2cpTrace.Sub(19);
			CopyUntil(C2cpStrings.line, ';', C2cpStrings.line); (* #define - CONST *)
		END DoConstant;

		PROCEDURE (*20*) DeclareType;
		BEGIN
			C2cpTrace.Sub(20);
			IF (s.class # C2cpFiles.cSpace) & (s.class # C2cpFiles.cEOL) THEN
				MemorizeType;
				IF vartype = 'struct' THEN
					typedef := TRUE;
					recdef := TRUE;
					vardef := FALSE;
					record := 0;
					union := 0;
					enum := 0;
					state := 21;
				ELSIF vartype = 'union' THEN
					typedef := TRUE;
					recdef := TRUE;
					vardef := FALSE;
					record := 0;
					union := 1;
					enum := 0;
					state := 21;
				ELSIF vartype = 'enum' THEN
					typedef := TRUE;
					recdef := TRUE;
					vardef := FALSE;
					record := 0;
					union := 0;
					enum := 1;
					state := 21;
				ELSE
					typedef := TRUE;
					recdef := FALSE;
					vardef := FALSE;
					record := 0;
					state := 26;
				END;
			END;
		END DeclareType;

		PROCEDURE (*21*) DeclareRecord;
		BEGIN
			C2cpTrace.Sub(21);
			IF (s.class # C2cpFiles.cSpace) & (s.class # C2cpFiles.cEOL) THEN
				pointer := FALSE; basetype := FALSE; dim := 0;
				IF s.s # '{' THEN
					LookAhead2;
					IF s.nextCh = '{' THEN SkipSpace END;
				END;
				IF s.s = '{' THEN
					INC(record);
					LookAhead;
					vartype := 'RECORD';
					IF enum # 0 THEN vartype := 'INTEGER(*enum*)' END;
					IF union # 0 THEN vartype := 'RECORD(*union*)' END;
					IF (name = 'struct') OR (name = 'union') OR (name = 'enum') THEN
						name := name + C2cpStrings.IntToString(s.lineNumber);
						alias := C2cpTable.GetAlias(C2cpTable.Search(group, name))$;
						omitted := TRUE;
					ELSE
						omitted := FALSE;
					END;
					INC(deepness);
					recname := name$; (* use name and not alias !!! *)
					C2cpTable.Insert(group, recname, '', vartype, '', 'T', s.lineNumber);
					IF record = 1 THEN 
						Output(special); Output('TYPE '); Output(alias); Output(' =');
					ELSE
						Output(alias); Output(':');
					END;
					IF enum = 0 THEN
						Output(' RECORD ');
					ELSE
						IF UseNamedInteger THEN
							Output(' ( ');
						ELSE
							Output(' INTEGER(*enum*);' + C2cpStrings.line); 
							Output(Margin( - 1)); Output('CONST ');
						END;
					END;
					IF union > 0 THEN
						Output(C2cpStrings.line); Output(Margin(0)); Output('(*union*)');
					END;
					Push('V', deepness, omitted);
					anw[stack] := group$;
					group := recname$;
					state := 2;
				ELSE
					recpoin := C2cpTable.GetInfo(C2cpTable.Search(alt, name))$;
					vartype := alias$;
					IF ~typedef THEN
						recdef := FALSE;
						record := 0;
						union := 0;
						enum := 0;
					END;
					state := 26;
				END;
			END;
		END DeclareRecord;

		PROCEDURE DoEndRecord;
		BEGIN
			C2cpTrace.Sub(22);
			pointer := FALSE; basetype := FALSE; dim := 0;
			IF enum = 0 THEN Output('END;') END;
			group := alt$;
			IF recdef THEN
				group := anw[stack]$;
				omitted := ende[stack];
				Pop;
				SkipEol;
				IF s.class = C2cpFiles.cName THEN
					IF name # recname THEN
						StorageClass;
						IF omitted THEN
							C2cpTable.Insert(group, recname, name, '', '', 'T', s.lineNumber);
							C2cpTable.Insert(group, name, '', '', recname, 'T', s.lineNumber);
						ELSE
							C2cpTable.Insert(group, recname, '', '', '', 'T', s.lineNumber);
							OutputName(name, recname);
						END;
					END;
					SkipEol;
					IF s.s = ',' THEN
						SkipEol;
						MemorizeTypeChain;
						varname := name$;
						vartype := 'POINTER TO ' + C2cpTable.GetAlias(C2cpTable.Search(group, recname))$;
						SkipEol;
						OutputName(varname, vartype);
					ELSE
						varname := name$;
						vartype := recname$;
					END;
					C2cpTable.Insert(group, recname, '', vartype, varname, 'T', s.lineNumber);
					C2cpTable.Insert(group, varname, '', vartype, recname, 'T', s.lineNumber);
				END;
			END;
			DEC(record);
			IF record > 0 THEN recname := group$ END;
			vartype := recname$;
			IF enum > 0 THEN LookAhead2 ELSE LookAhead END;
			IF s.nextCh = C2cpStrings.line THEN
				state := 2;
			ELSIF s.nextCh = ';' THEN
				SkipSpace;
				state := 2;
			ELSE
				Output(C2cpStrings.line); Output(Margin(0));
				state := 26;
			END;
			IF record = 0 THEN 
				typedef := FALSE;
				recdef := FALSE;
				recname := '';
			END;
			name := '';
			union := 0;
			enum := 0;
		END DoEndRecord;

		PROCEDURE DeclareVariable;
		BEGIN
			C2cpTrace.Sub(23);
			MemorizeType;
			xarray := 0;
			vcomma := FALSE;
			vardef := TRUE;
			state := 26;
			IF classdef THEN
				name := s.s$;
				class := 't' + alias$;
				C2cpTable.Insert(group, name, '', '', '', 'T', s.lineNumber);
				alt := group$;
				group := name$;
				Output(special);
				Output('TYPE ');
				Output(class);
				Output(' = POINTER TO RECORD');
				Output(C2cpStrings.line); Output(Margin(0));
				Output('END;(*class*)');
				Output(C2cpStrings.line); Output(Margin(0));
				state := 2;
			ELSIF union > 0 THEN
				Output('(*'); Output(C2cpStrings.IntToString(union)); Output('*)');
			ELSIF s.s = 'struct' THEN union := 0; enum := 0; state := 21
			ELSIF s.s = 'union' THEN union := 1; enum := 0; state := 21
			ELSIF s.s = 'enum' THEN union := 0; enum := 1; state := 21
			ELSIF s.s = vartype THEN
				IF s.class = C2cpFiles.cName THEN
					LookAhead2;
					s.s := vartype$;
					s.class := C2cpFiles.cName;
				END;
				IF ~(C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '_') OR (s.nextCh = '*')) THEN
					IF special # '' THEN vartype := 'INTEGER'
					ELSE vartype := 'POINTER' END;
					ListVariable;
				END;
			END;
		END DeclareVariable;

		PROCEDURE (*24*) ListVariableName;
			VAR si: INTEGER;
		BEGIN
			C2cpTrace.Sub(24);
			MemorizeTypeChain;
			ReadParenthesiseVariable;
			varname := name$;
			IF symtype = '' THEN symtype := vartype$ END;
			IF partype = '' THEN partype := symtype$ END;
			LookAhead2;
			IF vcomma & ((partype # symtype) OR (xarray > 0) OR (s.nextCh = '(') OR (s.nextCh = '[')) THEN
				OutputVariableType; Output(C2cpStrings.line); Output(Margin(0));
			END;
			IF s.nextCh = '*' THEN pointer := TRUE; INC(dim); s.Scan END;
			IF s.nextCh = 'O' THEN s.Scan END; (* OF Function *)
			IF s.nextCh = '(' THEN (* 1. Function *)
				DoProcedure;
			ELSE (* Variable *)
				arrayname := alias$;
				arraysign := sign;
				IF vcomma THEN
					IF UseNamedInteger OR (enum = 0) THEN Output(', ') END
				ELSIF (recname = '') THEN
					Output(special);
					CASE sign OF
					| 'C': Output('CONST ');
					| 'T': Output('TYPE ');
					ELSE Output('VAR ');
					END;
				END;
				IF s.nextCh = '[' THEN (* 2. Aarry *)
					SkipSpace;
					LookAhead2;
					ConvertType;
					IF C2cpStrings.SubStr(vartype, 0, 4)$ = 'CHAR' THEN
						IF s.nextCh = ']' THEN vartype := 'ARRAY OF CHAR' END;
					END;
					C2cpTable.Insert(group, varname, '', vartype, '1', sign, s.lineNumber);
					partype := CheckUpType()$;
					Output(alias);
					xarray := C2cpStrings.StringToInt(C2cpTable.GetInfo(C2cpTable.Search(group, varname)));
					IF ~ pointer & (symtype = 'ARRAY OF CHAR') THEN
						SkipSpace;
						IF s.s # ']' THEN
							Output('(* 0..');
							WHILE s.s # ']' DO
								Output(s.s);
								SkipSpace;
							END;
							Output('-1 *)');
						END;
						LookAhead2;
						IF s.nextCh = '[' THEN SkipSpace ELSE s.s := ']' END;
						xarray := 0;
					END;
					IF s.s # ']' THEN
						IF sign = 'T' THEN Output(' = ') ELSE Output(': ') END;
						Output('ARRAY ');
						IF s.nextCh = ']' THEN
							IF xarray = 1 THEN Output('xxx') ELSE Output(C2cpStrings.IntToString(xarray)) END;
						END;
						state := 28;
					END;
				ELSIF s.nextCh = ':' THEN (* 3. Bit *)
					SkipSpace;
					SkipSpace;
					si := ASH(1, s.i) - 1; (* si := (1 SHL s.i) - 1*)
					(* ------------------------- c := ASH(a, b);  equates c := a SHL b; *)
					(* ------------------------- c := ASH(a, -b); equates c := a SHR b; *)
					vartype := '0..' + C2cpStrings.IntToString(si) + ' (* ' + s.s + ' Bits Wide *)';
					C2cpTable.Insert(group, varname, '', vartype, '', sign, s.lineNumber);
					partype := CheckUpType()$;
					Output(alias);
				ELSE (* 4. Rest *)
					ConvertType;
					C2cpTable.Insert(group, varname, '', vartype, '', sign, s.lineNumber);
					partype := CheckUpType()$;
					Output(alias);
					IF ~UseNamedInteger & (enum > 0) THEN
						Output(' = '); Output(C2cpStrings.IntToString(enum)); INC(enum); Output('; ');
					END
				END;
			END;
		END ListVariableName;

		PROCEDURE (*25*) ListInitialiseValuesOfVariable;
		BEGIN
			C2cpTrace.Sub(25);
			sign := C2cpTable.GetSign(C2cpTable.Search(group, varname));
			IF sign # 'C' THEN sign := 'I' END;
			C2cpTable.Update(group, varname, '', '', C2cpStrings.IntToString(xarray), sign, 0);
			xarray := 1;
			IF sign = 'C' THEN
				index := 0;
				Output(' =');
				state := 29;
			ELSIF (sign = 'I')
				OR (sign = 'V') (* A, X, Z are not permitted to change to 'I' ! *)
				OR (sign = 'Z') & (vartype = 'ARRAY OF CHAR')
				OR ~ vbegin (* marker used by vbegin only *)
				THEN (* 'I' = (VAR)CONST ... : ... = ... ; *)
				index := 0; (* do not change the sign to 'I' *)
				Output('; '); Output(C2cpStrings.line); Output(Margin(0));
				LookAhead2;
				IF define OR C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '_') OR (s.nextCh = '(') THEN
					(* new or other functions used - nextCh = A..Z or a..z or ( *)
					StartStatement;
					term := arrayname$ + ' := ';
				ELSE
					(* initialised variable or constant definition - nextCh = 0..9 or { or " or ' *)
					IF s.nextCh # '{' THEN Output(arrayname); Output(' := ') END;
					state := 29;
				END;
			ELSE (* 'U' = VAR ... ; BEGIN ... := ... ; *)
				Output('; (* '); (* not insert into the marker *)
				index := 1; (* from here use the marker .. *)
				Output(alias); Output(' :='); (* insert into marker *)
				state := 29;
			END;
		END ListInitialiseValuesOfVariable;

		PROCEDURE (*26*) ListVariable;
		BEGIN
			C2cpTrace.Sub(26);
			ReadParenthesiseVariable;
			IF s.class = C2cpFiles.cSpace THEN
				blank := s.s$
			ELSIF s.class = C2cpFiles.cEOL THEN
					IF vcomma THEN OutputVariableType END;
					Output(s.s);
			ELSIF s.s = '*' THEN
				(* pointer normally set in Swap *)
				(* only if blanks is between * and variable name then it is set here *)
				pointer := TRUE; INC(dim);
			ELSIF (s.s = ',') & (sign # 'T') THEN
				vcomma := TRUE;
				pointer := FALSE; basetype := FALSE; dim := 0;
			ELSIF (s.s = ',') OR (s.s = ';') OR (s.s = ':=') THEN
				OutputVariableType;
				IF s.s = ',' THEN Output(C2cpStrings.line); Output(Margin(0)) END;
				IF s.s = ';' THEN state := 31 END;
				IF s.s = ':=' THEN state := 2 END;
			ELSIF s.class = C2cpFiles.cName THEN
				ListVariableName;
			ELSIF s.s = '{' THEN
				C2cpTable.Insert(group, varname, '', vartype, '', sign, s.lineNumber);
				Output(' = (');
				state := 27;
			ELSIF (s.s = '}') & (enum > 0) THEN
				IF UseNamedInteger THEN Output(' );') END;
				vcomma := FALSE;
				pointer := FALSE; basetype := FALSE; dim := 0;
				DoEnd;
			ELSE
				Output(s.s);
			END;
			IF s.s = ':=' THEN ListInitialiseValuesOfVariable END;
		END ListVariable;

		PROCEDURE (*27*) DoEnumEnde;
		BEGIN
			C2cpTrace.Sub(27);
			CopyUntil('}', '', ')'); (* enum type *)
		END DoEnumEnde;

		PROCEDURE (*28*) ArrayVariable;
		BEGIN
			C2cpTrace.Sub(28);
			IF s.class # C2cpFiles.cSpace THEN
				IF s.s = ']' THEN
					LookAhead2;
					IF s.nextCh = '[' THEN
						SkipSpace;
						Output(', ');
						LookAhead2;
						IF s.nextCh = ']' THEN
							IF xarray = 1 THEN Output('xxx') ELSE Output(C2cpStrings.IntToString(xarray)) END;
						END;
					ELSE
						xarray := 1;
						state := 26;
					END;
				ELSE
					Output(s.s);
				END;
			END;
		END ArrayVariable;

		PROCEDURE (*29*) InitVariable;
		BEGIN
			C2cpTrace.Sub(29);
			IF s.s = '{' THEN INC(bracket) END;
			IF s.s = '}' THEN DEC(bracket) END;
			IF (bracket = 0) & (s.s = ',') THEN
				IF index # 0 THEN
					marker[1] := marker[1] + ', ';
					index := 0;
					Output(' *)');
				ELSE
					Output(';');
					sign := C2cpTable.GetSign(C2cpTable.Search(group, varname));
					IF sign # 'C' THEN sign := 'I' END;
					C2cpTable.Update(group, varname, '', '', C2cpStrings.IntToString(xarray), sign, 0);
					xarray := 0;
				END;
				state := 30;
			ELSIF s.s = ',' THEN
				IF arraysign = 'C' THEN;
					Output(', ');
				ELSE
					Output('; '); Output(C2cpStrings.line); Output(Margin(0));
					Output(arrayname); Output('['); Output(C2cpStrings.IntToString(xarray)); Output('] := ');
				END;
				INC(xarray);
			ELSIF s.s = ';' THEN
				IF index # 0 THEN
					marker[1] := marker[1] + ', ';
					index := 0;
					Output(' *)');
				ELSE
					Output(';');
					sign := C2cpTable.GetSign(C2cpTable.Search(group, varname));
					IF sign # 'C' THEN sign := 'I' END;
					C2cpTable.Update(group, varname, '', '', C2cpStrings.IntToString(xarray), sign, 0);
					xarray := 0;
				END;
				state := 31;
			ELSE
				IF arraysign = 'C' THEN;
					IF s.s = '{' THEN Output('(')
					ELSIF s.s = '}' THEN Output(')')
					ELSE Output(s.s)
					END;
				ELSE
					IF s.s = '{' THEN
						Output(C2cpStrings.line); Output(Margin(0));
						Output(arrayname); 
						Output('['); Output(C2cpStrings.IntToString(xarray - 1)); Output('] := ');
					ELSIF s.s = '}' THEN (* Output(')') *)
					ELSE Output(s.s)
					END;
				END
			END;
		END InitVariable;

		PROCEDURE (*30*) NewVariableLine;
		BEGIN
			C2cpTrace.Sub(30);
			IF s.class = C2cpFiles.cSpace THEN
				IF s.nextCh # C2cpStrings.line THEN Output(s.s) END;
				state := 30;
			ELSE
				IF s.class # C2cpFiles.cEOL THEN Output(C2cpStrings.line); Output(Margin(0)) END;
				state := 26;
				ListVariable;
			END;
		END NewVariableLine;

		PROCEDURE (*31*) NewBlockLine;
		BEGIN
			C2cpTrace.Sub(31);
			int := TRUE;
			isfile := FALSE;
			ischar := FALSE;
			IF ~recdef THEN typedef := FALSE END;
			special := '';
			IF s.class = C2cpFiles.cSpace THEN
				IF s.nextCh # C2cpStrings.line THEN Output(s.s) END;
				state := 31;
			ELSE
				IF s.class = C2cpFiles.cEOL THEN
					Output(s.s);
					state := 2;
				ELSE
					Output(C2cpStrings.line);
					IF (deepness = 0) & (C2cpStrings.Left(follow, 3)$ = '-*)') THEN
						IF s.s # '{' THEN Output(Margin(0)); Output('--- ') END;
					ELSE
						IF s.s = '}' THEN Output(Margin( - 1)) ELSE Output(Margin(0)) END;
					END;
					anfang := TRUE;
					state := 2;
					IF s.class # C2cpFiles.cSpace THEN Block END;
				END;
			END;
		END NewBlockLine;

		PROCEDURE EndDefine;
		BEGIN
			C2cpTrace.Sub(32);
			EndSammeln;
			IF (s.s = ')') & ~ delBracket THEN Output(s.s) END;
			Output(';');
			anfang := FALSE;
			IF deepness > 0 THEN
				DoEnd;
				IF s.class = C2cpFiles.cSpace THEN Output(s.s) END;
			END;
			Output(C2cpStrings.line); Output(Margin( - 1));
			IF defineConst & defineFunction & (constname$ # '') THEN
				C2cpTable.Update(group, constname, '', '', '', 'F', 0); (* switch constant to function *)
			END;
			endbracket := 0;
			pointer := FALSE; basetype:=FALSE; dim := 0;
			delBracket := FALSE;
			define := FALSE;
			defineConst := FALSE;
			defineFunction := FALSE;
			dummy := FALSE;
			return := FALSE;
			constname := '';
			group := alt$;
			state := 2;
			IF deepness # 0 THEN state := 1 END; (* init variable, because there is a deepness error *)
		END EndDefine;

		PROCEDURE DoBegin;
		BEGIN
			C2cpTrace.Sub(33);
			IF deepness = 0 THEN
				Output(follow);
				follow := '';
				marker[1] := '';
				vbegin := TRUE;
				LookAhead2;
				state := 2;
			ELSE
				IF ende[stack] & (deepness = wert[stack] + 1) THEN
					IF anw[stack] = '' THEN Pop ELSE ende[stack] := FALSE END;
				ELSE
					IF (deepness = wert[stack]) & ((art[stack] = 'R') OR (art[stack] = 'C')) THEN
						IF anfang THEN
							LookAhead2;
							s.s := C2cpStrings.tab;
							s.class := C2cpFiles.cSpace;
							Output(s.s);
						END;
					END;
					INC(deepness);
				END;
				IF anfang THEN state := 2 ELSE state := 31 END;
			END;
		END DoBegin;

		PROCEDURE DoEndBegin;
		BEGIN
			C2cpTrace.Sub(34);
			IF art[stack] = 'C' THEN
				IF (*case-end*)(~ default & (deepness = wert[stack] + 1)) THEN
					IF ~ anfang THEN Output(C2cpStrings.line); Output(Margin(0)) END;
					anfang := TRUE;
					IF ~ isbreak THEN
						INC(labelnr);
						name := 'L' + C2cpStrings.IntToString(labelnr);
						C2cpTable.Insert(group, name, '', '', '', 'L', s.lineNumber);
						labelname := C2cpTable.GetAlias(C2cpTable.Search(group, name))$;
						IF s.s = '}' THEN Output(C2cpStrings.tab) END;
						Output('GOTO '); Output(labelname);
						Output('; (* Warning: no break statement *)' + C2cpStrings.line); Output(Margin(0));
					END;
				END;
				IF deepness = wert[stack] THEN
					DoDefault;
					Pop;
				END;
			END;
			IF deepness = 0 THEN state := 1 ELSE state := 82 END;
			IF ~ anfang THEN Output(C2cpStrings.line); Output(Margin(0)) END;
			IF (art[stack] = 'R') & (deepness = wert[stack]) THEN
				LookAhead; (* END UNTIL *)
			ELSE (* all other END *)
				WHILE ende[stack] & (deepness = wert[stack] + 1) DO
					DEC(deepness);
					Output('END;' + C2cpStrings.line); Output(Margin(0));
					DoDefault;
					Pop;
				END;
				IF define & return THEN (* Return 1 *)
					sign := C2cpTable.GetSign(C2cpTable.Search('', group));
					IF sign = 'F' THEN
						IF varname(*assign*)$ = '' THEN
							C2cpTable.Update('', group, '', '', '', 'P', 0); (* switch function to procedure *)
						ELSE
							Output(Margin(0)); Output('RETURN '); Output(varname(*assign*)); Output(';');
							Output(C2cpStrings.line); Output(Margin(0));
						END;
					END;
				END;
				IF s.class # C2cpFiles.cEOL THEN LookAhead END;
				IF s.nextCh = C2cpStrings.line THEN
					SkipSpace;
					LookAhead;
					s.s := C2cpStrings.line;
					s.class := C2cpFiles.cEOL;
				END;
				IF s.nextCh = 'e' THEN (* coming else ? *)
					s.class := C2cpFiles.cSpace;
					state := 81;
				ELSE (* there is no else *)
					Output('END');
					IF deepness = 0 THEN Output(' '); Output(group) END;
					Output(';');
					IF s.class = C2cpFiles.cEOL THEN Output(s.s) END;
				END;
			END;
		END DoEndBegin;

		PROCEDURE DoEnd;
		BEGIN
			C2cpTrace.Sub(35);
			IF vbegin THEN
				vbegin := FALSE;
				INC(deepness);
				Output('BEGIN' + C2cpStrings.line); Output(Margin( - 1));
			END;
			WHILE ende[stack] & (deepness = wert[stack] + 1) DO
				IF anw[stack] # '' THEN (* END FOR *)
					OutputMarker(anw[stack]);
				END;
				Pop;
				DEC(deepness);
				Output(C2cpStrings.line); Output(Margin(0)); Output('END;');
			END;
			IF (deepness = wert[stack] + 1) & (anw[stack] # '') THEN (* END FOR *)
				OutputMarker(anw[stack]);
				Pop;
			END;
			bracket := 0; (* close open brackets *)
			DEC(deepness);
			IF (deepness < 0) & (class # '') THEN
				deepness := 0;
				Output(C2cpStrings.line);
				Output(Margin(0)); Output('(* END class '); Output(class); Output(' *)');
				Output(C2cpStrings.line);
				class := '';
				state := 1;
			ELSIF (deepness < 0) & (namespace # '') THEN
				deepness := 0;
				Output(Margin(0)); Output('(* END namespace '); Output(namespace); Output(' *)');
				namespace := '';
				state := 1;
			ELSIF recname # '' THEN (* END RECORD *)
				IF deepness < 0 THEN deepness := 0 END; (* fehlerhaftes END korrigieren *)
				DoEndRecord;
			ELSE (* END nach BEGIN *)
				IF deepness < 0 THEN deepness := 0 END; (* fehlerhaftes END korrigieren *)
				DoEndBegin;
			END;
		END DoEnd;

		PROCEDURE DoIf;
		BEGIN
			C2cpTrace.Sub(36);
			LookAhead2;
			IF ~ anfang THEN Output(C2cpStrings.line); Output(Margin(0)) END;
			Push('I', deepness, TRUE);
			term := 'IF ';
			ma := 4;
			follow := ' THEN ';
			state := 77;
		END DoIf;

		PROCEDURE DoElse;
		BEGIN
			C2cpTrace.Sub(37);
			SkipEol;
			Push('I', deepness, TRUE);
			INC(deepness);
			Output('ELSE ');
			NewStatementLine;
		END DoElse;

		PROCEDURE ExQuestionMark;
			VAR bedingung: ARRAY 512 OF CHAR;
		BEGIN
			C2cpTrace.Sub(38);
			Push('?', deepness, TRUE);
			INC(deepness);
			(* *)
			bedingung := term$;
			WHILE C2cpStrings.Pos(':=', bedingung) # - 1 DO 
				bedingung := C2cpStrings.After(bedingung, ':=')$ 
			END;
			WHILE C2cpStrings.Pos(',', bedingung) # - 1 DO 
				bedingung := C2cpStrings.After(bedingung, ',')$ 
			END;
			(* *)
			term := C2cpStrings.Before(term, bedingung)$;
			bedingung := C2cpStrings.Trim(bedingung)$;
			(* *)
			IF define THEN
				IF (C2cpStrings.Left(bedingung, 1)$ = '(') & (bedingung[LEN(bedingung$) - 1] = ')') THEN
					bedingung := C2cpStrings.Trim(C2cpStrings.SubStr(bedingung, 1, LEN(bedingung$) - 2))$;
				END;
			ELSE
				WHILE (C2cpStrings.Left(bedingung, 1)$ = '(') DO
					bedingung := C2cpStrings.Trim(C2cpStrings.SubStr(bedingung, 1, LEN(bedingung$) - 1))$;
				END;
				WHILE (LEN(bedingung$) > 0) & (bedingung[LEN(bedingung$) - 1] = ')') DO
					bedingung := C2cpStrings.Trim(C2cpStrings.SubStr(bedingung, 0, LEN(bedingung$) - 1))$;
				END;
			END;
			(* *)
			IF return & (C2cpStrings.Left(bedingung, 7)$ = 'RETURN ') THEN (* Return 2 *)
				bedingung := C2cpStrings.Right(bedingung, LEN(bedingung$) - 7)$;
				IF bedingung[0] = '(' THEN 
					bedingung := C2cpStrings.Right(bedingung, LEN(bedingung$) - 1)$ 
				END;
				term := 'RETURN ';
			END;
			(* *)
			Output('IF '); Output(bedingung);
			IF relation = '' THEN Output(' # 0') END;
			Output(' THEN'); Output(C2cpStrings.line); Output(Margin(0));
		END ExQuestionMark;

		PROCEDURE ExColon;
		BEGIN
			C2cpTrace.Sub(39);
			IF art[stack] = '?' THEN (* xxx ? xxx : xxx *)
				IF define & (C2cpStrings.Pos(term, ':=') = -1) THEN (* Return 4a *)
					Output('RETURN ');
					return := TRUE; 
				END;
				Output(C2cpStrings.Trim(term)); Output(';'); Output(C2cpStrings.line); Output(Margin( - 1));
				Output('ELSE'); Output(C2cpStrings.line); Output(Margin(0));
				IF return THEN
					IF define THEN term := '' ELSE term := 'RETURN ' END; (* Return 4b *)
					return := FALSE;
				ELSE;
					term := C2cpStrings.Before(term, ':=') + ':=';
				END;
			ELSIF vorne THEN (* Label xxx: *)
				C2cpTable.Insert(group, name, '', '', '', 'L', s.lineNumber);
				EndSammeln;
			ELSE (* : *)
				term := term + s.s;
			END;
		END ExColon;

		PROCEDURE DoFor;
		BEGIN
			C2cpTrace.Sub(40);
			LookAhead2;
			IF ~ anfang THEN Output(C2cpStrings.line); Output(Margin(0)) END;
			IF s.nextCh = '(' THEN
				SkipSpace;
				bracket := 1;
			END;
			Output('(* FOR ');
			Push('F', deepness, TRUE);
			term := '';
			index := 1;
			marker[1] := '';
			marker[2] := '';
			marker[3] := '';
			marker[4] := '';
			follow := 'DO *)';
			state := 79;
		END DoFor;

		PROCEDURE ForStatement;
			VAR sv, sa, sb, sr, si: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(41);
			index := 0;
			marker[1] := C2cpStrings.Trim(C2cpStrings.Before(marker[1], ';'))$;
			marker[2] := C2cpStrings.Trim(C2cpStrings.Before(marker[2], ';'))$;
			marker[3] := C2cpStrings.Trim(C2cpStrings.Before(marker[3], 'DO'))$;
			sv := C2cpStrings.Trim(C2cpStrings.Before(marker[1], ':='))$;
			sa := C2cpStrings.Trim(C2cpStrings.After(marker[1], ':='))$;
			sb := C2cpStrings.Trim(C2cpStrings.After(marker[2], relation))$;
			sr := C2cpStrings.Trim(C2cpStrings.Before(marker[3], '('))$;
			si := C2cpStrings.Trim(C2cpStrings.After(marker[3], sr))$;
			IF (C2cpStrings.After(sa, ':=')$ = '')
				 & (sv = C2cpStrings.Trim(C2cpStrings.Before(marker[2], relation))$)
				 & (relation # '=')
				 & ((sr = 'INC') OR (sr = 'DEC'))
				 & ('(' + sv + ')' = si) THEN
				Output(C2cpStrings.line); Output(Margin(0));
				Output('FOR '); Output(sv); Output(' := '); Output(sa); Output(' TO '); Output(sb);
				IF sr = 'INC' THEN
					IF relation = '<' THEN Output('-1') END;
				ELSE
					IF relation = '>' THEN Output('+1') END;
					Output(' BY -1');
				END;
			ELSE
				OutputMarker(marker[1]);
				OutputMarker(marker[4]);
				IF marker[2] = '' THEN marker[2] := 'TRUE' END;
				Output(C2cpStrings.line); Output(Margin(0));
				Output('WHILE '); Output(marker[2]);
				anw[stack] := marker[3] + ', ' + marker[4];
			END;
			Output(' DO');
		END ForStatement;

		PROCEDURE DoWhile;
		BEGIN
			C2cpTrace.Sub(42);
			LookAhead2;
			IF (art[stack] = 'R') & (deepness = wert[stack]) THEN
				term := 'UNTIL ~(';
				ma := 9;
				Pop;
				follow := ')';
				state := 77;
			ELSE
				IF ~ anfang THEN Output(C2cpStrings.line); Output(Margin(0)) END;
				term := 'WHILE ';
				ma := 7;
				Push('W', deepness, TRUE);
				follow := ' DO ';
				state := 77;
			END;
		END DoWhile;

		PROCEDURE DoRepeat;
		BEGIN
			C2cpTrace.Sub(43);
			IF ~ anfang THEN Output(C2cpStrings.line); Output(Margin(0)) END;
			Output('REPEAT');
			Push('R', deepness, FALSE);
			follow := '';
			state := 83;
		END DoRepeat;

		PROCEDURE DoSwitch;
		BEGIN
			C2cpTrace.Sub(44);
			IF ~ anfang THEN Output(C2cpStrings.line); Output(Margin(0)) END;
			Push('C', deepness, FALSE);
			LookAhead2;
			IF s.nextCh = '(' THEN
				s.Scan;
				INC(bracket);
			END;
			term := 'CASE ';
			follow := ' OF ';
			isbreak := TRUE;
			state := 76;
		END DoSwitch;

		PROCEDURE DoCase;
			VAR merke: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(45);
			merke := s.s$;
			ende[stack] := FALSE;
			IF deepness > wert[stack] + 1 THEN (* if there was no END *)
				default := FALSE;
				DoEnd;
				Output(C2cpStrings.line); Output(Margin(0));
			END;
			(* *)
			IF merke = 'case' THEN (* s.s = 'case' *)
				Output('| ');
				LookAhead2; (* Skip over blank *)
				WHILE s.s # ':' DO
					s.Scan;
					Look;
					Output(s.s);
				END;
				default := FALSE;
			ELSE (* s.s = 'default' *)
				WHILE s.s # ':' DO s.Scan END;
				Output('ELSE');
				default := TRUE;
			END;
			(* *)
			LookAhead2;
			IF s.nextCh = '{' THEN
				SkipSpace;
				ende[stack] := FALSE;
			ELSE
				ende[stack] := TRUE;
			END;
			INC(deepness);
			IF ~ isbreak THEN
				name := 'L' + C2cpStrings.IntToString(labelnr);
				C2cpTable.Insert(group, name, '', '', '', 'L', s.lineNumber);
				labelname := C2cpTable.GetAlias(C2cpTable.Search(group, name))$;
				Output(C2cpStrings.line); Output(Margin(0)); Output(labelname); Output(': ');
			END;
			Output(' ');
			isbreak := FALSE;
			state := 83;
		END DoCase;

		PROCEDURE DoBreak;
		BEGIN
			C2cpTrace.Sub(46);
			IF (art[stack] = 'C') & (deepness = wert[stack] + 2) THEN
				IF ~ anfang THEN Output(C2cpStrings.line); Output(Margin(0)) END;
				Output('(* BREAK *)');
				isbreak := TRUE;
				IF ende[stack] THEN
					ende[stack] := FALSE;
					DEC(deepness);
				END;
				LookAhead2;
				IF s.nextCh = ';' THEN s.Scan END;
				state := 83;
			ELSE
				Output('EXIT');
			END;
		END DoBreak;

		PROCEDURE DoGoto;
		BEGIN
			C2cpTrace.Sub(47);
			SkipSpace;
			Output('GOTO '); Output(s.s);
			C2cpTable.Insert(group, name, '', '', '', 'L', s.lineNumber);
			IF (art[stack] = 'C') & (deepness = wert[stack] + 2) THEN
				isbreak := TRUE;
				isgoto := TRUE;
			END;
		END DoGoto;

		PROCEDURE DoHalt;
		BEGIN
			C2cpTrace.Sub(48);
			Output('HALT ');
			LookAhead2;
			IF (art[stack] = 'C') & (deepness = wert[stack] + 2) THEN
				isbreak := TRUE;
				isgoto := TRUE;
			END;
		END DoHalt;

		PROCEDURE DoInOut (z1, z2: INTEGER; a2: ARRAY OF CHAR);
		BEGIN
			C2cpTrace.Sub(49);
			ln := FALSE;
			IF C2cpTable.GetAlias(C2cpTable.Search('', 'FILE'))$ = 'TEXTFILE' THEN
				Output(s.s); Output(' '); (* C orientated *)
				bound := 0; (* scanf -> scanf & printf -> printf *)
				IF s.s[0] = 'f' THEN parameterCount := 2 ELSE parameterCount := 1 END;
				LookAhead2;
				state := z1;
			ELSE (* Pascal orientated *)
				Output(a2); (* scanf -> READ & printf -> StdLog. *)
				state := z2;
			END;
		END DoInOut;

		PROCEDURE ExClassNew;
		BEGIN
			C2cpTrace.Sub(50);
			term := term + 'NEW';
		END ExClassNew;

		PROCEDURE ExNew (hinweis: ARRAY OF CHAR);
		BEGIN
			C2cpTrace.Sub(51);
			LookAhead2;
			IF s.nextCh = '(' THEN
				SkipSpace;
				bound := bracket;
				INC(bracket);
				IF mi = 0 THEN
					alloc := C2cpStrings.Trim(C2cpStrings.Before(term, ':='))$;
					term := '';
				ELSE
					IF term[ma - 1] = '(' THEN
						bound := bound - 1;
						alloc := C2cpStrings.SubStr(term, ma, mi - ma - 1)$;
					ELSE
						alloc := C2cpStrings.SubStr(term, ma - 1, mi - ma)$;
					END;
					term := C2cpStrings.SubStr(term, 0, ma - 1)$;
				END;
				term := term + 'NEW' + hinweis + '(' + alloc + ', ';
				IF mi # 0 THEN mi := LEN(term$) END;
				LookAhead2;
			END;
		END ExNew;

		PROCEDURE ExFree;
		BEGIN
			C2cpTrace.Sub(52);
			alloc := 'FreeMem';
			bound := 1;
		END ExFree;

		PROCEDURE DoString;
			VAR
				op, merke: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(53);
			IF C2cpTable.GetAlias(C2cpTable.Search('', 'strlen'))$ = 'Length' THEN
				op := C2cpStrings.Right(s.s, 3)$; (* Pascal orientated *)
				SkipSpace;
				IF s.s = '(' THEN
					INC(bracket);
					SkipSpace;
				END;
				merke := '';
				WHILE s.s # ',' DO
					merke := merke + s.s;
					SkipSpace;
				END;
				Output(merke); Output(' := ');
				IF op = 'cat' THEN Output(merke); Output(' + ') END;
				SkipSpace;
				WHILE (s.s # ',') & (s.s # ')') DO
					Expression;
					SkipSpace;
				END;
				IF s.s = ',' THEN
					Output('Left('); Output(term); Output(', ');
				ELSE
					Output(term);
					DEC(bracket);
				END;
				term := '';
			ELSE (* C orientated *)
				state := 79;
				Expression;
			END;
		END DoString;

		PROCEDURE DoStringCopy;
		BEGIN
			C2cpTrace.Sub(54);
			IF C2cpTable.GetAlias(C2cpTable.Search('', 'strlen'))$ = 'Length' THEN
				Output(s.s); (* Pascal orientated *)
				SkipSpace;
				Output(' '); Output(s.s);
				SkipSpace;
				Output(s.s); Output(', ');
				SkipSpace;
				IF s.s = ',' THEN Output('+0') END;
				Output(s.s);
				IF s.s # ',' THEN LookAhead2 END;
			ELSE (* C orientated *)
				state := 79;
				Expression;
			END;
		END DoStringCopy;

		PROCEDURE DoIntr;
			VAR merke: ARRAY 256 OF CHAR; ch: ARRAY 2 OF CHAR;
		BEGIN
			C2cpTrace.Sub(55);
			merke := '';
			IF s.s = 'Intr' THEN ch := ',' ELSE ch := '(' END;
			Output(s.s); Output(' ');
			WHILE s.s # ch DO
				SkipSpace;
				Output(s.s);
			END;
			WHILE s.s # ')' DO
				IF (s.class = C2cpFiles.cName) & (alias # merke) THEN
					IF merke = '' THEN
						Output(s.s);
						merke := alias$; (* must be alias and not s.s *)
					ELSE
						C2cpTable.Update(group, name, merke, 'Registers', '', 'V', 0);
					END;
				END;
				SkipSpace;
			END;
			Output(s.s);
			LookAhead2;
		END DoIntr;

		PROCEDURE ExKey;
			VAR wort: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(56);
			C2cpTable.Insert('3', 'conio', 'Terminal', '', '', 'M', 1);
			LookAhead2; IF s.nextCh = '(' THEN SkipSpace END;
			SkipSpace; wort := s.s$;
			LookAhead2; IF s.nextCh = ')' THEN SkipSpace END;
			(* *)
			IF wort = '1' THEN (* bioskey(1) *)
				LookAhead2;
				IF (s.nextCh = '!') OR (s.nextCh = '=') THEN
					SkipSpace;
					wort := s.s$;
					SkipSpace;
				END;
				IF wort = '=' THEN term := term + '~' END; (* ==  = 0  not .. *)
				term := term + 'KeyPressed'; (* !=  # 0  key pressed *)
				relation := '#';
			ELSE
				IF (term = '') & (state = 79) THEN
					term := 'dummy := ';
					varname(*assign*) := 'dummy';
					C2cpTable.Insert(group, 'dummy', '', 'INTEGER', '', 'V', s.lineNumber);
				END;
				term := term + 'ORD(ReadKey)'; (* bioskey(0) *)
			END;
		END ExKey;

		PROCEDURE ExParamStr;
		BEGIN
			C2cpTrace.Sub(57);
			term := term + s.s;
			LookAhead2;
			IF s.nextCh = '[' THEN
				SkipSpace;
				term := term + '(';
				SkipSpace;
				WHILE s.s # ']' DO
					term := term + s.s;
					SkipSpace;
				END;
				term := term + ')';
			END;
		END ExParamStr;

		PROCEDURE ExReturn;
			VAR i: INTEGER;
		BEGIN
			C2cpTrace.Sub(58);
			LookAhead2;
			IF s.nextCh = ';' THEN
				SkipSpace;
				Output('RETURN;');
				state := 82;
			ELSE
				name := group$;
				i := C2cpTable.Search('', name);
				alias := C2cpTable.GetAlias(i)$;
				symtype := C2cpTable.GetType(i)$;
				sign := C2cpTable.GetSign(i);
				IF sign = 'P' THEN
					IF symtype = '' THEN symtype := 'INTEGER' END;
					C2cpTable.Update('', name, '', symtype, '', 'F', 0);
				END;
				IF ~ anfang THEN Output(C2cpStrings.line); Output(Margin(0)) END;
				term := 'RETURN ';
				vorne := FALSE;
				return := TRUE;
				state := 79;
				IF s.nextCh = '(' THEN
					SkipSpace;
					bracket := 1;
					delBracket := TRUE;
				END;
			END;
			IF (art[stack] = 'C') & (deepness = wert[stack] + 2) THEN
				isbreak := TRUE;
				isgoto := TRUE;
			END;
		END ExReturn;

		PROCEDURE ExPort;
		BEGIN
			C2cpTrace.Sub(59);
			term := term + 'Port';
			LookAhead2;
			IF s.nextCh = '(' THEN
				SkipSpace;
				term := term + '[';
				SkipSpace;
				WHILE (s.s # ')') & (s.s # ',') DO
					term := term + s.s;
					SkipSpace;
				END;
				term := term + ']';
				IF s.s = ',' THEN
					term := term + ' := ';
					INC(bracket);
					delBracket := TRUE;
				END;
			END;
		END ExPort;

		PROCEDURE DoRandomize;
		BEGIN
			C2cpTrace.Sub(60);
			Output(s.s); Output('; (* ');
			SkipSpace;
			WHILE s.s # ';' DO
				Output(s.s);
				SkipSpace;
			END;
			Output(' *)');
		END DoRandomize;

		PROCEDURE ExRandom;
		BEGIN
			C2cpTrace.Sub(61);
			term := term + s.s;
			LookAhead2;
			IF s.nextCh = '(' THEN
				SkipSpace;
				term := term + s.s;
				LookAhead2;
				IF s.nextCh = ')' THEN
					SkipSpace;
					term := term + '$7FFF' + s.s;
				ELSE
					INC(bracket);
				END;
			END;
		END ExRandom;

		PROCEDURE ExAssign;
			VAR token: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(62);
			token := s.s$;
			IF (s.s = '+') OR (s.s = '-') OR
				(s.s = '*') OR (s.s = '/') OR
				(s.s = (*/*) ' DIV ') OR (s.s = (*%*) ' MOD ') OR
				(s.s = (*&*) ' & ') OR (s.s = (*|*) ' OR ') OR
				(s.s = (*^*) ' XOR ') OR (s.s = (*!*) ' ~ ') OR
				(s.s = (*<<*)' SHL ') OR (s.s = (*>>*)' SHR ') THEN
				s.Scan; (* fetch '=' *)
				IF define & (s.s = '=') THEN defineFunction := TRUE END; 
				varname(*assign*) := C2cpStrings.Trim(C2cpStrings.Before(term, ':='))$;
				IF state = 78 THEN
					term := term + ':= ' + alias + token;
				ELSE
					term := term + ':= ' + term + token;
				END;
			ELSE
				term := term + token;
			END;
		END ExAssign;

		PROCEDURE LastTerm (): C2cpStrings.String; (* for e.g.: i++ or i-- *)
			VAR
				i: INTEGER;
				open: INTEGER;
				teil: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(63);
			open := 0;
			teil := term$;
			i := LEN(teil$) - 1;
			WHILE (i >= 0) & ((open > 0)
			OR C2cpStrings.IsAlpha(teil[i]) OR (teil[i] = '_') 
			OR C2cpStrings.IsDigit(teil[i])
			OR (teil[i] = '.') OR (teil[i] = '^') OR (teil[i] = ')') OR (teil[i] = ']') OR (teil[i] = '_')) DO
				IF (teil[i] = '(') OR (teil[i] = '[') THEN DEC(open) END;
				IF (teil[i] = ')') OR (teil[i] = ']') THEN INC(open) END;
				i := i - 1;
			END;
			RETURN C2cpStrings.Trim(C2cpStrings.Right(teil, LEN(teil$) - i - 1));
		END LastTerm;

		PROCEDURE NextTerm (): C2cpStrings.String; (* for e.g.: ++i or --i *)
			VAR
				open: INTEGER;
				teil: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(64);
			open := 0;
			teil := '';
			REPEAT
				s.Scan;
				Look;
				IF (s.s = '(') OR (s.s = '[') THEN INC(open) END;
				IF (s.s = ')') OR (s.s = ']') THEN DEC(open) END;
				teil := teil + s.s;
			UNTIL (open = 0) & ~(C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '_'))
			 & (s.nextCh # '[') & (s.nextCh # '.')
			 & (s.nextCh # '^') & (s.nextCh # '-');
			RETURN C2cpStrings.Trim(teil);
		END NextTerm;

		PROCEDURE ExDoppelt;
			VAR
				op, opop, teil, plus: ARRAY 256 OF CHAR;
				first: BOOLEAN;
		BEGIN
			C2cpTrace.Sub(65);
			op := s.s$;
			opop := '(*' + op + op + '*)';
			s.Scan; (* fetch second plus or minus character *)
			IF C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '_') OR (s.nextCh = '(') THEN (* ++i or --i *)
					first := TRUE;
				teil := NextTerm()$;
				term := term + opop + teil;
			ELSE (* i++ or i-- *)
				first := FALSE;
				teil := LastTerm()$;
				term := term + opop;
			END;
			IF C2cpStrings.Left(teil, 1)$ = '*' THEN teil := C2cpStrings.Right(teil, LEN(teil$) - 1)$ END;
			IF C2cpStrings.Right(teil, 1)$ = '^' THEN teil := C2cpStrings.Left(teil, LEN(teil$) - 1)$ END;
			IF ((sign = 'X') OR (sign = 'Z')) & (symtype # 'ARRAY OF CHAR') THEN
				IF C2cpStrings.Left(symtype, 11)$ # 'POINTER TO ' THEN
					symtype := C2cpTable.GetType(C2cpTable.Search(group, symtype))$
				END;
				IF first THEN
					plus := opop + teil
				ELSE
					plus := teil + opop
				END;
				plus := plus + ' := ' + teil;
				IF op = '-' THEN
					plus := plus + '.previous'
				ELSE
					plus := plus + '.next'
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
					outcome := outcome + term;
					marker[index] := marker[index] + plus
				ELSE
					Output(plus);
				END;
				term := '';
			ELSE
				IF first THEN
					IF index > 0 THEN
						marker[index] := marker[index] + plus + ', ';
					ELSE
						IF C2cpStrings.Left(term, 6)$ = 'UNTIL ' THEN Output(C2cpStrings.tab) END;
						Output(plus); Output('; ');
					END;
					IF (C2cpStrings.Left(term, 3)$ = 'IF ')
						OR (C2cpStrings.Left(term, 6)$ = 'WHILE ')
						OR (C2cpStrings.Left(term, 6)$ = 'UNTIL ') THEN
						Output(C2cpStrings.line); Output(Margin(0));
						IF (C2cpStrings.Left(term, 6)$ = 'WHILE ') THEN AddMarker(anw[stack], plus) END;
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
			C2cpTrace.Sub(66);
			vorne := TRUE;
			dummy := FALSE;
			closed := 0;
			term := '';
			plusplus := '';
			IF index = 0 THEN relation := '' END;
		END StartSammeln;

		PROCEDURE EndSammeln;
			VAR vari, expr: ARRAY 512 OF CHAR; equal: BOOLEAN;
		BEGIN
			C2cpTrace.Sub(67);
			IF index = 2 THEN
				marker[4] := marker[4] + marker[2];
				marker[2] := '';
			END;
			ma := 0;
			mi := 0;
			mk := 0;
			isfile := FALSE;
			ischar := FALSE;
			(* muß der term zerlegt werden? *)
			term := C2cpStrings.Trim(term)$;
			expr := term$;
			WHILE expr # '' DO vari := C2cpStrings.Parse(expr, ':=')$ END;
			IF (term$ # '') & (vari$ = term$) OR (state = 78) THEN
				(* no breakdown of term, because there is no ':=' inside the term -> copy term *)
				IF define & ~defineConst & (s.class = C2cpFiles.cEOL) THEN (* Return 3 *)
					sign := C2cpTable.GetSign(C2cpTable.Search('', group));
					IF sign = 'F' THEN Output('RETURN ') END;
					return := FALSE; (* return is done here *)
				END;
				Output(term);
				term := "";
			ELSE
				(* forward breakdown term, because there exist one or more ':=' *)
				expr := C2cpStrings.Trim(vari)$;
				equal := (expr = '-1') OR (expr = '0') OR (expr = '1'); (* TRUE = there is no side effects *)
				REPEAT
					vari := C2cpStrings.Trim(C2cpStrings.Parse(term, ':='))$;
					IF term # '' THEN
						Output(vari + ' := ' + expr);
						IF C2cpStrings.Pos(':=', term) # - 1 THEN Output('; ') END; ;
						IF ~equal THEN expr := vari END;
					END;
				UNTIL term = '';
			END;
			IF plusplus # '' THEN
				IF index > 0 THEN marker[index] := marker[index] + plusplus
				ELSE Output(plusplus) END;
			END;
			IF s.s = ',' THEN
				IF (index # 0) OR (bracket # endbracket) THEN
					Output(', ')
				ELSE
					Output('; '); Output(C2cpStrings.line); Output(Margin(0))
				END
			ELSIF s.s = ':' THEN Output(': ')
			ELSIF s.s = ';' THEN Output('; ')
			ELSIF s.s = 'else' THEN Output('; ')
			END;
			IF s.s = ';' THEN
				return := FALSE;
				IF index > 0 THEN INC(index) END;
				IF define THEN
					LookAhead;
					IF s.class = C2cpFiles.cEOL THEN Output(C2cpStrings.line) END;
				END;
			END;
			IF index > 0 THEN
				LookAhead2;
				Output(' ');
			END;
			state := 82;
		END EndSammeln;

		PROCEDURE TermAufteilen;
			VAR i: INTEGER; teil: ARRAY 256 OF CHAR; termleft, termright: C2cpStrings.String;
		BEGIN
			C2cpTrace.Sub(68);
			IF (ma > 0) & (ma < mi) THEN
				teil := C2cpStrings.SubStr(term, ma - 1, LEN(term$) - ma + 1)$;
				IF teil[0] = '(' THEN (* '(' at the beginning *)
					teil := C2cpStrings.After(teil, '(')$;
					FOR i := 1 TO mk - 1 DO teil := C2cpStrings.After(teil, '(')$ END;
					IF (C2cpStrings.Left(teil, 1)$ = '(') & (C2cpStrings.Right(teil, 1)$ = ')') THEN
						teil := C2cpStrings.SubStr(teil, 1, LEN(teil$) - 2)$;
					END;
					IF C2cpStrings.Pos(':=', teil) = - 1 THEN
						termleft := C2cpStrings.SubStr(term, 0, ma - 1); (* 'P' *)
						termright := C2cpStrings.After(term, '('); (* 'P' *)
						term := termleft + termright; (* 'P' *)
					ELSE
						termleft := C2cpStrings.SubStr(term, 0, ma - 1); (* 'F' *)
						termright := C2cpStrings.SubStr(term, ma, mi - ma - 1); (* 'F' *)
						term := termleft + termright; (* 'F' *)
					END;
				ELSIF C2cpStrings.Pos('(', teil) = - 1 THEN (* no '(' and ':=' only *)
					term := C2cpStrings.SubStr(term, 0, mi - 1)$;
				ELSIF C2cpStrings.Pos(':=', teil) = - 1 THEN (* no ':=' *)
					termleft := C2cpStrings.SubStr(term, 0, ma - 1);
					termright := C2cpStrings.After(C2cpStrings.SubStr(term, ma, mi - ma - 1), '(');
					term := termleft + termright;
				ELSIF C2cpStrings.Pos(':=', teil) < C2cpStrings.Pos('(', teil) THEN (* ':=' in front of '(' *)
					term := C2cpStrings.SubStr(term, 0, mi - 1)$;
				ELSE (* '(' in front of ':=' *)
					IF C2cpStrings.Pos('(', teil) # C2cpStrings.Pos('(*', teil) THEN (* if not '( *' *)
						teil := C2cpStrings.After(teil, '(')$;
					END;
					term := C2cpStrings.Trim(C2cpStrings.SubStr(term, 0, mi - 1)) + s.s;
				END;
				IF C2cpStrings.Left(term, 6)$ = 'UNTIL ' THEN Output(C2cpStrings.tab) END;
				Output(C2cpStrings.Trim(teil));
				IF C2cpStrings.Left(term, 6)$ = 'WHILE ' THEN AddMarker(anw[stack], teil) END;
				IF plusplus # '' THEN
					plusplus := C2cpStrings.Trim(C2cpStrings.After(plusplus, ';'))$;
					Output(';'); Output(C2cpStrings.line); Output(Margin(0));
					IF C2cpStrings.Left(term, 6)$ = 'UNTIL ' THEN Output(C2cpStrings.tab) END;
					Output(plusplus);
					IF C2cpStrings.Left(term, 6)$ = 'WHILE ' THEN AddMarker(anw[stack], plusplus) END;
					plusplus := '';
				END;
				IF index = 0 THEN Output(';' + C2cpStrings.line); Output(Margin(0)) ELSE Output(', ') END;
				mi := 0;
			END
		END TermAufteilen;

		PROCEDURE ExBracket;
			VAR info: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(69);
			IF (bracket = 0) & (term # '') THEN
				vorne := FALSE;
			END;
			IF (s.s = '(') & (s.nextCh = ')') THEN (* () read over *)
				SkipSpace;
			ELSE
				IF s.s = '(' THEN INC(bracket) END;
				IF s.s = ')' THEN DEC(bracket) END;
				IF bracket < 0 THEN bracket := 0 END;
				IF (bracket <= mk) & (ma > 0) & (ma < mi) THEN
					TermAufteilen;
				ELSIF (bracket = 0) & (follow # '') THEN
					EndSammeln; (* end bracket *)
					IF (art[stack] = '?') & (deepness = wert[stack] + 1) THEN
						Pop; (* ((a==0) ? b : c) *)
						DEC(deepness);
						Output(';' + C2cpStrings.line); Output(Margin(0)); Output('END');
					END;
					Output(follow);
					addBracket := FALSE;
					delBracket := FALSE;
					IF (C2cpStrings.Pos('DO', follow) # - 1) OR (C2cpStrings.Pos('THEN', follow) # - 1) THEN
						IF index > 0 THEN ForStatement END;
						INC(deepness);
						state := 83;
					ELSE
						state := 75;
					END;
					term := '';
					plusplus := '';
					follow := '';
				ELSIF (bracket = 0) & define & defineConst & (s.nextCh # ';') THEN
					EndSammeln;
				ELSIF (bracket = 0) & return & delBracket THEN
					(* not copy the last ')' *)
					delBracket := FALSE;
				ELSE
					IF (alloc # '') & (s.s = ')') & (bracket <= bound + mk) THEN
						IF alloc = 'FreeMem' THEN
							alloc := C2cpStrings.Trim(C2cpStrings.After(term, '('))$;
							IF C2cpTable.Search(group, alloc) = 0 THEN
								C2cpTable.Insert(group, alloc, '', 'POINTER', '???', 'X', s.lineNumber);
							END;
							term := alloc + ' := NIL';
							s.s := ''; (* ignore ')' *)
						ELSE
							info := C2cpStrings.Trim(C2cpStrings.After(term, ','))$;
							IF C2cpTable.Search(group, alloc) = 0 THEN
								C2cpTable.Insert(group, alloc, '', 'POINTER', info, 'X', s.lineNumber);
							ELSE
								IF C2cpTable.GetType(C2cpTable.Search(group, alloc))$ = 'ARRAY OF CHAR' THEN
									vartype := 'POINTER';
									sign := 'X';
								ELSE (* not change *)
									vartype := '';
									sign := ' ';
								END;
								C2cpTable.Update(group, alloc, '', vartype, info, sign, 0);
							END;
						END;
						alloc := '';
					END;
					IF (s.s = '(') & (state = 79) & (sign # 'F') & (sign # 'P') THEN
						state := 80 (* do type cast test *)
					ELSE (* no type cast test *)
						sign := ' ';
						IF s.s = ')' THEN
							IF delBracket & ((bracket = 0) OR (state = 78)) THEN
								delBracket := FALSE
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
		END ExBracket;

		PROCEDURE ConvertToFunction;
		BEGIN
			C2cpTrace.Sub(70);
			IF (s.class = C2cpFiles.cName) & (sign = 'F') & (term = '') & (bracket = 0) & (state = 79) THEN
				term := 'dummy := ';
				varname(*assign*) := 'dummy';
				C2cpTable.Insert(group, 'dummy', '', symtype, '', 'V', s.lineNumber);
				dummy := TRUE;
			END;
		END ConvertToFunction;

		PROCEDURE ConvertToProcedure;
			VAR termleft, termright: C2cpStrings.String;
		BEGIN
			C2cpTrace.Sub(71);
			IF (s.class = C2cpFiles.cName) & (sign = 'P')
			& (C2cpStrings.Pos(':=', term) # - 1) 
			& (C2cpStrings.Trim(C2cpStrings.After(term, ':='))$ = '') THEN
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
					SkipSpace;
					LookAhead2;
					IF s.nextCh = ')' THEN
						SkipSpace;
					ELSE
						s.s := ',';
						INC(bracket);
					END;
				ELSE
					s.s := ')';
				END;
			END;
		END ConvertToProcedure;

		PROCEDURE StartAsm;
		BEGIN
			C2cpTrace.Sub(72);
			Output('ASM'); Output(C2cpStrings.line); Output(Margin(1));
			LookAhead2;
			state := 73;
		END StartAsm;

		PROCEDURE (*73*) DoAsm;
		BEGIN
			C2cpTrace.Sub(73);
			IF s.class = C2cpFiles.cEOL THEN (* end of ASM statement *)
				Output(s.s);
				state := 74;
			ELSE
				IF s.nextCh = '/' THEN
					Output(s.s);
					s.Scan;
					IF s.nextCh = '*' THEN (* comment found *)
						s.Scan;
						Output('(* /*');
						state := 3;
						back := 73;
					ELSIF s.nextCh = '/' THEN (* comment found *)
						s.Scan;
						Output('(* //');
						state := 4;
						back := 73;
					ELSE
						Look;
						Output(s.s);
					END;
				ELSE
					Output(s.s);
				END;
			END;
		END DoAsm;

		PROCEDURE (*74*) EndAsm;
		BEGIN
			C2cpTrace.Sub(74);
			IF s.s = 'asm' THEN
				LookAhead2;
				Output(C2cpStrings.tab);
				state := 73;
			ELSE
				IF s.s = '}' THEN
					Output(C2cpStrings.tab); Output('END;'); Output(C2cpStrings.line); Output(Margin( - 1))
				ELSE
					Output('END;'); Output(C2cpStrings.line); Output(Margin(0))
				END;
				StartStatement;
			END;
		END EndAsm;

		PROCEDURE (*75*) StartStatement;
			VAR i: INTEGER;
		BEGIN
			C2cpTrace.Sub(75);
			IF vbegin THEN
				i := C2cpTable.Search(group, 'dummy');
				IF i > 0 THEN
					alias := C2cpTable.GetAlias(i)$;
					Output('VAR '); Output(alias); Output(': '); Output(C2cpTable.GetType(i)); Output(';');
					Output(C2cpStrings.line); Output(Margin(0));
				END;
				vbegin := FALSE;
				anfang := FALSE;
				INC(deepness);
				Output('BEGIN');
				OutputMarker(marker[1]);
				Output(C2cpStrings.line); Output(Margin(0));
				anfang := TRUE;
			END;
			StartSammeln;
			IF index = 2 THEN (* second statement in FOR *)
				state := 77;
				StartCondition;
			ELSE (* normal statement *)
				state := 76;
				Statement;
			END;
		END StartStatement;

		PROCEDURE (*76*) Statement;
		BEGIN
			C2cpTrace.Sub(76);
			BlockStatement;
		END Statement;

		PROCEDURE (*77*) StartCondition;
		BEGIN
			C2cpTrace.Sub(77);
			IF s.class # C2cpFiles.cSpace THEN
				IF (index = 0) & (s.s = '(') THEN
					IF (C2cpStrings.Pos('THEN', follow) = - 1) (* if not THEN *)
						 & (C2cpStrings.Pos('DO', follow) = - 1) (* and not DO *)
						 & (follow # ')') THEN (* and not UNTIL *)
						term := term + s.s;
					END;
					INC(bracket);
					SkipSpace;
				ELSE
					ma := 1;
				END;
				bound := 1;
				vorne := FALSE; (* ++ & -- can not stay by its own inside condition *)
				addBracket := FALSE;
				delBracket := FALSE;
				relation := '';
				state := 78;
				Condition;
			END;
		END StartCondition;

		PROCEDURE (*78*) Condition;
			VAR termleft, termright: C2cpStrings.String;
		BEGIN
			C2cpTrace.Sub(78);
			IF s.class # C2cpFiles.cSpace THEN
				IF s.s = '(' THEN
					ExBracket;
				ELSIF (s.s = ')') OR (s.s = ';') OR (s.s = ',') & (bracket <= bound) THEN
					IF bracket <= bound THEN
						TermAufteilen;
						IF ~ bitwise & (relation = '') THEN
							IF term # '' THEN term := term + ' # 0' END;
							relation := '#';
						END;
						IF addBracket THEN
							term := term + ')';
							addBracket := FALSE;
						END;
						IF bitwise & (relation = '') THEN
							IF term # '' THEN term := term + ' # 0' END;
							relation := '#';
						END;
					END;
					IF s.s = ')' THEN ExBracket
					ELSIF s.s = ';' THEN EndSammeln
					ELSE
						term := term + ', ';
						ma := LEN(term$) + 1;
						relation := '';
					END;
				ELSIF s.class = C2cpFiles.cEOL THEN
					Output(C2cpStrings.Trim(term)); Output(s.s);
					term := '';
					ma := 0;
				ELSIF s.s = '~' THEN
					term := term + s.s;
					IF ~ bitwise THEN
						LookAhead2;
						IF s.nextCh = '(' THEN
							s.Scan;
							ExBracket;
						ELSE
							term := term + '(';
							addBracket := TRUE;
						END;
						bound := bracket;
					END;
					ma := LEN(term$) + 1;
				ELSIF (s.s = ' & ') OR (s.s = ' OR ') OR (s.s = ' XOR ') THEN
					IF ~ addBracket & (ma > 0) & (term # '') & (term[ma - 1] # '(') THEN
						termleft := C2cpStrings.Left(term, ma - 1);
						termright := C2cpStrings.Right(term, LEN(term$) - ma + 1);
						term := termleft + '(' + termright;
						addBracket := TRUE;
					END;
					IF bitwise THEN
						term := term + s.s;
					ELSE
						IF relation = '' THEN term := term + ' # 0' END;
						IF addBracket THEN
							term := term + ')';
							addBracket := FALSE;
						END;
						IF (term # '') & (term[LEN(term$) - 1] # ')') THEN
							term := term + ')';
							delBracket := TRUE;
						END;
						term := term + s.s;
						LookAhead2;
						IF mi = 0 THEN ma := LEN(term$) + 1 END;
						IF (s.nextCh # '(') & (s.nextCh # '!' (*'NOT'*)) THEN
							term := term + '(';
							addBracket := TRUE;
						END;
					END;
					relation := '';
				ELSE
					Expression;
				END;
			END;
		END Condition;

		PROCEDURE (*79*) Expression;
		BEGIN
			C2cpTrace.Sub(79);
			IF s.class = C2cpFiles.cName THEN
				ConvertToFunction;
				IF symtype = 'BOOLEAN' THEN relation := '#' END;
				IF s.s = 'sizeof' THEN
					C2cpTable.Insert('', s.s, 'SIZE', 'INTEGER', '', 'F', s.lineNumber);
				END;
			ELSE
				IF s.s = '[' THEN INC(bracket) END;
				IF s.s = ']' THEN DEC(bracket) END;
				IF bracket < 0 THEN bracket := 0 END;
			END;
			IF s.s = '(' THEN ExBracket
			ELSIF s.s = ')' THEN ExBracket
			ELSIF s.s = 'else' THEN EndSammeln; EndStatement;
			ELSIF s.s = '}' THEN EndSammeln; DoEnd;
			ELSIF s.s = ';' THEN EndSammeln
			ELSIF s.s = '?' THEN ExQuestionMark
			ELSIF s.s = ':' THEN ExColon
			ELSIF s.s = 'new' THEN ExClassNew
			ELSIF s.s = 'malloc' THEN ExNew('')
			ELSIF s.s = 'calloc' THEN ExNew('(*0*)')
			ELSIF s.s = 'realloc' THEN ExNew('(*r*)')
			ELSIF s.s = 'free' THEN ExFree
			ELSIF s.s = 'struct' THEN term := term + '(*' + s.s + '*)'
			ELSIF s.s = 'bioskey' THEN ExKey
			ELSIF s.s = 'ParamStr' THEN ExParamStr
			ELSIF s.s = 'inportb' THEN ExPort
			ELSIF s.s = 'outportb' THEN ExPort
			ELSIF s.s = 'return' THEN ExReturn
			ELSIF s.s = 'Random' THEN ExRandom
			ELSIF define & (s.class = C2cpFiles.cEOL) THEN EndDefine
			ELSIF ~ define & (s.class = C2cpFiles.cEOL) THEN s.class := C2cpFiles.cSpace
			ELSIF s.nextCh = '=' THEN ExAssign
			ELSIF (s.s = '+') & (s.nextCh = '+') THEN ExDoppelt
			ELSIF (s.s = '-') & (s.nextCh = '-') THEN ExDoppelt
			ELSIF (s.s = ',') & (index # 0) THEN EndSammeln
			ELSIF (s.s = ',') & (bracket = endbracket) THEN EndSammeln; StartSammeln; state := 79
			ELSIF (s.s = '<') OR (s.s = '<=') OR (s.s = '=')
				OR (s.s = '>') OR (s.s = '>=') OR (s.s = '#') THEN
				term := term + ' ' + s.s + ' ';
				relation := s.s$;
			ELSE
				(* translate variable type for type cast - see also 80 *)
				IF s.s = 'short' THEN s.s := 'SHORTINT'
				ELSIF s.s = 'long' THEN s.s := 'LONGINT'
				ELSIF s.s = 'char' THEN s.s := 'CHAR'
				ELSIF s.s = 'int' THEN s.s := 'INTEGER'
				ELSIF s.s = 'float' THEN s.s := 'REAL'
				ELSIF s.s = 'double' THEN s.s := 'REAL'
				ELSIF s.s = 'bool' THEN s.s := 'BOOLEAN'
				ELSIF s.s = 'boolean' THEN s.s := 'BOOLEAN'
				END;
				(* type cast with pointer *)
				IF (s.s = '*') & (s.nextCh = ')') THEN s.s := '(***)' END;
				(* *)
				IF (s.class = C2cpFiles.cName) THEN
					IF sign = 'U' THEN
						IF C2cpStrings.Right(s.s, 1)$ = '^' THEN s.s := C2cpStrings.Left(s.s, LEN(s.s$) - 1)$ END;
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
					IF dummy & (bracket = 0) THEN
						Output(term); Output(';' + C2cpStrings.line); Output(Margin(0));
						term := 'dummy';
						varname(*assign*) := 'dummy';
						dummy := FALSE;
					END;
				END;
				IF (s.s = '.') OR (s.s = '^.') THEN
					IF ((sign = 'U') 
					OR (sign = 'V')) 
					OR ((sign = 'X') & (C2cpStrings.Right(term, 1)$ = '^')) THEN
						s.s := '.'
					END;
					term := term + s.s;
					alt := group$;
					name := symtype$; (* set group name := record name and ... *)
					IF C2cpStrings.Left(name, 1)$ = '^' THEN name := C2cpStrings.After(name, '^')$ END;
					group := C2cpTable.GetInfo(C2cpTable.Search(group, name))$;
					SkipSpace; (* ... translate next token via record name *)
					group := alt$;
				END;
				IF (s.s = ':=') THEN
					varname(*assign*) := C2cpStrings.Trim(C2cpStrings.Before(term, ':='))$;
				END;
				IF (s.s = ':=') & (ma > 0) & (mi = 0) THEN
					mi := LEN(term$) + 1;
					mk := bracket - 1;
					term := term + ' ' + s.s + ' ';
				ELSE
					IF LEN(term$) + LEN(s.s$) > 255 THEN
						Output(C2cpStrings.line); Output(Margin(0));
						Output(term);
						Output(C2cpStrings.line); Output(Margin(0));
						term := "";
					END;
					term := term + s.s;
				END;
				IF (s.nextCh = '.') & (symtype = 'Registers') THEN
					alt := group$;
					group := symtype$;
					SkipSpace;
					term := term + s.s;
					SkipSpace;
					IF ((s.s = 'x') OR (s.s = 'h')) & (s.nextCh = '.') THEN
						SkipSpace;
						SkipSpace;
					END;
					term := term + s.s;
					group := alt$;
				END;
				IF (s.s = ':=') OR (s.s = ',') THEN
					vorne := FALSE;
					IF define & (bracket = 0) THEN
						LookAhead;
						IF s.class = C2cpFiles.cSpace THEN term := term + s.s END;
						IF s.nextCh = '(' THEN
							SkipSpace;
							bracket := 1;
							delBracket := TRUE;
						END;
					END;
				ELSE
					IF (bracket < closed) & ~(C2cpStrings.IsAlpha(s.nextCh) OR (s.nextCh = '_')) & (s.nextCh # '.')
						THEN
						term := term + ')';
						closed := 0;
					END;
				END;
				IF s.class = C2cpFiles.cName THEN
					IF sign = 'P' THEN term := term + ' ' END;
					IF sign = 'F' THEN LookAhead END;
				END;
			END;
		END Expression;

		PROCEDURE (*80*) TypeCast;
			VAR wort: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(80);
			wort := '';
			state := 79;
			IF s.class = C2cpFiles.cSpace THEN
				SkipSpace;
			END;
			IF s.s = 'unsigned' THEN SkipSpace END;
			IF (s.s = 'char') 
			OR (s.s = 'int') 
			OR (s.s = 'double') 
			OR ((s.class = C2cpFiles.cName) & (sign = 'T')) THEN
				DEC(bracket);
				wort := s.s$;
				SkipSpace;
				IF s.s = 'far' THEN
					SkipSpace;
				END;
				IF (wort = 'char') & (s.s = '*') THEN
					wort := 'ARRAY OF CHAR';
					SkipSpace;
				END;
				IF (s.s = ')') & (s.nextCh # ',') & (s.nextCh # ')') THEN
					(* type cast translation *)
					IF alloc # '' THEN
						LookAhead2;
					ELSE
						IF wort = 'char' THEN wort := 'CHR'
						ELSIF wort = 'int' THEN wort := 'ENTIER'
						ELSIF wort = 'double' THEN wort := 'LONG'
						ELSE wort := '(*' + wort + '*)' (* no type cast translation *)
						END;
						term := term + wort;
						LookAhead2;
						IF s.nextCh # '(' THEN
							term := term + '(';
							closed := bracket + 1;
						END;
					END;
				ELSE (* no type cast translation *)
					term := term + '(*' + wort;
					WHILE s.s # ')' DO
						term := term + s.s;
						SkipSpace;
					END;
					term := term + '*)';
				END;
			ELSE (* no type cast translation *)
				IF ma = 0 THEN
					ma := LEN(term$) + 1;
				END;
				term := term + '(' + wort;
				Expression;
			END;
		END TypeCast;

		PROCEDURE (*81*) ElseStatement;
		BEGIN
			C2cpTrace.Sub(81);
			IF s.s = 'else' THEN
				SkipEol;
				IF s.s = 'if' THEN
					anfang := TRUE;
					StartSammeln;
					DoIf;
					term := 'ELSIF ';
					ma := 7;
				ELSE
					Push('I', deepness, TRUE);
					INC(deepness);
					Output('ELSE');
					NewStatementLine;
				END;
			ELSE
				Output(';'); Output(C2cpStrings.line); Output(Margin(0));
				StartStatement;
			END;
		END ElseStatement;

		PROCEDURE (*82*) EndStatement;
		BEGIN
			C2cpTrace.Sub(82);
			IF s.class = C2cpFiles.cSpace THEN
				Output(s.s);
			ELSIF define & (s.class = C2cpFiles.cEOL) THEN
				EndDefine;
			ELSE
				IF ende[stack] & (deepness = wert[stack] + 1) THEN
					IF s.class = C2cpFiles.cEOL THEN LookAhead END;
					IF s.class = C2cpFiles.cSpace THEN SkipSpace END;
					WHILE ende[stack] & (deepness = wert[stack] + 1) DO
						IF anw[stack] # '' THEN (* END FOR *)
							OutputMarker(anw[stack]);
						END;
						Pop;
						DEC(deepness);
						Output(C2cpStrings.line); Output(Margin(0));
						IF s.s # 'else' THEN Output('END;') END;
					END;
					IF s.s = 'else' THEN
						ElseStatement
					ELSE
						IF s.class # C2cpFiles.cEOL THEN
							anfang := TRUE;
							Output(C2cpStrings.line);
							IF s.s = '}' THEN Output(Margin( - 1))
							ELSE Output(Margin(0)) END;
						END;
						StartStatement;
					END;
				ELSE
					StartStatement;
				END;
			END;
		END EndStatement;

		PROCEDURE (*83*) NewStatementLine;
		BEGIN
			C2cpTrace.Sub(83);
			IF s.class = C2cpFiles.cSpace THEN
				IF (s.nextCh # C2cpStrings.line) & (s.nextCh # '{') THEN Output(s.s) END;
				state := 83;
			ELSIF define & (s.class = C2cpFiles.cEOL) THEN
				EndDefine;
			ELSE
				IF s.class = C2cpFiles.cEOL THEN SkipSpace END;
				IF s.s = '{' THEN
					anfang := FALSE;
					DoBegin;
				ELSE
					Output(C2cpStrings.line);
					IF s.s = '}' THEN Output(Margin( - 1)) ELSE Output(Margin(0)) END;
					anfang := TRUE;
					state := 75;
					IF s.class # C2cpFiles.cSpace THEN StartStatement END;
				END;
			END;
		END NewStatementLine;

		PROCEDURE EndInOut;
		BEGIN
			C2cpTrace.Sub(84);
			IF ln THEN Output('Ln'); ln := FALSE; END;
			state := 79; (* end of scanf or printf *)
			IF define & (s.class = C2cpFiles.cEOL) THEN
				s.s := ' ';
				s.class := C2cpFiles.cSpace;
				term := '';
				EndDefine;
			END;
		END EndInOut;

		PROCEDURE (*85*) DoScanf;
		BEGIN
			C2cpTrace.Sub(85);
			state := 79;
			Expression;
		END DoScanf;

		PROCEDURE (*86*) DoScanf1;
		BEGIN
			C2cpTrace.Sub(86);
			IF s.class = C2cpFiles.cSpace THEN SkipSpace END;
			Output(' '); Output(s.s);
			state := 87;
		END DoScanf1;

		PROCEDURE (*87*) DoScanf2;
		BEGIN
			C2cpTrace.Sub(87);
			IF s.class = C2cpFiles.cString THEN
				Output('(*'); Output(s.s); Output('*)');
				IF s.nextCh = ',' THEN SkipSpace END; (* suppressed "," behind "..." *)
			ELSE
				Output(s.s);
			END;
			IF s.s = ')' THEN EndInOut END; (* end of scanf *)
		END DoScanf2;

		PROCEDURE (*88*) DoPrintf;
		BEGIN
			C2cpTrace.Sub(88);
			IF s.s = ',' THEN
				parameterCount := parameterCount - 1;
			END;
			IF (s.s = ')') & (bracket = 1) THEN
				IF parameterCount > 0 THEN
					term := C2cpStrings.Trim(term) + ', dummy';
					C2cpTable.Insert(group, 'dummy', '', 'INTEGER', '', 'V', s.lineNumber);
				END;
				state := 79; (* end of printf *)
			END;
			Expression;
			IF (bound > 0) & (bracket <= bound) THEN
				term := term + '^';
				bound := 0;
			END;
			IF (s.class = C2cpFiles.cName) & (symtype = 'ARRAY OF CHAR') & (parameterCount < 1) THEN
				IF (s.nextCh = '(') OR (s.nextCh = '[') THEN
					bound := bracket
				ELSE
					term := term + '^'
				END;
			END;
		END DoPrintf;

		PROCEDURE (*89*) DoPrintf1;
			VAR
				kanal, merke, teil, rest: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(89);
			kanal := '';
			IF s.class = C2cpFiles.cSpace THEN SkipSpace END;
			IF s.s = '(' THEN SkipSpace END;
			IF s.class = C2cpFiles.cName THEN
				kanal := s.s$;
				SkipSpace;
				IF s.s = ',' THEN SkipSpace END;
			END;
			IF s.class = C2cpFiles.cString THEN
				merke := '';
				IF kanal # '' THEN kanal := '(*' + kanal + '*)' END;
				rest := C2cpStrings.SubStr(s.s, 1, LEN(s.s$) - 2)$;
				SkipSpace;
				WHILE rest # '' DO (* start for all '\n' *)
					IF C2cpStrings.Pos('\n', rest) # - 1 THEN ln := TRUE END;
					teil := C2cpStrings.Parse(rest, '\n')$; (* get the part in front of '\n' *)
					IF teil # '' THEN
						merke := '';
						WHILE teil # '' DO (* start for all '%' *)
							Output(merke);
							merke := C2cpStrings.Parse(teil, '%')$; (* get the part in front of '%' *)
							IF merke # '' THEN
								Output('String('); 
								Output(kanal); Output("'"); Output(merke); Output("'");
								Output(')');
								merke := '; StdLog.';
							END;
							IF (teil # '') & (s.s = ',') THEN
								Output(merke); Output('Int('); Output(kanal);
								(* insert variable name *)
								SkipEol;
								WHILE (s.s # ',') & (s.s # ')') DO
									Expression;
									s.Scan;
									Look;
								END;
								Output(C2cpStrings.Trim(term));
								term := '';
								teil := C2cpStrings.Right(teil, LEN(teil$) - 1)$; (* %x *)
								Output(')');
							END;
							merke := '; StdLog.';
						END; (* -end for all '%' *)
						IF ln THEN merke := merke + 'Ln'; ln := FALSE ELSE merke := '' END;
					ELSE
						merke := 'Ln'; ln := FALSE; (* it exist only one \n *)
					END;
					IF rest # '' THEN Output(merke); Output('; StdLog.') END;
				END; (* -end for all '\n' *)
				IF s.s = ')' THEN
					Output(merke);
					EndInOut; (* end of printf *)
				ELSE (* s.s = ',' *)
					Output('); StdLog.String(');
					state := 90;
				END;
			ELSE (* parameter list without format string *)
				Output('String('); Output(kanal);
				state := 90;
				IF (s.s = ')') OR (s.s = ';') THEN
					Output(')'); EndInOut;
				ELSE
					Output('); StdLog.String('); Output(s.s);
				END;
			END;
		END DoPrintf1;

		PROCEDURE (*90*) DoPrintf2;
		BEGIN
			C2cpTrace.Sub(90);
			IF s.s = ',' THEN
				Output('); StdLog.String(');
			ELSE
				Output(s.s);
			END;
			IF (s.s = ')') OR (s.s = ';') THEN EndInOut END; (* end of printf *)
		END DoPrintf2;

		PROCEDURE (*91*) DoSprint;
			VAR
				merke, rest: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(91);
			merke := '';
			IF s.class = C2cpFiles.cSpace THEN SkipSpace END;
			IF s.s = '(' THEN SkipSpace END;
			WHILE s.s # ',' DO (* fetch the result variable *)
				Output(s.s);
				s.Scan;
				Look;
			END;
			Output(' := ');
			SkipSpace;
			IF s.class = C2cpFiles.cString THEN
				rest := C2cpStrings.SubStr(s.s, 1, LEN(s.s$) - 2)$;
				SkipSpace;
				WHILE rest # '' DO (* start for all '%' *)
					Output(merke);
					merke := C2cpStrings.Parse(rest, '%')$; (* get the part in front of '%' *)
					IF merke # '' THEN
						Output("'"); Output(merke); Output("'");
						merke := ' + ';
					END;
					IF (rest # '') & (s.s = ',') THEN
						Output(merke); (* insert variable name *)
						SkipSpace; IF s.class = C2cpFiles.cEOL THEN SkipSpace END;
						WHILE (s.s # ',') & (s.s # ')') DO
							Expression;
							s.Scan;
							IF s.class = C2cpFiles.cSpace THEN SkipSpace END;
						END;
						Output(term);
						term := '';
						rest := C2cpStrings.Right(rest, LEN(rest$) - 1)$; (* %x *)
					END;
					merke := ' + ';
				END; (* -end for all '%' *)
			END;
			IF s.s # ')' THEN Output(merke); Output(s.s) END;
			state := 79; (* end of sprintf *)
		END DoSprint;

		PROCEDURE (*92*) DoCin;
		BEGIN
			C2cpTrace.Sub(92);
			IF s.class = C2cpFiles.cSpace THEN SkipSpace END;
			IF s.s = ' SHR ' (*>>*) THEN SkipSpace ELSE Output(C2cpStrings.line); Output(Margin(0)) END;
			Output('READ (');
			WHILE (s.s # ';') & (s.s # ' SHR ') (*>>*) DO
				Output(s.s);
				SkipSpace;
			END;
			Output(')');
			IF s.s # ';' THEN Output(';') ELSE EndSammeln END;
		END DoCin;

		PROCEDURE (*93*) DoCout;
			VAR teil: ARRAY 256 OF CHAR;
		BEGIN
			C2cpTrace.Sub(93);
			IF s.class = C2cpFiles.cSpace THEN SkipSpace END;
			IF s.s = ' SHL ' (*<<*) THEN SkipSpace ELSE Output(C2cpStrings.line); Output(Margin(0)) END;
			IF s.class = C2cpFiles.cString THEN
				s.s := C2cpStrings.SubStr(s.s, 1, LEN(s.s$) - 2)$;
				WHILE s.s # '' DO (* for all '\n' *)
					IF C2cpStrings.Pos('\n', s.s) # -1 THEN ln := TRUE END;
					teil := C2cpStrings.Parse(s.s, '\n')$; (* get the part in front of '\n' *)
					IF teil # '' THEN
						Output('StdLog.String('); Output("'"); Output(teil); Output("'"); Output(')');
					END;
					IF ln THEN
						Output('StdLog.Ln'); ln := FALSE;
						IF s.s # '' THEN Output(';') END;
					END;
					IF s.s # '' THEN Output(C2cpStrings.line); Output(Margin(0)) END;
				END;
				SkipSpace;
			ELSE
				Output('StdLog.String');
				Output(' (');
				WHILE (s.s # ';') & (s.s # ' SHL ') (*<<*) DO
					Output(s.s);
					SkipSpace;
				END;
				Output(')');
			END;
			IF s.s # ';' THEN Output(';') ELSE EndSammeln END;
		END DoCout;

		PROCEDURE (*94*) DoUsing;
		BEGIN
			C2cpTrace.Sub(94);
			IF s.s = ';' THEN (* end of using *)
				Output('; *)');
				state := 2;
			ELSE
				Output(s.s);
				IF (s.class = C2cpFiles.cName) THEN
					C2cpTable.Insert('', name, '', '', '', 'M', s.lineNumber);
				END;
			END;
		END DoUsing;

		PROCEDURE (*95*) DoNamespace;
		BEGIN
			C2cpTrace.Sub(95);
			IF (s.class = C2cpFiles.cName) THEN
				group := name$;
				namespace := name$;
			END;
			CopyUntil(C2cpStrings.line, ' *)', C2cpStrings.line);
		END DoNamespace;

		(*96 & 97 are not used*) 

		PROCEDURE (*98*) BlockStatement;
		BEGIN
			C2cpTrace.Sub(98);
			(* - common of - 2 Block: definition part - 76 Statement: statement part - *)
			IF s.s = '#' THEN state := 14
			ELSIF s.s = 'main' THEN DeclareVariable
			ELSIF s.s = 'public' THEN DeclareVariable
			ELSIF s.s = 'class' THEN DeclareVariable
			ELSIF s.s = 'auto' THEN DeclareVariable
			ELSIF s.s = 'static' THEN DeclareVariable
			ELSIF s.s = 'register' THEN DeclareVariable
			ELSIF s.s = 'extern' THEN DeclareVariable
			ELSIF s.s = 'local' THEN DeclareVariable
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
			ELSIF (s.class = C2cpFiles.cName) & (sign = 'T') THEN DeclareVariable
			ELSIF s.s = 'const' THEN DeclareVariable
			ELSIF s.s = 'volatile' THEN DeclareVariable
			ELSIF s.s = 'typedef' THEN state := 20
			ELSIF s.s = 'struct' THEN union := 0; enum := 0; state := 21;
			ELSIF s.s = 'union' THEN union := 1; enum := 0; state := 21;
			ELSIF s.s = 'enum' THEN union := 0; enum := 1; state := 21;
			ELSIF s.s = 'void' THEN DeclareVariable
			ELSIF s.s = 'inline' THEN LookAhead; Output('(* inline *)');
			ELSIF s.s = 'using' THEN Output('(* using'); state := 94
			ELSIF s.s = 'namespace' THEN Output('(* namespace'); state := 95
			ELSIF s.s = '{' THEN DoBegin
			ELSIF s.s = '}' THEN DoEnd
			ELSIF s.s = 'if' THEN IF vbegin THEN StartStatement ELSE DoIf END
			ELSIF s.s = 'else' THEN IF vbegin THEN StartStatement ELSE DoElse END
			ELSIF s.s = 'for' THEN IF vbegin THEN StartStatement ELSE DoFor END
			ELSIF s.s = 'while' THEN IF vbegin THEN StartStatement ELSE DoWhile END
			ELSIF s.s = 'do' THEN IF vbegin THEN StartStatement ELSE DoRepeat END
			ELSIF s.s = 'switch' THEN IF vbegin THEN StartStatement ELSE DoSwitch END
			ELSIF s.s = 'case' THEN IF vbegin THEN StartStatement ELSE DoCase END
			ELSIF s.s = 'break' THEN IF vbegin THEN StartStatement ELSE DoBreak END
			ELSIF s.s = 'default' THEN IF vbegin THEN StartStatement ELSE DoCase END
			ELSIF s.s = 'goto' THEN IF vbegin THEN StartStatement ELSE DoGoto END
			ELSIF state = 2 THEN
				(* - 2 Block - definition part - *)
				IF s.class = C2cpFiles.cSpace THEN Output(s.s)
				ELSIF s.class = C2cpFiles.cEOL THEN Output(s.s)
				ELSIF recname # '' THEN DeclareVariable
				ELSIF deepness > 0 THEN StartStatement
				ELSIF vbegin THEN StartStatement
				ELSIF s.class = C2cpFiles.cName THEN DeclareVariable
				ELSE Output(s.s)
				END
			ELSE
				(* - 76 Statement - statement part - *)
				IF s.s = 'scanf' THEN DoInOut(85, 86, 'READ')
				ELSIF s.s = 'fscanf' THEN DoInOut(85, 86, 'READ')
				ELSIF s.s = 'printf' THEN DoInOut(88, 89, 'StdLog.')
				ELSIF s.s = 'fprintf' THEN DoInOut(88, 89, 'StdLog.')
				ELSIF s.s = 'sprintf' THEN state := 91
				ELSIF s.s = 'cin' THEN state := 92
				ELSIF s.s = 'cout' THEN state := 93
				ELSIF s.s = 'exit' THEN DoHalt
				ELSIF s.s = 'strcat' THEN DoString
				ELSIF s.s = 'strncat' THEN DoString
				ELSIF s.s = 'strcpy' THEN DoString
				ELSIF s.s = 'strncpy' THEN DoStringCopy
				ELSIF s.s = 'Intr' THEN DoIntr
				ELSIF s.s = 'MsDos' THEN DoIntr
				ELSIF s.s = 'Randomize' THEN DoRandomize
				ELSIF s.s = 'randomize' THEN Output('Randomize')
				ELSIF s.s = 'asm' THEN StartAsm
				ELSIF s.s = '#' THEN state := 14
				ELSIF s.class = C2cpFiles.cSpace THEN Output(s.s)
				ELSIF define & (s.class = C2cpFiles.cEOL) THEN EndDefine
				ELSIF ~ define & (s.class = C2cpFiles.cEOL) THEN Output(s.s)
				ELSE
					state := 79;
					Expression;
				END;
			END;
		END BlockStatement;

		(*99 #include file via 16 #include name*) 

	BEGIN
		C2cpTrace.Sub(0);
		outcome := '';
		(* 1. Skip over blanks at end of line *)
		(* 2. If not in comment then check if comment *)
		(* 3. If not in comment then Look = MemorizeName & Swap *)
		(* 4. Via state do task *)
		(* 5. If new line and not in comment then do left margin *)
		IF (s.class = C2cpFiles.cSpace) & (s.nextCh = C2cpStrings.line) THEN s.Scan END;
		IF (state # 3) & (state # 4) THEN
			IF (s.s = '/') & (s.nextCh = '*') THEN
				IF vcomma OR (enum > 0) THEN
					OutputVariableType;
					Output(blank);
				END;
				s.Scan;
				s.s := '(*';
				back := state;
				state := 3;
			END;
			IF (s.s = '/') & (s.nextCh = '/') THEN
				IF vcomma OR (enum > 0) THEN
					OutputVariableType;
					Output(blank);
				END;
				s.Scan;
				s.s := '(* //';
				back := state;
				state := 4;
			END;
		END;
		IF (state # 3) & (state # 4) & (state # 16) & (state # 99) THEN
			Look;
		END;
		CASE state OF
		| 1: StartBlock; (* Init & Block *)
		| 2: Block; (* Block without Init *)
		| 3: Comment1; (* /* .. */ *)
		| 4: Comment2; (* // .. CR *)
		| 6: DoProcedure; (* PROCEDURE or FUNCTION name *)
		| 7: ParameterStart; (* ... for .DEF of PROCEDURE or FUNCTION *)
		| 8: ParameterList; (* ... for .DEF of PROCEDURE or FUNCTION *)
		| 9: ParameterEnd; (* ... for .DEF of PROCEDURE or FUNCTION *)
		| 10: ParameterStart; (* ... of PROCEDURE or FUNCTION *)
		| 11: ParameterList; (* ... of PROCEDURE or FUNCTION *)
		| 12: ParameterEnd; (* ... of PROCEDURE or FUNCTION *)
		| 13: RunParameter; (* ... of 'main' *)
		| 14: Preprocessor; (* # *)
		| 15: IncludeStart; (* #include start *)
		| 16: IncludeName; (* #include name & end *)
		| 17: DoAsmBlock; (* #asm .. #endasm *)
		| 18: DoDefine; (* FUNCTION, DEFINE or CONST ? *)
		| 19: DoConstant; (* #define - CONST *)
		| 20: DeclareType; (* TYPE *)
		| 21: DeclareRecord; (* RECORD *)
		| 26: ListVariable; (* TYPE, VAR or FUNCTION *)
		| 27: DoEnumEnde; (* enum type *)
		| 28: ArrayVariable; (* VAR *)
		| 29: InitVariable; (* INIT VAR *)
		| 30: NewVariableLine; (* insert CR+LF - var def *)
		| 31: NewBlockLine; (* insert CR+LF - Block Ebene *)
		| 73: DoAsm; (* assembler line *)
		| 74: EndAsm; (* are there more assembler line ? *)
		| 75: StartStatement; (* for +=, -=, *=, etc. *)
		| 76: Statement; (* IF, FOR, WHILE, etc. *)
		| 77: StartCondition; (* <=, AND, etc. *)
		| 78: Condition; (* <=, AND, etc. *)
		| 79: Expression; (* for ++ & -- *)
		| 80: TypeCast; (* (type) *)
		| 81: ElseStatement; (* no ; in front of ELSE *)
		| 82: EndStatement; (* insert END *)
		| 83: NewStatementLine; (* insert CR+LF and start statement *)
		| 85: DoScanf; (* scanf *)
		| 86: DoScanf1; (* READ 1. part *)
		| 87: DoScanf2; (* READ 2. part *)
		| 88: DoPrintf; (* printf *)
		| 89: DoPrintf1; (* StdLog. 1. part *)
		| 90: DoPrintf2; (* StdLog. 2. part *)
		| 91: DoSprint; (* sprintf *)
		| 92: DoCin; (* cin *)
		| 93: DoCout; (* cout *)
		| 94: DoUsing; (* using  - c# *)
		| 95: DoNamespace; (* namespace - c# *)
		| 99: IncludeName; (* #include file & end *)
		ELSE
			StdLog.String('>>> invalid state '); StdLog.Int(state);
			StdLog.String(' at line '); StdLog.Int(s.lineNumber); StdLog.String(' <<<'); StdLog.Ln;
			StartBlock;
		END;
		IF s.class = C2cpFiles.cEOL THEN
			anfang := TRUE;
			LookAhead;
			IF (deepness = 0) & (C2cpStrings.Left(follow, 3)$ = '-*)') THEN
				Output(Margin(0));
				IF s.nextCh # '{' THEN Output('--- ') END;
			ELSIF (state = 3) OR (s.nextCh = '/') THEN
				Output(Margin(0));
				IF (deepness = 0) & (s.class = C2cpFiles.cSpace) & ~ vbegin THEN Output(s.s) END
			ELSIF s.nextCh = '#' THEN Output(Margin(0)) (* margin for preprocessor *)
			ELSIF s.nextCh = '}' THEN Output(Margin( - 1))
			ELSIF isgoto THEN Output(Margin( - 1))
			ELSIF (state = 8) OR (state = 11) THEN Output(Margin( + 8))
			ELSIF (state = 20) OR (state = 21) THEN (* no margin *)
			ELSE Output(Margin(0))
			END;
			isgoto := FALSE;
		ELSE
			IF (outcome # '') OR (term # '') THEN anfang := FALSE END;
		END;
		C2cpTrace.Result(state, outcome);
	END Biber;

END C2cpCBiber.

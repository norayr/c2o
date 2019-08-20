(*---------------------------------------------------------------------------
--   Debugging Routines
--   (C) Helmut Zinn   V12/08/21
--
--   C2cpTrace.DebugOn
--   C2cpTrace.DebugOff
--
	"Toggle &Debug"	""	"C2cpTrace.ToggleDebug"	"C2cpTrace.ToggleDebugGuard"
---------------------------------------------------------------------------*)

MODULE C2cpTrace;

	

	IMPORT
		Dialog, Ports, StdLinks, StdLog, Strings, TextModels;

	VAR
		debug: BOOLEAN;
		pass2: BOOLEAN;
		newline: BOOLEAN;
		state: INTEGER;

	PROCEDURE WriteLink (number: INTEGER);
		VAR
			attr: TextModels.Attributes;
			beg, end, color: INTEGER;
			num, msg: ARRAY 32 OF CHAR;
			cmd: ARRAY 128 OF CHAR;
	BEGIN
		Strings.IntToStringForm(number, 10, 1, '0', FALSE, num);
		cmd := "InfoCmds.OpenDoc('C2cp/Mod/CBiber'); InfoCmds.Select('Sub(" + num$ + ")')";
		IF number = state THEN 
			msg := "[" + num$ + "]";
			color := Ports.red
		ELSE 
			msg := "(" + num$ + ")"; 
			color := Ports.blue
		END;
		StdLog.View(StdLinks.dir.NewLink(cmd));
		StdLog.String(msg);
		end := StdLog.text.Length(); beg := end - LEN(msg$);
		NEW(attr); attr.InitFromProp(StdLog.text.Prop(beg, end));
		attr := TextModels.NewColor(attr, color);
		StdLog.text.SetAttr(beg, end, attr);
		StdLog.View(StdLinks.dir.NewLink(""));
	END WriteLink;

	PROCEDURE Token* (lineNumber: INTEGER; IN s: ARRAY OF CHAR);
	BEGIN
		IF debug & pass2 THEN
			IF ~newline THEN StdLog.Ln END;
			StdLog.Int(lineNumber); StdLog.String(': '); StdLog.String(s); StdLog.Ln;
			newline := TRUE;
		END;
	END Token;

	PROCEDURE Sub* (number: INTEGER);
	BEGIN
		IF debug & pass2 THEN
			IF newline THEN StdLog.Tab ELSE StdLog.Char(' ') END;
			WriteLink(number); (* StdLog.Char('('); StdLog.IntForm(number, 10, 1, '0', FALSE); StdLog.Char(')'); *)
			newline := FALSE;
		END;
	END Sub;

	PROCEDURE Result* (zustand: INTEGER; IN s: ARRAY OF CHAR);
	BEGIN
		IF debug & pass2 THEN
			state := zustand;
			IF s # "" THEN
				IF ~newline THEN StdLog.Ln END;
				StdLog.Tab; StdLog.String(s); StdLog.Ln;
				newline := TRUE;
			END;
		END;
	END Result;

	PROCEDURE CloseDebug*;
	BEGIN
		IF debug & pass2 THEN
			IF ~newline THEN StdLog.Ln END;
			StdLog.Ln;
			pass2 := FALSE;
			newline := TRUE;
			StdLog.Open;
		END;
	END CloseDebug;

	PROCEDURE SetPass2*;
	BEGIN
		pass2 := TRUE;
	END SetPass2;

	PROCEDURE DebugOn*;
	BEGIN
		debug := TRUE;
		pass2 := FALSE;
		newline := FALSE;
		state := 1;
	END DebugOn;

	PROCEDURE DebugOff*;
	BEGIN
		debug := FALSE;
		pass2 := FALSE;
		newline := FALSE;
		state := 1;
	END DebugOff;

	PROCEDURE ToggleDebugGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~debug THEN par.label := "Debug On"
		ELSE par.label := "Debug Off"
		END
	END ToggleDebugGuard;

	PROCEDURE ToggleDebug*;
	BEGIN
		IF debug THEN DebugOff ELSE DebugOn END
	END ToggleDebug;

BEGIN
	DebugOff;
END C2cpTrace.

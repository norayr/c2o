(*-----------------------------------------------------------------------------
--   Manage the key table
--   Schlüssel-Tabelle verwalten
--   (C) Helmut Zinn   V90/03/24 - V95/02/23 - V05/05/12
-------------------------------------------------------------------------------
--   MakeDynString  Copy static string to dynamic string.
--                  Kopiert den statischen string in den dynamischen String.
--
--   SearchKey    Get index of keyword, if not found then enter it into keytable.
--                Schlüssel für ein Wort holen, wenn fehlt in Tabelle eintragen.
--   EntryOrder   Alpha order place of this word.
--                Auf welchen Platz das Wort wäre,   wenn alpha sortiert.
--   AlphaOrder   Which word is on this place?
--                Welches Wort an diesem Platz wäre, wenn alpha sortiert.
--   GetKeyword   Get the word from the key table.
--                Das Wort, das an diesem Platz ist, holen.
--
--   ShowKeyList  List keyword table for debugging.
--                Die gesamte Schlüsseltabelle für Testzwecke ausgeben.
--   Dispose      Give the table memory free - empty table for new use.
--                Schlüsseltabelle im Speicher löschen
-----------------------------------------------------------------------------*)

MODULE C2cpKeytab;

	

	IMPORT C2cpStrings, Ports, StdLog, TextMappers, TextModels, TextRulers, TextViews, Views;

	CONST
		maxKey* = 2000;

	VAR
		numberOfKey-: INTEGER;
		keytable: ARRAY maxKey OF RECORD
			place: INTEGER;
			alpha: INTEGER;
			word: C2cpStrings.DynString;
		END;

	PROCEDURE EntryOrder* (index: INTEGER): INTEGER;
	BEGIN
		RETURN keytable[index].place;
	END EntryOrder;

	PROCEDURE AlphaOrder* (index: INTEGER): INTEGER;
	BEGIN
		RETURN keytable[index].alpha;
	END AlphaOrder;

	PROCEDURE GetKeyword* (index: INTEGER): C2cpStrings.DynString;

	BEGIN
		RETURN keytable[index].word;
	END GetKeyword;

	PROCEDURE SearchKey* (word: ARRAY OF CHAR): INTEGER;
		VAR
			a, b, i: INTEGER;
			w, t: ARRAY 256 OF CHAR;
	BEGIN
		IF word$ = '' THEN (* quick exit for empty word -- schneller Ausgang für Leerwort *)
			RETURN 0;
		END;
		w := C2cpStrings.ToUpper(word)$; (* search in key table -- in Tabelle suchen *)
		a := 1;
		b := numberOfKey;
		WHILE a <= b DO
			i := (a + b) DIV 2;
			t := C2cpStrings.ToUpper(GetKeyword(AlphaOrder(i)))$;
			IF (w > t) OR (w = t) & (word$ > GetKeyword(AlphaOrder(i))$)
				THEN a := i + 1
			ELSE b := i - 1 END;
		END;
		IF (a > numberOfKey) OR (word$ # GetKeyword(AlphaOrder(a))$) THEN
			IF numberOfKey < maxKey - 1 THEN
				numberOfKey := numberOfKey + 1;
				FOR i := 1 TO numberOfKey DO
					IF keytable[i].place >= a THEN
						keytable[i].place := keytable[i].place + 1;
					END;
				END;
				FOR i := numberOfKey TO a + 1 BY - 1 DO
					keytable[i].alpha := keytable[i - 1].alpha;
				END;
				keytable[numberOfKey].place := a; (* entry word into key table -- in Tabelle eintragen *)
				keytable[a].alpha := numberOfKey;
				keytable[numberOfKey].word := C2cpStrings.MakeDynString(word);
			ELSE
				StdLog.String(' *** C2cpKeytab.SearchKey table overflow *** ');
				StdLog.Ln;
				RETURN 0; (* give empty word back - no entry in key table -- Leerwort zurückgeben *)
			END;
		END;
		RETURN keytable[a].alpha; (* give key -- Schlüssel zurückgeben *)
	END SearchKey;

	PROCEDURE WriteRuler  (VAR fm:  TextMappers.Formatter);
		VAR ruler: TextRulers.Ruler;
	BEGIN
		ruler := TextRulers.dir.New(NIL);
		TextRulers.AddTab(ruler, 10 * Ports.mm); TextRulers.MakeRightTab(ruler);
		TextRulers.AddTab(ruler, 20 * Ports.mm); TextRulers.MakeRightTab(ruler);
		TextRulers.AddTab(ruler, 25 * Ports.mm);
		TextRulers.AddTab(ruler, 70 * Ports.mm); TextRulers.MakeRightTab(ruler);
		TextRulers.AddTab(ruler, 80 * Ports.mm); TextRulers.MakeRightTab(ruler);
		TextRulers.AddTab(ruler, 85 * Ports.mm);
		fm.WriteView(ruler);
	END WriteRuler;

	PROCEDURE ShowKeyList*;
		VAR
			md: TextModels.Model; vw: TextViews.View; title: Views.Title;
			fm: TextMappers.Formatter; i: INTEGER;
	BEGIN
		title := 'Keyword table';
		md := TextModels.dir.New();
		fm.ConnectTo(md);
		vw := TextViews.dir.New(md);
		Views.OpenAux(vw, title); (* browser mode *)
		WriteRuler(fm);
		fm.WriteTab; fm.WriteString('Key');
		fm.WriteTab; fm.WriteString('Place');
		fm.WriteTab; fm.WriteString('Word (entry order)');
		fm.WriteTab; fm.WriteString('Key');
		fm.WriteTab; fm.WriteString('Place');
		fm.WriteTab; fm.WriteString('Word (alpha order)');
		fm.WriteLn;
		FOR i := 0 TO numberOfKey - 1 DO
			fm.WriteTab; fm.WriteInt(i);
			fm.WriteTab; fm.WriteInt(EntryOrder(i));
			fm.WriteTab; fm.WriteString(GetKeyword(i));
			fm.WriteTab; fm.WriteInt(i);
			fm.WriteTab; fm.WriteInt(AlphaOrder(i));
			fm.WriteTab; fm.WriteString(GetKeyword(AlphaOrder(i)));
			fm.WriteLn;
		END;
		fm.WriteString('Ende.'); fm.WriteLn;
	END ShowKeyList;

	PROCEDURE Dispose*;
	BEGIN
		WHILE numberOfKey > 0 DO
			keytable[numberOfKey].word := NIL;
			numberOfKey := numberOfKey - 1;
		END;
	END Dispose;

BEGIN
	numberOfKey := 0; (* empty word at place 0 -- Der 1. Platz in der Tabelle ist ein Leerwort *)
	keytable[0].place := 0;
	keytable[0].alpha := 0;
	keytable[0].word := C2cpStrings.MakeDynString('');
END C2cpKeytab.

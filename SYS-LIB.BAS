DECLARE SUB DISPLAY.ERROR.MESSAGE (ERROR.CODE, ERR.ROUTINE$)
DECLARE SUB DECIMAL.TO.BINARY (X%, B$)
DECLARE SUB OPEN.HELP.FILE (RAM.DRIVE$, HELP.FILE.NUMBER$, HELP.FILE.OPEN!)
DECLARE SUB OPEN.TRIGGER.FILE (RAM.DRIVE$, TRIGGER.FILE.NUMBER$, TRIGGER.FILE.OPEN!)
DECLARE SUB PUT.SHADOW (XS%, YS%, LS%, ds%, C$)
DECLARE SUB COPY.FILES (ERROR.NUM!, FROM.DRIVE$, TO.DRIVE$, EXT.DRIVE$, IN.ARRAY%(), OUT.ARRAY%(), ax!, bx!, cx!, dx!, EXT.MIN.SP#, FMT.NAME$, FMT.VER$, FMT.NDX!, BGCLR, DISPLAY.MODE$, C.SCREEN)
DECLARE SUB DISK.SPACE (IN.ARRAY%(), OUT.ARRAY%(), ax!, bx!, cx!, dx!, AVAILABLE.SPACE#, DRIVE$)
DECLARE FUNCTION HEX.TO.DECIMAL! (HX$)
DECLARE FUNCTION HEX.TO.CHAR$ (HX$)
'
'
'
'*=============================================================================*
'| File Id.  ............ SYS-LIB.BAS                                          |
'| Author ............... Muawiya Abujubain COVENTRY GROUP LTD.                |
'|                                                                             |
'| Purpose  ............. This file includes routines that are called by       |
'|                        program SCR-MNGR.BAS.                                |
'*=============================================================================*
'
'
'
REM $INCLUDE: 'QB.BI'
REM $INCLUDE: 'MOUSE.BI'

SYS.LIB.ERROR.ROUTINE:
	'
	' Trap the following errors, other wise give error message and ABORT :-
	'
	'    53 = File Not Found
	'
	ERROR.CODE = ERR
	SELECT CASE ERROR.CODE
	   CASE 53
	      RESUME NEXT
	   CASE ELSE
	      CALL DISPLAY.ERROR.MESSAGE(ERROR.CODE, ERR.ROUTINE$)
	END SELECT

VIDEO.ERROR.ROUTINE:
	'
	' Trap the following errors, other wise give error message and ABORT :-
	'
	'    5 = Illegal function call
	'
	ERROR.CODE = ERR
	SELECT CASE ERROR.CODE
	   CASE 5
	      RESUME NEXT
	   CASE ELSE
	      CALL DISPLAY.ERROR.MESSAGE(ERROR.CODE, ERR.ROUTINE$)
	END SELECT

SUB ANALIZE.PIC (PIC$)
SHARED ERR.ROUTINE$

' This procedure analizes a picture format that is input by the
' calling program in the variable PIC$. The procedure return the
' proper picture format via the same variable.
'
' The input picture format is usualy compressed, and this procedure
' decompresses it.
'
'    Example of input to this procedure and its output :-
'
'         INPUT                          OUTPUT
'    ===============================================
'       ZZ/ZZ/ZZ                        ZZ/ZZ/ZZ
'       6#.2#                           ######.##
'       6Z.2Z                          ~######.##
'       6Z.2Z-                         ~######.##-
'
' The ~ character means that the field is an edited field, while the #
' means it is a numeric field.
'
' Please note that this procedure is called only for numeric fields.
'
	ERR.ROUTINE$ = "ANALIZE.PIC"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	NUMERIC.EDITED.CODE = 0

	X = INSTR(PIC$, "#")
	IF LEFT$(PIC$, 5) <> "ZZ/ZZ" THEN
	   IF X = 0 THEN
	      X = INSTR(PIC$, "Z")
	      IF X > 0 THEN
		 NUMERIC.EDITED.CODE = 1
	      END IF
	   END IF
	END IF

	IF X > 0 THEN
	   WHOLE.NUMBER = VAL(LEFT$(PIC$, LEN(PIC$) - X + 1))

	   X = INSTR(PIC$, ".")
	   IF X > 0 THEN
	      DECIMAL.POINT = X
	   ELSE
	      DECIMAL.POINT = 0
	   END IF

	   X = INSTR(PIC$, "-")
	   IF X > 0 THEN
	      NEGATIVE.SIGN = X
	   ELSE
	      NEGATIVE.SIGN = 0
	   END IF

	   IF DECIMAL.POINT <> 0 THEN
	      R.PIC$ = RIGHT$(PIC$, LEN(PIC$) - DECIMAL.POINT)
	      X = INSTR(R.PIC$, "#")
	      IF X = 0 THEN
		 X = INSTR(R.PIC$, "Z")
	      END IF
	      IF X > 0 THEN
		 DECIMAL.NUMBER = VAL(LEFT$(R.PIC$, LEN(R.PIC$) - X + 1))
	      ELSE
		 DECIMAL.NUMBER = 0
	      END IF
	   END IF

	   IF NUMERIC.EDITED.CODE = 1 THEN
	      PIC$ = "~" + STRING$(WHOLE.NUMBER, "#")
	   ELSE
	      PIC$ = " " + STRING$(WHOLE.NUMBER, "#")
	   END IF

	   IF DECIMAL.POINT <> 0 THEN
	      PIC$ = PIC$ + "." + STRING$(DECIMAL.NUMBER, "#")
	   END IF

	   IF NEGATIVE.SIGN <> 0 THEN
	      PIC$ = PIC$ + "-"
	   END IF
	END IF
END SUB

SUB CALC (OUT.BUFFER$, C.SCREEN, BGCLR, SWidth%, VideoAddr%, PrL%, PrR%, PrLr%, PrM%, PrLM%, PrMR%, PrAll%, PrNone%, ButtonLeft%, ButtonRight%, ButtonMiddle%, True%, False%, COLORED) STATIC

'  This is the calculator procedure. It makes use of the mouse user
'  library MOUSE.BI.
'
        IF ThereIsAMouse% THEN
           IF NOT MouseReset% THEN
              MOUSE.RESET% = 0
           ELSE
              MOUSE.RESET% = 1
           END IF
           PCOPY C.SCREEN, C.SCREEN + 1
           MouseOn
           ClearButton ButtonLeft%
           PCOPY C.SCREEN + 1, C.SCREEN
           SCREEN , , C.SCREEN, C.SCREEN
        END IF

	DISPLAY$ = "N"

	KEY 1, CHR$(1)
	KEY 2, CHR$(2)
	KEY 3, CHR$(3)
	KEY 9, CHR$(9)

MAIN.LINE:

	LOCATE 5, 1, 0
	PCOPY C.SCREEN, C.SCREEN + 1
	SCREEN , , C.SCREEN + 1, C.SCREEN + 1

	IF COLORED = 1 THEN
	   COLOR 3, 1
	ELSE
	   COLOR 15, 0
	END IF

	IF FIRST.TIME = 0 OR OUT.BUFFER$ = "NEW" THEN
	   OPEN "SCR-CALC.INI" FOR INPUT AS #9
	   LINE INPUT #9, TAX1$
	   LINE INPUT #9, TAX2$
	   LINE INPUT #9, TAX3$
	   CLOSE #9
	END IF

	IF FIRST.TIME = 0 THEN
	   ROW% = 13
	   COL% = 20
	   POSITION$ = "LR"
	END IF

	LOCATE ROW% + 0, COL%, 0: PRINT "ีออออออออออออัอออออออออออออออออัออออออออออออออออออออออออออออธ";
	LOCATE ROW% + 1, COL%, 0: PRINT "ณ            ณ                 ณ                            ณ";
	LOCATE ROW% + 2, COL%, 0: PRINT "ณ            ฦอออออออออออออออออต                            ณ";
	LOCATE ROW% + 3, COL%, 0: PRINT "ณ            ณ                 ณ                            ณ";
	LOCATE ROW% + 4, COL%, 0: PRINT "ณ            ฦอออออออออออออออออต                            ณ";
	LOCATE ROW% + 5, COL%, 0: PRINT "ณ            ณ                 ณ                            ณ";
	LOCATE ROW% + 6, COL%, 0: PRINT "ณ            ิอออออออออออออออออพ                            ณ";
	LOCATE ROW% + 7, COL%, 0: PRINT "ฦออออออออออออออออัออTaxอออธ                                 ณ";
	LOCATE ROW% + 8, COL%, 0: PRINT "ณ                ณ        ณ                                 ณ";
	LOCATE ROW% + 9, COL%, 0: PRINT "ณExit With  Exit ณ        ณ                                 ณ";
	LOCATE ROW% + 10, COL%, 0: PRINT "ณ Transfer       ณ        ณ                                 ณ";
	LOCATE ROW% + 11, COL%, 0: PRINT "ิออออออออออออออออฯออออออออฯอออออออออออออออออออออออออออออออออพ";

	IF DISPLAY$ = "N" THEN
	   GOSUB CLEAR.DESC
	   LOCATE ROW% + 3, COL% + 1, 0: PRINT "Last Entry";
	   LOCATE ROW% + 5, COL% + 1, 0: PRINT "Amount";
	   IF COLORED = 1 THEN
	      COLOR 4, 15
	   ELSE
	      COLOR 15, 7
	   END IF
	   LOCATE ROW% + 1, COL% + 14, 0: PRINT SPC(17);
	   LOCATE ROW% + 3, COL% + 14, 0: PRINT SPC(17);
	ELSE
	   GOSUB CLEAR.DESC
	   LOCATE ROW% + 1, COL% + 1, 0: PRINT "Amount";
	   LOCATE ROW% + 3, COL% + 1, 0: PRINT "Tax "; TAX.OP$; " "; TAX.RATE$; "%";
	   LOCATE ROW% + 5, COL% + 1, 0: PRINT "Net";
	   IF COLORED = 1 THEN
	      COLOR 4, 15
	   ELSE
	      COLOR 15, 7
	   END IF
	   GOSUB DISPLAY.TAX
	END IF

	COLOR 0, 15

	FOR I = ROW% + 1 TO ROW% + 7 STEP 2
	   LOCATE I, COL% + 33, 0: PRINT "     ";
	   LOCATE I, COL% + 40, 0: PRINT "     ";
	   LOCATE I, COL% + 47, 0: PRINT "     ";
	   LOCATE I, COL% + 54, 0: PRINT "     ";
	NEXT I

	LOCATE ROW% + 4, COL% + 54, 0: PRINT "     ";
	LOCATE ROW% + 7, COL% + 27, 0: PRINT "     ";
	LOCATE ROW% + 8, COL% + 2, 0: PRINT "        ";
	LOCATE ROW% + 8, COL% + 19, 0: PRINT "   -A ";
	LOCATE ROW% + 8, COL% + 54, 0: PRINT "     ";
	LOCATE ROW% + 9, COL% + 19, 0: PRINT "   -B ";
	LOCATE ROW% + 9, COL% + 27, 0: PRINT " C   ";
	LOCATE ROW% + 9, COL% + 33, 0: PRINT "            ";
	LOCATE ROW% + 9, COL% + 47, 0: PRINT "     ";
	LOCATE ROW% + 9, COL% + 54, 0: PRINT "     ";
	LOCATE ROW% + 10, COL% + 19, 0: PRINT "   -C ";

	IF COLORED = 1 THEN
	   COLOR 4, 15
	ELSE
	   COLOR 15, 7
	END IF

	LOCATE ROW% + 1, COL% + 35, 0: PRINT "%";
	LOCATE ROW% + 1, COL% + 42, 0: PRINT "/";
	LOCATE ROW% + 1, COL% + 49, 0: PRINT "*";
	LOCATE ROW% + 1, COL% + 56, 0: PRINT "-";

	LOCATE ROW% + 3, COL% + 35, 0: PRINT "7";
	LOCATE ROW% + 3, COL% + 42, 0: PRINT "8";
	LOCATE ROW% + 3, COL% + 49, 0: PRINT "9";
	LOCATE ROW% + 4, COL% + 56, 0: PRINT "+";

	LOCATE ROW% + 5, COL% + 35, 0: PRINT "4";
	LOCATE ROW% + 5, COL% + 42, 0: PRINT "5";
	LOCATE ROW% + 5, COL% + 49, 0: PRINT "6";

	LOCATE ROW% + 7, COL% + 29, 0: PRINT "C";
	LOCATE ROW% + 7, COL% + 35, 0: PRINT "1";
	LOCATE ROW% + 7, COL% + 42, 0: PRINT "2";
	LOCATE ROW% + 7, COL% + 49, 0: PRINT "3";

	LOCATE ROW% + 8, COL% + 5, 0: PRINT "F9";
	LOCATE ROW% + 8, COL% + 12, 0: PRINT " ESC ";
	LOCATE ROW% + 8, COL% + 20, 0: PRINT "F1";
	LOCATE ROW% + 8, COL% + 55, 0: PRINT "ฤู";

	LOCATE ROW% + 9, COL% + 20, 0: PRINT "F2";
	LOCATE ROW% + 9, COL% + 29, 0: PRINT "E";
	LOCATE ROW% + 9, COL% + 39, 0: PRINT "0";
	LOCATE ROW% + 9, COL% + 49, 0: PRINT ".";
	LOCATE ROW% + 9, COL% + 56, 0: PRINT "=";

	LOCATE ROW% + 10, COL% + 20, 0: PRINT "F3";

	IF DISPLAY$ = "N" THEN
	   GOSUB CLEAR.DISPLAY
	END IF

	IF FIRST.TIME = 0 THEN
	   GOSUB INITIALIZE
	   FIRST.TIME = 1
	END IF

	IF OUT.BUFFER$ <> "" THEN
	   W$ = "="
	   GOSUB OPERATOR.CLICK
	   READ.OUT$ = OUT.BUFFER$
	   GOSUB CHECK.READ.OUT
	   OUT.BUFFER$ = ""
	   TXT$ = READ.OUT$: INC = 5
	   GOSUB SCREEN.FX
	   OP1$ = READ.OUT$
	   NUM.OPS = 1
	   TEMP.READ.OUT$ = READ.OUT$
	   TXT$ = TEMP.READ.OUT$: INC = 3
	   GOSUB SCREEN.FX
	   GOTO GET.CHARACTER
	END IF

	IF DISPLAY$ = "N" THEN
	   TXT$ = TEMP.READ.OUT$: INC = 3
	   GOSUB SCREEN.FX
	   TXT$ = READ.OUT$: INC = 5
	   GOSUB SCREEN.FX
	END IF

       
GET.CHARACTER:

        IF ThereIsAMouse% THEN
           DO WHILE Buttons% <> PrR%
              EXIT.FLAG = 1
              W$ = INKEY$
              IF W$ <> "" THEN
                 EXIT.FLAG = 0
                 EXIT DO
              END IF
              Buttons% = GetMouseStatus%(MPosX%, MPosY%)
              PY% = MPosY%
              PX% = MPosX%
              GOSUB DspButtons
              IF Buttons% = PrL% THEN
                 MouseOn
                 PY% = MPosY%
                 PX% = MPosX%
                 GOSUB DspButtons
                 ClearButton ButtonLeft%
                 EXIT.FLAG = 0
                 EXIT DO
              END IF
           LOOP
        ELSE
           W$ = INKEY$
        END IF

CHECK.BUTTON:

        IF EXIT.FLAG = 1 THEN
           MouseOn
           ClearButton ButtonRight%
           Buttons% = 0
           SELECT CASE POSITION$
              CASE "LR"
                 W$ = CHR$(0) + "H"
              CASE "TR"
                 W$ = CHR$(0) + "K"
              CASE "TL"
                 W$ = CHR$(0) + "P"
              CASE "LL"
                 W$ = CHR$(0) + "M"
           END SELECT
        END IF

CHECK.W:

	IF W$ = "" THEN
	   GOTO GET.CHARACTER
	END IF

	IF LEN(W$) = 2 THEN
	   SELECT CASE POSITION$

	      CASE "LR"
		 SELECT CASE RIGHT$(W$, 1)
		    CASE "H"
		       ROW% = ROW% - 12
		       POSITION$ = "TR"
		    CASE "K"
		       COL% = COL% - 19
		       POSITION$ = "LL"
		    CASE ELSE
		       GOTO CONTINUE.CALC
		 END SELECT

	      CASE "TR"
		 SELECT CASE RIGHT$(W$, 1)
		    CASE "P"
		       ROW% = ROW% + 12
		       POSITION$ = "LR"
		    CASE "K"
		       COL% = COL% - 19
		       POSITION$ = "TL"
		    CASE ELSE
		       GOTO CONTINUE.CALC
		 END SELECT

	      CASE "TL"
		 SELECT CASE RIGHT$(W$, 1)
		    CASE "P"
		       ROW% = ROW% + 12
		       POSITION$ = "LL"
		    CASE "M"
		       COL% = COL% + 19
		       POSITION$ = "TR"
		    CASE ELSE
		       GOTO CONTINUE.CALC
		 END SELECT

	      CASE "LL"
		 SELECT CASE RIGHT$(W$, 1)
		    CASE "H"
		       ROW% = ROW% - 12
		       POSITION$ = "TL"
		    CASE "M"
		       COL% = COL% + 19
		       POSITION$ = "LR"
		    CASE ELSE
		       GOTO CONTINUE.CALC
		 END SELECT
	   END SELECT
	   GOTO MAIN.LINE
	END IF

CONTINUE.CALC:
	  
	SELECT CASE W$
	   CASE CHR$(27)
	      OUT.BUFFER$ = ""
              IF ThereIsAMouse% THEN
                 MouseOff
              END IF
	      EXIT SUB
	   CASE CHR$(9)
	      IF DISPLAY$ = "T" THEN
		 OUT.BUFFER$ = LTRIM$(STR$(NET#))
	      ELSE
		 OUT.BUFFER$ = READ.OUT$
	      END IF
              IF ThereIsAMouse% THEN
                 MouseOff
              END IF
	      EXIT SUB
	   CASE "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"
	      IF DISPLAY$ = "T" THEN
		 GOSUB CLEAR.DISPLAY
	      END IF
	      DISPLAY$ = "N"
	      GOSUB NUMBER.CLICK
	      TXT$ = READ.OUT$: INC = 5
	      GOSUB SCREEN.FX
	   CASE "+", "-", "*", "/", "=", CHR$(13)
	      GOSUB OPERATOR.CLICK
	      IF DISPLAY$ = "N" THEN
		 TXT$ = READ.OUT$: INC = 5
		 GOSUB SCREEN.FX
	      END IF
	   CASE CHR$(1), CHR$(2), CHR$(3)
	      GOSUB TAX.CLICK
	      W$ = "="
	      GOTO CONTINUE.CALC
	   CASE "%"
	      IF DISPLAY$ = "T" THEN
		 GOSUB CLEAR.DISPLAY
	      END IF
	      DISPLAY$ = "N"
	      GOSUB PERCENT.CLICK
	      TXT$ = READ.OUT$: INC = 5
	      GOSUB SCREEN.FX
	   CASE "C"
	      GOSUB CANCEL.CLICK
	      TXT$ = READ.OUT$: INC = 5
	      GOSUB SCREEN.FX
	      INC = 3
	      GOSUB SCREEN.FX
	      INC = 1
	      GOSUB SCREEN.FX
	   CASE "E"
	      IF DISPLAY$ = "T" THEN
		 GOSUB CLEAR.DISPLAY
	      END IF
	      DISPLAY$ = "N"
	      GOSUB CANCEL.ENTRY.CLICK
	      TXT$ = READ.OUT$: INC = 5
	      GOSUB SCREEN.FX
	   CASE "."
	      IF DISPLAY$ = "T" THEN
		 GOSUB CLEAR.DISPLAY
	      END IF
	      DISPLAY$ = "N"
	      GOSUB DECIMAL.CLICK
	      TXT$ = READ.OUT$: INC = 5
	      GOSUB SCREEN.FX
       END SELECT

       IF DISPLAY$ = "N" THEN
	  IF COLORED = 1 THEN
	     COLOR 3, 1
	  ELSE
	     COLOR 15, 0
	  END IF
	  GOSUB CLEAR.DESC
	  LOCATE ROW% + 3, COL% + 1, 0: PRINT "Last Entry";
	  LOCATE ROW% + 5, COL% + 1, 0: PRINT "Amount";
	  IF COLORED = 1 THEN
	     COLOR 4, 15
	  ELSE
	     COLOR 15, 7
	  END IF
       END IF

       COLOR 15, BGCLR
       LOCATE 25, 1, 0
       PRINT SPACE$(80);
       IF COLORED = 1 THEN
	  COLOR 4, 15
       ELSE
	  COLOR 15, 7
       END IF
       W$ = ""
       EXIT.FLAG = 0
       GOTO GET.CHARACTER

INITIALIZE:

	False = 0
	True = 1
	DECIMAL.FLAG = False
	NUM.OPS = 0
	LAST.INPUT$ = "NONE"
	OP.FLAG$ = " "
	TAX.OP$ = ""
	LAST.OP$ = ""
	DISPLAY$ = "N"
	RETURN

' Click event procedure for C (cancel) key.
' Reset the display and initializes variables.
'
CANCEL.CLICK:

	READ.OUT$ = ""
	OP1$ = "0"
	OP2$ = "0"
	TAX.OP$ = ""
	LAST.OP$ = ""
	DISPLAY$ = "N"
	GOSUB INITIALIZE
	RETURN

' Click event procedure for CE (cancel entry) key.
'

CANCEL.ENTRY.CLICK:

	READ.OUT$ = ""
	DECIMAL.FLAG = False
	LAST.INPUT$ = "CE"
	RETURN

' Click event procedure for decimal point (.) key.
' If last keypress was an operator, initialize
' readout to "0." Otherwise, append a decimal
' point to the display.
'

DECIMAL.CLICK:

	IF LAST.INPUT$ = "NEG" THEN
	    READ.OUT$ = "-0."
	ELSEIF LAST.INPUT$ <> "NUMS" THEN
	    READ.OUT$ = "0."
	END IF
	DECIMAL.FLAG = True
	LAST.INPUT$ = "NUMS"
	RETURN
       
' Click event procedure for number keys (0-9).
' Appends new number to the number in the display.
'
NUMBER.CLICK:

	IF LAST.INPUT$ <> "NUMS" THEN
	    READ.OUT$ = "."
	    DECIMAL.FLAG = False
	END IF

	IF DECIMAL.FLAG THEN
	    READ.OUT$ = READ.OUT$ + W$
	ELSE
	    READ.OUT$ = LEFT$(READ.OUT$, INSTR(READ.OUT$, ".") - 1) + W$ + "."
	END IF

	IF LAST.INPUT$ = "NEG" THEN
	   READ.OUT$ = "-" + READ.OUT$
	END IF

	GOSUB CHECK.READ.OUT

	LAST.INPUT$ = "NUMS"
	RETURN

' Click event procedure for operator keys (+, -, x, /, =).
' If the immediately preceeding keypress was part of a
' number, increment NumOps. If one operand is present,
' set Op1. If two are present, set Op1 equal to the
' result of the operation on Op1 and the current
' input string, and display the result.
'
OPERATOR.CLICK:

	LAST.OP$ = W$

	IF W$ <> "=" AND W$ <> CHR$(13) THEN
	   TAX.OP$ = W$
	   IF TAX.OP$ <> "+" AND TAX.OP$ <> "-" THEN
	      TAX.OP$ = "-"
	   END IF
	END IF

	IF DISPLAY$ = "N" THEN
	   TEMP.READ.OUT$ = READ.OUT$
	   TXT$ = TEMP.READ.OUT$: INC = 3
	   GOSUB SCREEN.FX
	END IF

	IF LAST.INPUT$ = "NUMS" THEN
	    NUM.OPS = NUM.OPS + 1
	END IF

	SELECT CASE NUM.OPS
	    CASE 0
	       IF W$ = "-" AND LAST.INPUT$ <> "NEG" THEN
		   READ.OUT$ = "-" + READ.OUT$
		   LAST.INPUT$ = "NEG"
	       END IF
	    CASE 1
	       OP1$ = READ.OUT$
	       IF W$ = "-" AND LAST.INPUT$ <> "NUMS" AND OP.FLAG$ <> "=" THEN
		  READ.OUT$ = "-"
		  LAST.INPUT$ = "NEG"
	       END IF
	    CASE 2
	       OP2$ = TEMP.READ.OUT$
	       SELECT CASE OP.FLAG$
		  CASE "+"
		     OP1$ = LTRIM$(STR$(VAL(OP1$) + VAL(OP2$)))
		  CASE "-"
		     OP1$ = LTRIM$(STR$(VAL(OP1$) - VAL(OP2$)))
		  CASE "*"
		     OP1$ = LTRIM$(STR$(VAL(OP1$) * VAL(OP2$)))
		  CASE "/"
		     IF VAL(OP2$) = 0 THEN
			COLOR 15, BGCLR
			LOCATE 25, 1, 0: PRINT "Can't divide by zero !!!";
			BEEP
			IF COLORED = 1 THEN
			   COLOR 4, 15
			ELSE
			   COLOR 15, 7
			END IF
			SLEEP 3
		     ELSE
			OP1$ = LTRIM$(STR$(VAL(OP1$) / VAL(OP2$)))
		     END IF
		  CASE "=", CHR$(13)
		     OP1$ = OP2$
		  CASE "%"
		     OP1$ = LTRIM$(STR$(VAL(OP1$) * VAL(OP2$)))
	       END SELECT
	    READ.OUT$ = OP1$
	    NUM.OPS = 1
	END SELECT

	GOSUB CHECK.READ.OUT

	IF LAST.INPUT$ <> "NEG" THEN
	    LAST.INPUT$ = "OPS"
	    OP.FLAG$ = W$
	END IF

	RETURN

' Click event procedure for percent key (%).
' Compute and display a percentage of the first operand.
'
PERCENT.CLICK:

	READ.OUT$ = LTRIM$(STR$(VAL(READ.OUT$) / 100))
	LAST.INPUT$ = "OPS"
	OP.FLAG$ = "%"
	NUM.OPS = NUM.OPS + 1
	DECIMAL.FLAG = True
	RETURN

TAX.CLICK:

	SELECT CASE W$
	   CASE CHR$(1)
	      TAX.RATE$ = TAX1$
	   CASE CHR$(2)
	      TAX.RATE$ = TAX2$
	   CASE CHR$(3)
	      TAX.RATE$ = TAX3$
	END SELECT

	TAX.AMOUNT# = VAL(READ.OUT$) * (VAL(TAX.RATE$) / 100)

	IF TAX.OP$ <> "+" AND TAX.OP$ <> "-" THEN
	   TAX.OP$ = "-"
	END IF

	SELECT CASE TAX.OP$
	   CASE "+"
	      NET# = VAL(READ.OUT$) + TAX.AMOUNT#
	   CASE "-"
	      NET# = VAL(READ.OUT$) - TAX.AMOUNT#
	END SELECT

	GOSUB DISPLAY.TAX

	RETURN

DISPLAY.TAX:

	OLD.READ.OUT$ = READ.OUT$
	TXT$ = READ.OUT$: INC = 1
	GOSUB SCREEN.FX

	READ.OUT$ = LTRIM$(STR$(TAX.AMOUNT#))
	GOSUB CHECK.READ.OUT
	TXT$ = READ.OUT$: INC = 3
	GOSUB SCREEN.FX

	READ.OUT$ = LTRIM$(STR$(NET#))
	GOSUB CHECK.READ.OUT
	TXT$ = READ.OUT$: INC = 5
	GOSUB SCREEN.FX

	READ.OUT$ = OLD.READ.OUT$

	IF COLORED = 1 THEN
	   COLOR 3, 1
	ELSE
	   COLOR 15, 0
	END IF
	GOSUB CLEAR.DESC
	LOCATE ROW% + 1, COL% + 1, 0: PRINT "Amount";
	LOCATE ROW% + 3, COL% + 1, 0: PRINT "Tax "; TAX.OP$; " "; TAX.RATE$; "%";
	LOCATE ROW% + 5, COL% + 1, 0: PRINT "Net";
	DISPLAY$ = "T"

	RETURN
	     
SCREEN.FX:

	LOCATE ROW% + INC, COL% + 14, 0: PRINT SPC(17);
	LOCATE ROW% + INC, COL% + 31 - LEN(TXT$), 0: PRINT TXT$;
	RETURN

CLEAR.DESC:
	
	LOCATE ROW% + 1, COL% + 1, 0: PRINT SPC(12);
	LOCATE ROW% + 3, COL% + 1, 0: PRINT SPC(12);
	LOCATE ROW% + 5, COL% + 1, 0: PRINT SPC(12);
	RETURN

CLEAR.DISPLAY:

	LOCATE ROW% + 1, COL% + 14, 0: PRINT SPC(17);
	LOCATE ROW% + 3, COL% + 14, 0: PRINT SPC(17);
	LOCATE ROW% + 5, COL% + 14, 0: PRINT SPC(17);
	RETURN

CHECK.READ.OUT:

	IF LEN(READ.OUT$) > 16 THEN
	   X1 = INSTR(READ.OUT$, ".")
	   IF X1 > 0 THEN
	      IF X1 = 17 THEN
		 READ.OUT$ = RIGHT$(READ.OUT$, 16)
	      ELSE
		 READ.OUT$ = LEFT$(READ.OUT$, 16)
	      END IF
	   END IF
	END IF
	RETURN

DspButtons:

	SELECT CASE RIGHT$(POSITION$, 1)
	   CASE "R"
	      SELECT CASE PY%
		 CASE IS = ROW% + 1
		    IF PX% >= 53 AND PX% <= 57 THEN W$ = "%": RETURN
		    IF PX% >= 60 AND PX% <= 64 THEN W$ = "/": RETURN
		    IF PX% >= 67 AND PX% <= 71 THEN W$ = "*": RETURN
		    IF PX% >= 74 AND PX% <= 78 THEN W$ = "-": RETURN
		 CASE IS = ROW% + 3
		    IF PX% >= 53 AND PX% <= 57 THEN W$ = "7": RETURN
		    IF PX% >= 60 AND PX% <= 64 THEN W$ = "8": RETURN
		    IF PX% >= 67 AND PX% <= 71 THEN W$ = "9": RETURN
		    IF PX% >= 74 AND PX% <= 78 THEN W$ = "+": RETURN
		 CASE IS = ROW% + 4
		    IF PX% >= 74 AND PX% <= 78 THEN W$ = "+": RETURN
		 CASE IS = ROW% + 5
		    IF PX% >= 53 AND PX% <= 57 THEN W$ = "4": RETURN
		    IF PX% >= 60 AND PX% <= 64 THEN W$ = "5": RETURN
		    IF PX% >= 67 AND PX% <= 71 THEN W$ = "6": RETURN
		    IF PX% >= 74 AND PX% <= 78 THEN W$ = "+": RETURN
		 CASE IS = ROW% + 7
		    IF PX% >= 47 AND PX% <= 51 THEN W$ = "C": RETURN
		    IF PX% >= 53 AND PX% <= 57 THEN W$ = "1": RETURN
		    IF PX% >= 60 AND PX% <= 64 THEN W$ = "2": RETURN
		    IF PX% >= 67 AND PX% <= 71 THEN W$ = "3": RETURN
		    IF PX% >= 74 AND PX% <= 78 THEN W$ = "=": RETURN
		 CASE IS = ROW% + 8
		    IF PX% >= 22 AND PX% <= 28 THEN W$ = CHR$(9): RETURN
		    IF PX% >= 32 AND PX% <= 36 THEN W$ = CHR$(27): RETURN
		    IF PX% >= 39 AND PX% <= 44 THEN W$ = CHR$(1): RETURN
		    IF PX% >= 74 AND PX% <= 78 THEN W$ = "=": RETURN
		 CASE IS = ROW% + 9
		    IF PX% >= 39 AND PX% <= 44 THEN W$ = CHR$(2): RETURN
		    IF PX% >= 47 AND PX% <= 51 THEN W$ = "E": RETURN
		    IF PX% >= 53 AND PX% <= 64 THEN W$ = "0": RETURN
		    IF PX% >= 67 AND PX% <= 71 THEN W$ = ".": RETURN
		    IF PX% >= 74 AND PX% <= 78 THEN W$ = "=": RETURN
		 CASE IS = ROW% + 10
		    IF PX% >= 39 AND PX% <= 44 THEN W$ = CHR$(3): RETURN
	      END SELECT
	   CASE "L"
	      SELECT CASE PY%
		 CASE IS = ROW% + 1
		    IF PX% >= 34 AND PX% <= 38 THEN W$ = "%": RETURN
		    IF PX% >= 41 AND PX% <= 45 THEN W$ = "/": RETURN
		    IF PX% >= 48 AND PX% <= 52 THEN W$ = "*": RETURN
		    IF PX% >= 55 AND PX% <= 59 THEN W$ = "-": RETURN
		 CASE IS = ROW% + 3
		    IF PX% >= 34 AND PX% <= 38 THEN W$ = "7": RETURN
		    IF PX% >= 41 AND PX% <= 45 THEN W$ = "8": RETURN
		    IF PX% >= 48 AND PX% <= 52 THEN W$ = "9": RETURN
		    IF PX% >= 55 AND PX% <= 59 THEN W$ = "+": RETURN
		 CASE IS = ROW% + 4
		    IF PX% >= 55 AND PX% <= 59 THEN W$ = "+": RETURN
		 CASE IS = ROW% + 5
		    IF PX% >= 34 AND PX% <= 38 THEN W$ = "4": RETURN
		    IF PX% >= 41 AND PX% <= 45 THEN W$ = "5": RETURN
		    IF PX% >= 48 AND PX% <= 52 THEN W$ = "6": RETURN
		    IF PX% >= 55 AND PX% <= 59 THEN W$ = "+": RETURN
		 CASE IS = ROW% + 7
		    IF PX% >= 28 AND PX% <= 32 THEN W$ = "C": RETURN
		    IF PX% >= 34 AND PX% <= 38 THEN W$ = "1": RETURN
		    IF PX% >= 41 AND PX% <= 45 THEN W$ = "2": RETURN
		    IF PX% >= 48 AND PX% <= 52 THEN W$ = "3": RETURN
		    IF PX% >= 55 AND PX% <= 59 THEN W$ = "=": RETURN
		 CASE IS = ROW% + 8
		    IF PX% >= 3 AND PX% <= 10 THEN W$ = CHR$(9): RETURN
		    IF PX% >= 13 AND PX% <= 17 THEN W$ = CHR$(27): RETURN
		    IF PX% >= 20 AND PX% <= 25 THEN W$ = CHR$(1): RETURN
		    IF PX% >= 55 AND PX% <= 59 THEN W$ = "=": RETURN
		 CASE IS = ROW% + 9
		    IF PX% >= 20 AND PX% <= 25 THEN W$ = CHR$(2): RETURN
		    IF PX% >= 28 AND PX% <= 32 THEN W$ = "E": RETURN
		    IF PX% >= 34 AND PX% <= 45 THEN W$ = "0": RETURN
		    IF PX% >= 48 AND PX% <= 52 THEN W$ = ".": RETURN
		    IF PX% >= 55 AND PX% <= 59 THEN W$ = "=": RETURN
		 CASE IS = ROW% + 10
		    IF PX% >= 20 AND PX% <= 25 THEN W$ = CHR$(3): RETURN
	      END SELECT
	END SELECT
	RETURN
END SUB

SUB CONFIG (W$, C.SCREEN, COLORS$, COMMS$, DISK$, FKEYS$, SFKEYS$, CFKEYS$, AFKEYS$, AUTO$, PASSWORD$, CALC.CONFIG$, BGCLR)
SHARED ERR.ROUTINE$

' In this procedure we call the configuration subsystem SCR-CNFG. A
' parameter is passed to the configuration subsystem to indicate what
' module is to be executed. The parameter is passed according to the
' user input which is in W$.

	ERR.ROUTINE$ = "CONFIG"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	PCOPY C.SCREEN, C.SCREEN + 1
	SCREEN , , C.SCREEN + 1, C.SCREEN + 1

	COLOR 15, BGCLR
	LOCATE 25, 1, 0
	PRINT SPACE$(80);
	LOCATE 25, 1, 0
	PRINT "Loading CONFIGURATION subsystem ... Please wait";

	LOCATE 1, 1, 0
	SELECT CASE W$
	   CASE COLORS$
	      SHELL "SCR-CNFG 1"
	   CASE COMMS$
	      SHELL "SCR-CNFG 2"
	   CASE DISK$
	      SHELL "SCR-CNFG 3"
	   CASE FKEYS$
	      SHELL "SCR-CNFG 4"
	   CASE SFKEYS$
	      SHELL "SCR-CNFG 5"
	   CASE CFKEYS$
	      SHELL "SCR-CNFG 6"
	   CASE AFKEYS$
	      SHELL "SCR-CNFG 7"
	   CASE AUTO$
	      SHELL "SCR-CNFG 8"
	   CASE PASSWORD$
	      SHELL "SCR-CNFG P"
	   CASE CALC.CONFIG$
	      SHELL "SCR-CNFG C"
	END SELECT
	SCREEN , , C.SCREEN, C.SCREEN

END SUB

SUB COPY.FILES (ERROR.NUM, FROM.DRIVE$, TO.DRIVE$, EXT.DRIVE$, IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, EXT.MIN.SP#, FMT.NAME$, FMT.VER$, FMT.NDX, BGCLR, DISPLAY.MODE$, C.SCREEN)
SHARED ERROR.CODE
SHARED ERR.ROUTINE$

' The function of this procedure is to copy files. The FROM file and
' the TO file are passed to it by the calling program. The procedure
' checks the available space in the distenation disk before attempting
' to copy the file. The available space is compared against the
' parameter EXT.MIN.SP#. If it is less than the value stored in the
' parameter EXT.MIN.SP# the procedure aborts with an error.
'
' The nature of the files to be copied is FORMAT files. Therefore the
' procedure copies the associated HELP and TRIG files to each format
' file.


	ERR.ROUTINE$ = "COPY.FILES"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	ERROR.CODE = 0
	IF C.SCREEN = 0 THEN
	   SCREEN , , 1, 1
	ELSE
	   IF DISPLAY.MODE$ = "B" THEN
	      SCREEN , , C.SCREEN, C.SCREEN
	   END IF
	END IF

	COLOR 15, BGCLR
	LOCATE 25, 1, 0
	PRINT SPACE$(80);
	LOCATE 25, 1, 0
	PRINT "Copying "; FMT.NAME$;
	IF TO.DRIVE$ = EXT.DRIVE$ THEN
	   AVAILABLE.SPACE# = 0
	   CALL DISK.SPACE(IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, AVAILABLE.SPACE#, LEFT$(EXT.DRIVE$, 2))
	   IF AVAILABLE.SPACE# <= EXT.MIN.SP# THEN
	      ERROR.NUM = 1
	      LOCATE 25, 1, 0
	      PRINT SPACE$(80);
	      EXIT SUB
	   END IF
	   PRINT " to the EXTERNAL drive";
	ELSE
	   PRINT " to the RAM drive";
	END IF

	IF C.SCREEN <> 0 THEN
	   IF DISPLAY.MODE$ = "B" THEN
	      SCREEN , , C.SCREEN + 1, C.SCREEN
	   END IF
	END IF

	FROM$ = FROM.DRIVE$ + FMT.NAME$ + "." + FMT.VER$
	TO$ = TO.DRIVE$ + FMT.NAME$ + "." + FMT.VER$

	OPEN FROM$ FOR INPUT AS #2

	IF ERROR.CODE <> 0 THEN
	   CLOSE #2
	   ERROR.NUM = ERROR.CODE
	   LOCATE 25, 1, 0
	   PRINT SPACE$(80);
	   EXIT SUB
	END IF

	OPEN TO$ FOR OUTPUT AS #4

	DO UNTIL EOF(2)
	   LINE INPUT #2, A$
	   PRINT #4, A$
	LOOP

	CLOSE #2, #4

	FROM$ = FROM.DRIVE$ + "HELP-" + LTRIM$(STR$(FMT.NDX)) + ".DAT"
	TO$ = TO.DRIVE$ + "HELP-" + LTRIM$(STR$(FMT.NDX)) + ".DAT"

	OPEN FROM$ FOR INPUT AS #2

	IF ERROR.CODE <> 0 THEN
	   CLOSE #2
	   ERROR.CODE = 0
	   GOTO COPY.TRIGS
	END IF

	OPEN TO$ FOR OUTPUT AS #4

	DO UNTIL EOF(2)
	   LINE INPUT #2, A$
	   PRINT #4, A$
	LOOP

	CLOSE #2, #4

COPY.TRIGS:

	FROM$ = FROM.DRIVE$ + "TRIG-" + LTRIM$(STR$(FMT.NDX)) + ".DAT"
	TO$ = TO.DRIVE$ + "TRIG-" + LTRIM$(STR$(FMT.NDX)) + ".DAT"

	OPEN FROM$ FOR INPUT AS #2

	IF ERROR.CODE <> 0 THEN
	   CLOSE #2
	   ERROR.CODE = 0
	   LOCATE 25, 1, 0
	   PRINT SPACE$(80);
	   EXIT SUB
	END IF

	OPEN TO$ FOR OUTPUT AS #4

	DO UNTIL EOF(2)
	   LINE INPUT #2, A$
	   PRINT #4, A$
	LOOP

	CLOSE #2, #4

	LOCATE 25, 1, 0
	PRINT SPACE$(80);

END SUB

SUB COPY.TO.EXT (EXT.DRIVE$, R.FREQ, FMT.START, FMT.COUNT, FORMATS$(), RAM.DRIVE$, ERROR.CODE, IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, EXT.MIN.SP#, BGCLR, DISPLAY.MODE$, C.SCREEN)
SHARED ERR.ROUTINE$

' This procedure calls the COPY.FILES procedure to copy the format
' files from the RAM drive to the EXTERNAL drive. The procedure sets
' the FROM.DRIVE$ parameter to the RAM drive and the TO.DRIVE$
' parameter to the EXTERNAL drive, before calling COPY.FILES procedure.
'
' After the copying is over for each format file, the procedure
' updates the directory file SCR-MNGR.DIR, which resides in the
' external drive, to contain the necessary information for each format
' file. The details of the information stored in the SCR-MNGR.DIR file
' in discussed in details in program SCR-MNGR.BAS in routine INIT1000.


	ERR.ROUTINE$ = "COPY.TO.EXT"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE

	CHANGE.MADE = 0
	DIR.FILE = FREEFILE
	OPEN EXT.DRIVE$ + "SCR-MNGR.DIR" FOR RANDOM AS DIR.FILE

	IF LOF(DIR.FILE) = 0 THEN
	   CLOSE #DIR.FILE
	   DIR.RECORD$ = ""
	ELSE
	   CLOSE #DIR.FILE
	   OPEN EXT.DRIVE$ + "SCR-MNGR.DIR" FOR INPUT AS DIR.FILE
	   LINE INPUT #DIR.FILE, DIR.RECORD$
	   CLOSE #DIR.FILE
	END IF

	NEW.FMT = 0
	FOR I = FMT.START TO FMT.COUNT
	   IF MID$(FORMATS$(I), 11, 1) = "N" THEN
	      NEW.FMT = NEW.FMT + 1
	   END IF
	NEXT I

	IF NEW.FMT >= R.FREQ THEN
	   NEW.FMT = 0
	   FOR I = FMT.START TO FMT.COUNT
	      IF MID$(FORMATS$(I), 11, 1) = "N" THEN
		 FMT.DESC$ = FORMATS$(I)
		 FMT.NAME$ = LEFT$(FMT.DESC$, 6)
		 FMT.VER$ = MID$(FMT.DESC$, 7, 2)
		 FMT.NDX = I
		 FMT.NDX$ = LTRIM$(STR$(I))
		 FMT.NDX$ = STRING$(3 - LEN(FMT.NDX$), "0") + FMT.NDX$
		 FMT.RECORD$ = FMT.NAME$ + FMT.VER$ + FMT.NDX$
		 ERROR.CODE = 0
		 FROM.DRIVE$ = RAM.DRIVE$
		 TO.DRIVE$ = EXT.DRIVE$
		 AVAILABLE.SPACE# = 0
		 CALL COPY.FILES(ERROR.CODE, FROM.DRIVE$, TO.DRIVE$, EXT.DRIVE$, IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, EXT.MIN.SP#, FMT.NAME$, FMT.VER$, FMT.NDX, BGCLR, DISPLAY.MODE$, C.SCREEN)
		 IF ERROR.CODE <> 0 THEN
		    ERROR.CODE = 0
		    EXIT SUB
		 END IF
		 MID$(FORMATS$(I), 11, 1) = "Y"
		 X = INSTR(DIR.RECORD$, FMT.NAME$)
		 IF X > 0 THEN
		    OLD.FMT$ = MID$(DIR.RECORD$, X, 8)
		    IF OLD.FMT$ <> FMT.NAME$ + FMT.VER$ THEN
		       L.DIR.RECORD$ = LEFT$(DIR.RECORD$, X - 1)
		       R.DIR.RECORD$ = RIGHT$(DIR.RECORD$, LEN(DIR.RECORD$) - (X + 10))
		       DIR.RECORD$ = L.DIR.RECORD$ + R.DIR.RECORD$
		       KILL EXT.DRIVE$ + LTRIM$(LEFT$(OLD.FMT$, 6)) + "." + RIGHT$(OLD.FMT$, 2)
		    END IF
		 END IF
		 DIR.RECORD$ = DIR.RECORD$ + FMT.RECORD$
		 CHANGE.MADE = 1
	      END IF

NEXT.COPY.STEP:

	   NEXT I
	END IF
	IF CHANGE.MADE = 1 THEN
	   DIR.FILE = FREEFILE
	   OPEN EXT.DRIVE$ + "SCR-MNGR.DIR" FOR OUTPUT AS DIR.FILE
	   PRINT #DIR.FILE, DIR.RECORD$;
	   CLOSE DIR.FILE
	END IF
END SUB

SUB COPY.TO.RAM (ERROR.CODE, FMT.COUNT, DIR.RECORD$, EXT.DRIVE$, RAM.DRIVE$, FORMATS$(), IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, MIN.SPACE#, BGCLR, DISPLAY.MODE$, C.SCREEN)
SHARED ERR.ROUTINE$

' This procedure calls the COPY.FILES procedure to copy the format
' files from the EXTERNAL drive to the RAM drive. The procedure sets
' the FROM.DRIVE$ parameter to the EXTERNAL drive and the TO.DRIVE$
' parameter to the RAM drive, before calling COPY.FILES procedure.
' The procedure uses the information stored in SCR-MNGR.DIR file to
' copy the formats files. The details of the information stored in the
' SCR-MNGR.DIR file in discussed in details in program SCR-MNGR.BAS in
' routine INIT1000.

	ERR.ROUTINE$ = "COPY.TO.RAM"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE

	ERROR.CODE = 0
	FMT.COUNT = 0
	FOR I = 1 TO LEN(DIR.RECORD$) STEP 11
	   FMT.DESC$ = MID$(DIR.RECORD$, I, 11)
	   FMT.NAME$ = LEFT$(FMT.DESC$, 6)
	   FMT.VER$ = MID$(FMT.DESC$, 7, 2)
	   FMT.NDX = VAL(RIGHT$(FMT.DESC$, 3))
	   FROM.DRIVE$ = EXT.DRIVE$
	   TO.DRIVE$ = RAM.DRIVE$
	   CALL COPY.FILES(ERROR.CODE, FROM.DRIVE$, TO.DRIVE$, EXT.DRIVE$, IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, EXT.MIN.SP#, FMT.NAME$, FMT.VER$, FMT.NDX, BGCLR, DISPLAY.MODE$, C.SCREEN)
	   IF ERROR.CODE <> 0 THEN
	      ERROR.CODE = 0
	      EXIT SUB
	   END IF
	   AVAILABLE.SPACE# = 0
	   CALL DISK.SPACE(IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, AVAILABLE.SPACE#, LEFT$(RAM.DRIVE$, 2))
	   IF AVAILABLE.SPACE# <= MIN.SPACE# THEN
	      EXIT SUB
	   END IF
	   FORMATS$(FMT.NDX) = FMT.NAME$ + FMT.VER$ + "01" + "Y"
	   FMT.COUNT = FMT.COUNT + 1
	NEXT I

END SUB

SUB CREATE.HELP.FILE (RAM.DRIVE$, FMT.COUNT, HELP.FILE.NUMBER$, HELP.FILE.OPEN, FMT.DESC$, LAST.FIELD.NUM, R$)
SHARED ERR.ROUTINE$

' This procedure createsd the help file that is associated with a
' format file. The help is created only when there is help text in the
' format file. The help text is always preceded by "HI". The
' programmer is allowed to insert a help text for every input field on
' the screen. The help text that is associated with an input field
' should always be after the definition of the input field in the
' source program.
'
' The help file that is created is a sequential file. Each logical
' record in the help file is made of 6 physical records. The first
' record has the character ? as the first byte and the field number
' follwes it. The next five lines are made of 40 characters each.
' These lines make up the help text.

	ERR.ROUTINE$ = "CREATE.HELP.FILE"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE

	HELP.FILE.NUMBER$ = LTRIM$(STR$(FMT.COUNT + 1))
	CALL OPEN.HELP.FILE(RAM.DRIVE$, HELP.FILE.NUMBER$, HELP.FILE.OPEN)
	R1$ = "?" + LTRIM$(STR$(LAST.FIELD.NUM))
	PRINT #6, R1$
	PRINT #6, RIGHT$(R$, LEN(R$) - 3)

HELP.LOOP:
	J = INSTR(FMT.DESC$, "~~~")
	IF J > 0 THEN
	   R$ = MID$(FMT.DESC$, 1, J - 1)
	   FMT.DESC$ = MID$(FMT.DESC$, J + 3, LEN(FMT.DESC$) - J + 3)
	   IF LEFT$(R$, 3) <> "HI," AND LEFT$(R$, 3) <> "TG," THEN
	      PRINT #5, R$
	   END IF
	   IF LEFT$(R$, 3) = "HI," THEN
	      PRINT #6, RIGHT$(R$, LEN(R$) - 3)
	      GOTO HELP.LOOP
	   END IF
	END IF
	CLOSE #6
	HELP.FILE.OPEN = 0

END SUB

SUB CREATE.TRIGGER.FILE (RAM.DRIVE$, FMT.COUNT, TRIGGER.FILE.NUMBER$, TRIGGER.FILE.OPEN, FMT.DESC$, R$)
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "CREATE.TRIGGER.FILE"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	TRIGGER.FILE.NUMBER$ = LTRIM$(STR$(FMT.COUNT + 1))
	CALL OPEN.TRIGGER.FILE(RAM.DRIVE$, TRIGGER.FILE.NUMBER$, TRIGGER.FILE.OPEN)
	PRINT #7, RIGHT$(R$, LEN(R$) - 3)

TRIGGER.LOOP:

	J = INSTR(FMT.DESC$, "~~~")
	IF J > 0 THEN
	   R$ = MID$(FMT.DESC$, 1, J - 1)
	   FMT.DESC$ = MID$(FMT.DESC$, J + 3, LEN(FMT.DESC$) - J + 3)
	   IF LEFT$(R$, 3) <> "HI," AND LEFT$(R$, 3) <> "TG," THEN
	      PRINT #5, R$
	   END IF
	   IF LEFT$(R$, 3) = "TG," THEN
	      PRINT #7, RIGHT$(R$, LEN(R$) - 3)
	      GOTO TRIGGER.LOOP
	   END IF
	END IF
	CLOSE #7
	TRIGGER.FILE.OPEN = 0

END SUB

SUB DECIMAL.TO.BINARY (X%, B$)

		Y = 1
		Z = 1
		B$ = ""

		DO UNTIL (Y = 0 AND Z = 0)
		   Z = INT(X% / 2)
		   Y = X% MOD 2
		   IF Y > 0 THEN
		      B$ = B$ + "1"
		   ELSE
		      B$ = B$ + "0"
		   END IF
		   X% = Z
		LOOP
		B$ = LEFT$(B$, LEN(B$) - 1)
		FOR I = LEN(B$) TO 1 STEP -1
		    B2$ = B2$ + MID$(B$, I, 1)
		NEXT I
		B$ = B2$
		B$ = STRING$(15 - LEN(B$), "0") + B$

END SUB

SUB DECOMPRESS.FORMAT.FILE (RAM.DRIVE$, FMT$, TRIGGER.FILE.OPEN, HELP.FILE.OPEN, HELP.FILE.NUMBER$, TRIGGER.FILE.NUMBER$, FMT.COUNT, MAX.FIELDS, CLIENT.VER$)
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "DECOMPRESS.FORMAT.FILE"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	SC$ = CHR$(1)
	si$ = CHR$(2)
	SO$ = CHR$(3)
	MI$ = CHR$(4)
	MO$ = CHR$(5)
	CU$ = CHR$(6)
	HI$ = CHR$(7)
	TG$ = CHR$(8)

	O$ = RAM.DRIVE$ + FMT$ + ".FMT"
	OPEN RAM.DRIVE$ + FMT$ + "." + CLIENT.VER$ FOR RANDOM AS #3 LEN = 1
	OPEN O$ FOR OUTPUT AS #4
	FIELD #3, 1 AS INPUT.BYTE$
	END.OF.FILE = LOF(3)
	POINTER = 0

READ.LOOP:

	POINTER = POINTER + 1

	IF POINTER > END.OF.FILE THEN
	   CLOSE #3, #4, #6, #7
	   TRIGGER.FILE.OPEN = 0
	   HELP.FILE.OPEN = 0
	   KILL RAM.DRIVE$ + FMT$ + "." + CLIENT.VER$
	   NAME O$ AS RAM.DRIVE$ + FMT$ + "." + CLIENT.VER$
	   EXIT SUB
	END IF

	GET #3, POINTER

	GOSUB DECOMPRESS
	GOTO READ.LOOP
	EXIT SUB

DECOMPRESS:

	OUTPUT.RECORD$ = ""
	IF INPUT.BYTE$ = SC$ THEN
	   GOSUB PROCESS.SC
	   RETURN
	END IF

	IF INPUT.BYTE$ = si$ OR INPUT.BYTE$ = SO$ THEN
	   GOSUB PROCESS.SI.SO
	   RETURN
	END IF

	IF INPUT.BYTE$ = MI$ OR INPUT.BYTE$ = MO$ THEN
	   GOSUB PROCESS.MI.MO
	   RETURN
	END IF

	IF INPUT.BYTE$ = CU$ THEN
	   GOSUB PROCESS.CU
	   RETURN
	END IF

	IF INPUT.BYTE$ = HI$ THEN
	   IF HELP.FILE.OPEN = 0 THEN
	      HELP.FILE.NUMBER$ = LTRIM$(STR$(FMT.COUNT))
	      CALL OPEN.HELP.FILE(RAM.DRIVE$, HELP.FILE.NUMBER$, HELP.FILE.OPEN)
	   END IF
	   GOSUB PROCESS.HI
	   RETURN
	END IF

	IF INPUT.BYTE$ = TG$ THEN
	   IF TRIGGER.FILE.OPEN = 0 THEN
	      TRIGGER.FILE.NUMBER$ = LTRIM$(STR$(FMT.COUNT))
	      CALL OPEN.TRIGGER.FILE(RAM.DRIVE$, TRIGGER.FILE.NUMBER$, TRIGGER.FILE.OPEN)
	   END IF
	   GOSUB PROCESS.TG
	   RETURN
	END IF

	BEEP
	SCREEN , , 0, 0
	COLOR 15, 0
	CLS
	PRINT "FILE ERROR ... OUT OF SYNC"
	CLOSE
	END

PROCESS.SC:

	OUTPUT.RECORD$ = "SC,"

	POINTER = POINTER + 1
	GET #3, POINTER
	ROW$ = LTRIM$(STR$(ASC(INPUT.BYTE$)))
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + ROW$ + ","

	POINTER = POINTER + 1
	GET #3, POINTER
	COL$ = LTRIM$(STR$(ASC(INPUT.BYTE$)))
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + COL$ + ","

	POINTER = POINTER + 1
	GET #3, POINTER
	FOR I = 1 TO ASC(INPUT.BYTE$)
	   POINTER = POINTER + 1
	   GET #3, POINTER
	   OUTPUT.RECORD$ = OUTPUT.RECORD$ + INPUT.BYTE$
	NEXT I
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + ","

	SP$ = ""
	POINTER = POINTER + 1
	GET #3, POINTER
	SP$ = SPACE$(ASC(INPUT.BYTE$))
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + SP$

GET.SC.TEXT:

	POINTER = POINTER + 1
	GET #3, POINTER

	IF INPUT.BYTE$ = CHR$(13) THEN
	   POINTER = POINTER + 1
	   GET #3, POINTER
	   PRINT #4, OUTPUT.RECORD$
	   OUTPUT.RECORD$ = ""
	   RETURN
	END IF

	OUTPUT.RECORD$ = OUTPUT.RECORD$ + INPUT.BYTE$
	GOTO GET.SC.TEXT

PROCESS.SI.SO:

	IF INPUT.BYTE$ = si$ THEN
	   OUTPUT.RECORD$ = "SI,"
	ELSE
	   OUTPUT.RECORD$ = "SO,"
	END IF

	POINTER = POINTER + 1
	GET #3, POINTER
	ROW$ = LTRIM$(STR$(ASC(INPUT.BYTE$)))
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + ROW$ + ","

	POINTER = POINTER + 1
	GET #3, POINTER
	COL$ = LTRIM$(STR$(ASC(INPUT.BYTE$)))
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + COL$ + ","

	POINTER = POINTER + 1
	GET #3, POINTER
	HUNDREDS = ASC(INPUT.BYTE$)

	POINTER = POINTER + 1
	GET #3, POINTER
	ONES = ASC(INPUT.BYTE$)

	FEELD.NUM = HUNDREDS * 100 + ONES
	FLN$ = LTRIM$(STR$(FEELD.NUM))
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + FLN$ + ","

	POINTER = POINTER + 1
	GET #3, POINTER
	FOR I = 1 TO ASC(INPUT.BYTE$)
	   POINTER = POINTER + 1
	   GET #3, POINTER
	   OUTPUT.RECORD$ = OUTPUT.RECORD$ + INPUT.BYTE$
	NEXT I
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + ","

	POINTER = POINTER + 1
	GET #3, POINTER
	FOR I = 1 TO ASC(INPUT.BYTE$)
	   POINTER = POINTER + 1
	   GET #3, POINTER
	   OUTPUT.RECORD$ = OUTPUT.RECORD$ + INPUT.BYTE$
	NEXT I
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + ","

	POINTER = POINTER + 1
	GET #3, POINTER
	HUNDREDS = ASC(INPUT.BYTE$)

	POINTER = POINTER + 1
	GET #3, POINTER
	ONES = ASC(INPUT.BYTE$)

	JUMP.FLAG = 0

	IF ONES = 13 THEN
	   JUMP.FLAG = 1
	   POINTER = POINTER + 1
	   GET #3, POINTER
	   IF INPUT.BYTE$ = CHR$(10) THEN
	      ONES = 32
	   ELSE
	      POINTER = POINTER + 1
	      GET #3, POINTER
	   END IF
	END IF

	FEELD.NUM = HUNDREDS * 100 + ONES

	IF LEFT$(OUTPUT.RECORD$, 3) = "SI," AND FEELD.NUM < MAX.FIELDS THEN
	   LAST.FIELD.NUM = FEELD.NUM
	   R1$ = LTRIM$(STR$(LAST.FIELD.NUM))
	END IF

	FLN$ = LTRIM$(STR$(FEELD.NUM))
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + FLN$

	PRINT #4, OUTPUT.RECORD$
	OUTPUT.RECORD$ = ""

	IF JUMP.FLAG = 1 THEN
	   JUMP.FLAG = 0
	   RETURN
	END IF

	POINTER = POINTER + 1
	GET #3, POINTER
	POINTER = POINTER + 1
	GET #3, POINTER
	RETURN

PROCESS.MI.MO:

	IF INPUT.BYTE$ = MI$ THEN
	   OUTPUT.RECORD$ = "MI,"
	ELSE
	   OUTPUT.RECORD$ = "MO,"
	END IF

	POINTER = POINTER + 1
	GET #3, POINTER
	HUNDREDS = ASC(INPUT.BYTE$)

	POINTER = POINTER + 1
	GET #3, POINTER
	ONES = ASC(INPUT.BYTE$)

	FEELD.NUM = HUNDREDS * 100 + ONES
	FLN$ = LTRIM$(STR$(FEELD.NUM))
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + FLN$ + ","

	POINTER = POINTER + 1
	GET #3, POINTER
	HUNDREDS = ASC(INPUT.BYTE$)

	POINTER = POINTER + 1
	GET #3, POINTER
	ONES = ASC(INPUT.BYTE$)

	FEELD.NUM = HUNDREDS * 100 + ONES
	FLN$ = LTRIM$(STR$(FEELD.NUM))

	IF LEFT$(OUTPUT.RECORD$, 3) <> "MO," THEN
	   OUTPUT.RECORD$ = OUTPUT.RECORD$ + FLN$ + ","
	   POINTER = POINTER + 1
	   GET #3, POINTER
	   FOR I = 1 TO ASC(INPUT.BYTE$)
	      POINTER = POINTER + 1
	      GET #3, POINTER
	      OUTPUT.RECORD$ = OUTPUT.RECORD$ + INPUT.BYTE$
	   NEXT I
	ELSE
	   OUTPUT.RECORD$ = OUTPUT.RECORD$ + FLN$
	END IF

	PRINT #4, OUTPUT.RECORD$
	OUTPUT.RECORD$ = ""

	'
	' Get the carrage return and line feed characters that mark
	' the end of the record.
	'

	POINTER = POINTER + 1
	GET #3, POINTER
	POINTER = POINTER + 1
	GET #3, POINTER
	RETURN

PROCESS.CU:

	OUTPUT.RECORD$ = "CU,"

	POINTER = POINTER + 1
	GET #3, POINTER
	HUNDREDS = ASC(INPUT.BYTE$)

	POINTER = POINTER + 1
	GET #3, POINTER
	ONES = ASC(INPUT.BYTE$)

	FEELD.NUM = HUNDREDS * 100 + ONES
	FLN$ = LTRIM$(STR$(FEELD.NUM))
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + FLN$

	PRINT #4, OUTPUT.RECORD$
	OUTPUT.RECORD$ = ""

	POINTER = POINTER + 1
	GET #3, POINTER
	POINTER = POINTER + 1
	GET #3, POINTER
	RETURN

PROCESS.HI:

	PRINT #6, "?"; R1$

	FOR J = 1 TO 5
	   OUTPUT.RECORD$ = "HI,"
	   FOR I = 1 TO 42
	      POINTER = POINTER + 1
	      GET #3, POINTER
	      IF INPUT.BYTE$ = CHR$(13) THEN
		 POINTER = POINTER + 1
		 GET #3, POINTER
		 I = 43
	      ELSE
		 IF I > 1 OR (INPUT.BYTE$ <> CHR$(7) AND INPUT.BYTE$ <> CHR$(10) AND INPUT.BYTE$ <> CHR$(13)) THEN
		    OUTPUT.RECORD$ = OUTPUT.RECORD$ + INPUT.BYTE$
		 END IF
	      END IF
	   NEXT I
	   PRINT #6, RIGHT$(OUTPUT.RECORD$, LEN(OUTPUT.RECORD$) - 3)
	   OUTPUT.RECORD$ = ""
	NEXT J
	RETURN

PROCESS.TG:

	OUTPUT.RECORD$ = "TG,"

	POINTER = POINTER + 1
	GET #3, POINTER
	HUNDREDS = ASC(INPUT.BYTE$)

	POINTER = POINTER + 1
	GET #3, POINTER
	ONES = ASC(INPUT.BYTE$)

	TRIG.NUM = HUNDREDS * 100 + ONES
	TGN$ = LTRIM$(STR$(TRIG.NUM))
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + TGN$ + ","

	SP$ = ""
	POINTER = POINTER + 1
	GET #3, POINTER
	SP$ = SPACE$(ASC(INPUT.BYTE$))
	OUTPUT.RECORD$ = OUTPUT.RECORD$ + SP$

GET.TG.TEXT:

	POINTER = POINTER + 1
	GET #3, POINTER

	IF INPUT.BYTE$ = CHR$(13) THEN
	   POINTER = POINTER + 1
	   GET #3, POINTER
	   PRINT #7, RIGHT$(OUTPUT.RECORD$, LEN(OUTPUT.RECORD$) - 3)
	   OUTPUT.RECORD$ = ""
	   RETURN
	END IF

	OUTPUT.RECORD$ = OUTPUT.RECORD$ + INPUT.BYTE$
	GOTO GET.TG.TEXT

END SUB

SUB DISK.SPACE (IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, AVAILABLE.SPACE#, DRIVE$)
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "DISK.SPACE"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	IN.ARRAY%(ax) = &H3600
	SELECT CASE DRIVE$
	   CASE "A:"
	      IN.ARRAY%(dx) = &H1
	   CASE "B:"
	      IN.ARRAY%(dx) = &H2
	   CASE "C:"
	      IN.ARRAY%(dx) = &H3
	   CASE "D:"
	      IN.ARRAY%(dx) = &H4
	   CASE "E:"
	      IN.ARRAY%(dx) = &H5
	   CASE "F:"
	      IN.ARRAY%(dx) = &H6
	   CASE "G:"
	      IN.ARRAY%(dx) = &H7
	   CASE "H:"
	      IN.ARRAY%(dx) = &H8
	   CASE "I:"
	      IN.ARRAY%(dx) = &H9
	   CASE "J:"
	      IN.ARRAY%(dx) = &HA
	   CASE "K:"
	      IN.ARRAY%(dx) = &HB
	   CASE "L:"
	      IN.ARRAY%(dx) = &HC
	   CASE "M:"
	      IN.ARRAY%(dx) = &HD
	END SELECT

	CALL INT86OLD(&H21, IN.ARRAY%(), OUT.ARRAY%())

	W$ = HEX$(OUT.ARRAY%(dx))
	CLUSTERS = HEX.TO.DECIMAL(W$)

	W$ = HEX$(OUT.ARRAY%(bx))
	AVAILABLE.CLUSTERS = HEX.TO.DECIMAL(W$)

	W$ = HEX$(OUT.ARRAY%(ax))
	SECTORS = HEX.TO.DECIMAL(W$)

	W$ = HEX$(OUT.ARRAY%(cx))
	BYTES.PER.SECTOR = HEX.TO.DECIMAL(W$)

	AVAILABLE.SPACE# = AVAILABLE.CLUSTERS * SECTORS * BYTES.PER.SECTOR

END SUB

SUB DISPLAY.ERROR.MESSAGE (ERROR.CODE, ERR.ROUTINE$)
	CLOSE
	OPEN "ERR-MSGS.DAT" FOR RANDOM AS #1 LEN = 30
	FIELD #1, 30 AS ERROR.MESSAGE$
	GET #1, ERROR.CODE
	SCREEN , , 0, 0
	COLOR 15, BGCLR
	CLS
	PRINT "ERROR = "; ERROR.CODE
	PRINT "ERROR MESSAGE = "; ERROR.MESSAGE$
	PRINT "Error occured in routine "; ERR.ROUTINE$
	END
END SUB

SUB DISPLAY.FUNCTION.KEYS (C.SCREEN)
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "DISPLAY.FUNCTION.KEYS"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	PCOPY C.SCREEN, C.SCREEN + 1
	SCREEN , , C.SCREEN + 1, C.SCREEN + 1
	COLOR 0, 3
	CLS
	LOCATE 1, 1, 0: PRINT "F1   =                                  ^F1  =                                  ";
	LOCATE 2, 1, 0: PRINT "F2   =                                  ^F2  =                                  ";
	LOCATE 3, 1, 0: PRINT "F3   =                                  ^F3  =                                  ";
	LOCATE 4, 1, 0: PRINT "F4   =                                  ^F4  =                                  ";
	LOCATE 5, 1, 0: PRINT "F5   =                                  ^F5  =                                  ";
	LOCATE 6, 1, 0: PRINT "F6   =                                  ^F6  =                                  ";
	LOCATE 7, 1, 0: PRINT "F7   =                                  ^F7  =                                  ";
	LOCATE 8, 1, 0: PRINT "F8   =                                  ^F8  =                                  ";
	LOCATE 9, 1, 0: PRINT "F9   =                                  ^F9  =                                  ";
	LOCATE 10, 1, 0: PRINT "F10  =                                  ^F10 =                                  ";
	LOCATE 11, 1, 0: PRINT "F11  =                                  ^F11 =                                  ";
	LOCATE 12, 1, 0: PRINT "F12  =                                  ^F12 =                                  ";
	LOCATE 13, 1, 0: PRINT "#F1  =                                  @F1  =                                  ";
	LOCATE 14, 1, 0: PRINT "#F2  =                                  @F2  =                                  ";
	LOCATE 15, 1, 0: PRINT "#F3  =                                  @F3  =                                  ";
	LOCATE 16, 1, 0: PRINT "#F4  =                                  @F4  =                                  ";
	LOCATE 17, 1, 0: PRINT "#F5  =                                  @F5  =                                  ";
	LOCATE 18, 1, 0: PRINT "#F6  =                                  @F6  =                                  ";
	LOCATE 19, 1, 0: PRINT "#F7  =                                  @F7  =                                  ";
	LOCATE 20, 1, 0: PRINT "#F8  =                                  @F8  =                                  ";
	LOCATE 21, 1, 0: PRINT "#F9  =                                  @F9  =                                  ";
	LOCATE 22, 1, 0: PRINT "#F10 =                                  @F10 =                                  ";
	LOCATE 23, 1, 0: PRINT "#F11 =                                  @F11 =                                  ";
	LOCATE 24, 1, 0: PRINT "#F12 =                                  @F12 =                                  ";
	COLOR 15, 4
	LOCATE 25, 1, 0: PRINT "Function Keys Assignments   # -> SHIFT   ^ -> CTRL   @ -> ALT    Esc to exit    ";
	FILENUM = FREEFILE
	OPEN "SCR-MNGR.INI" FOR INPUT AS FILENUM
	DO UNTIL EOF(FILENUM)
	   LINE INPUT #FILENUM, A$
	   IF LEFT$(A$, 1) = "*" THEN
	      GOTO GET.NEXT.CONFIG.RECORD
	   END IF
	   SP = INSTR(A$, " ")
	   EQ = INSTR(A$, "=")
	   LA$ = LTRIM$(LEFT$(A$, SP - 1))
	   RA$ = LTRIM$(RIGHT$(A$, LEN(A$) - EQ))

	   IF LEN(RA$) > 32 THEN
	      RA$ = LEFT$(RA$, 32)
	   END IF

	   COLOR 15, 3

	   SELECT CASE LA$
	      CASE "F1"
		 LOCATE 1, 8
		 PRINT RA$;
	      CASE "F2"
		 LOCATE 2, 8
		 PRINT RA$;
	      CASE "F3"
		 LOCATE 3, 8
		 PRINT RA$;
	      CASE "F4"
		 LOCATE 4, 8
		 PRINT RA$;
	      CASE "F5"
		 LOCATE 5, 8
		 PRINT RA$;
	      CASE "F6"
		 LOCATE 6, 8
		 PRINT RA$;
	      CASE "F7"
		 LOCATE 7, 8
		 PRINT RA$;
	      CASE "F8"
		 LOCATE 8, 8
		 PRINT RA$;
	      CASE "F9"
		 LOCATE 9, 8
		 PRINT RA$;
	      CASE "F10"
		 LOCATE 10, 8
		 PRINT RA$;
	      CASE "F11"
		 LOCATE 11, 8
		 PRINT RA$;
	      CASE "F12"
		 LOCATE 12, 8
		 PRINT RA$;
	      CASE "#F1"
		 LOCATE 13, 8
		 PRINT RA$;
	      CASE "#F2"
		 LOCATE 14, 8
		 PRINT RA$;
	      CASE "#F3"
		 LOCATE 15, 8
		 PRINT RA$;
	      CASE "#F4"
		 LOCATE 16, 8
		 PRINT RA$;
	      CASE "#F5"
		 LOCATE 17, 8
		 PRINT RA$;
	      CASE "#F6"
		 LOCATE 18, 8
		 PRINT RA$;
	      CASE "#F7"
		 LOCATE 19, 8
		 PRINT RA$;
	      CASE "#F8"
		 LOCATE 20, 8
		 PRINT RA$;
	      CASE "#F9"
		 LOCATE 21, 8
		 PRINT RA$;
	      CASE "#F10"
		 LOCATE 22, 8
		 PRINT RA$;
	      CASE "#F11"
		 LOCATE 23, 8
		 PRINT RA$;
	      CASE "#F12"
		 LOCATE 24, 8
		 PRINT RA$;
	      CASE "^F1"
		 LOCATE 1, 48
		 PRINT RA$;
	      CASE "^F2"
		 LOCATE 2, 48
		 PRINT RA$;
	      CASE "^F3"
		 LOCATE 3, 48
		 PRINT RA$;
	      CASE "^F4"
		 LOCATE 4, 48
		 PRINT RA$;
	      CASE "^F5"
		 LOCATE 5, 48
		 PRINT RA$;
	      CASE "^F6"
		 LOCATE 6, 48
		 PRINT RA$;
	      CASE "^F7"
		 LOCATE 7, 48
		 PRINT RA$;
	      CASE "^F8"
		 LOCATE 8, 48
		 PRINT RA$;
	      CASE "^F9"
		 LOCATE 9, 48
		 PRINT RA$;
	      CASE "^F10"
		 LOCATE 10, 48
		 PRINT RA$;
	      CASE "^F11"
		 LOCATE 11, 48
		 PRINT RA$;
	      CASE "^F12"
		 LOCATE 12, 48
		 PRINT RA$;
	      CASE "@F1"
		 LOCATE 13, 48
		 PRINT RA$;
	      CASE "@F2"
		 LOCATE 14, 48
		 PRINT RA$;
	      CASE "@F3"
		 LOCATE 15, 48
		 PRINT RA$;
	      CASE "@F4"
		 LOCATE 16, 48
		 PRINT RA$;
	      CASE "@F5"
		 LOCATE 17, 48
		 PRINT RA$;
	      CASE "@F6"
		 LOCATE 18, 48
		 PRINT RA$;
	      CASE "@F7"
		 LOCATE 19, 48
		 PRINT RA$;
	      CASE "@F8"
		 LOCATE 20, 48
		 PRINT RA$;
	      CASE "@F9"
		 LOCATE 21, 48
		 PRINT RA$;
	      CASE "@F10"
		 LOCATE 22, 48
		 PRINT RA$;
	      CASE "@F11"
		 LOCATE 23, 48
		 PRINT RA$;
	      CASE "@F12"
		 LOCATE 24, 48
		 PRINT RA$;
	   END SELECT

GET.NEXT.CONFIG.RECORD:

	LOOP
	CLOSE #FILENUM

	DO
	   AA$ = INPUT$(1)
	LOOP WHILE AA$ <> CHR$(27)
	SCREEN , , C.SCREEN, C.SCREEN
END SUB

SUB DISPLAY.WINDOW (NX, NY, C.SCREEN)
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "DISPLAY.WINDOW"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	NX = CSRLIN
	NY = POS(S)

	LOCATE 5, 1, 0
	PCOPY C.SCREEN, C.SCREEN + 1
	SCREEN , , C.SCREEN + 1, C.SCREEN + 1

	COLOR 15, 4
	LOCATE 7, 18, 0: PRINT "ษออออออออออออออออออออออออออออออออออออออออออป";
	LOCATE 8, 18, 0: PRINT "บ                                          บ";
	LOCATE 9, 18, 0: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ";
	LOCATE 10, 18, 0: PRINT "บ                                          บ";
	LOCATE 11, 18, 0: PRINT "บ                                          บ";
	LOCATE 12, 18, 0: PRINT "บ                                          บ";
	LOCATE 13, 18, 0: PRINT "บ                                          บ";
	LOCATE 14, 18, 0: PRINT "บ                                          บ";
	LOCATE 15, 18, 0: PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ";
	LOCATE 16, 18, 0: PRINT "บ                                          บ";
	LOCATE 17, 18, 0: PRINT "ศออออออออออออออออออออออออออออออออออออออออออผ";
	CALL PUT.SHADOW(7, 18, 44, 11, "บ")

END SUB

SUB DOS (C.SCREEN)
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "DOS"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	PCOPY C.SCREEN, C.SCREEN + 1
	SCREEN , , C.SCREEN + 1, C.SCREEN + 1
	CLS
	COLOR 15, 4
	PRINT "Type 'EXIT' and press ฤฤู to return to client"
	SHELL "COMMAND"
	SCREEN , , C.SCREEN, C.SCREEN

END SUB

SUB GET.TRIGGER.MESSAGE (RAM.DRIVE$, TRIGGER.MESSAGE$, FMT.INDEX, TRIGGER.CODE$)
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "GET.TRIGGER.MESSAGE"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	TRIGGER.MESSAGE$ = ""
	TRIGGER.FILE.NUMBER$ = LTRIM$(STR$(FMT.INDEX))
	TRIGGER.FILE.NAME$ = RAM.DRIVE$ + "TRIG-" + TRIGGER.FILE.NUMBER$ + ".DAT"
	OPEN TRIGGER.FILE.NAME$ FOR INPUT AS #7
	DO UNTIL EOF(7)
	   LINE INPUT #7, A$
	   X1 = INSTR(A$, ",")
	   TRIGGER$ = LEFT$(A$, X1 - 1)
	   IF VAL(TRIGGER$) = VAL(TRIGGER.CODE$) THEN
	      TRIGGER.MESSAGE$ = RIGHT$(A$, LEN(A$) - X1)
	      CLOSE #7
	      EXIT SUB
	   END IF
	LOOP
	CLOSE #7

END SUB

SUB HELP.DISPLAY (H1$, H2$, H3$, H4$, H5$)
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "HELP.DISPLAY"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	IF H1$ = "" AND H2$ = "" AND H3$ = "" AND H4$ = "" AND H5$ = "" THEN
	   LOCATE 12, 22, 0
	   PRINT "No help is available for this field"
	ELSE
	   LOCATE 10, 20, 0: PRINT H1$;
	   LOCATE 11, 20, 0: PRINT H2$;
	   LOCATE 12, 20, 0: PRINT H3$;
	   LOCATE 13, 20, 0: PRINT H4$;
	   LOCATE 14, 20, 0: PRINT H5$;
	END IF

	CLOSE #6
END SUB

FUNCTION HEX.TO.CHAR$ (HX$)

	FOR I = 1 TO LEN(HX$) STEP 2
	  MX$ = MID$(HX$, I, 2)
	  BYTE = HEX.TO.DECIMAL(MX$)
	  Y$ = Y$ + CHR$(BYTE)
	NEXT I
	HEX.TO.CHAR$ = Y$

END FUNCTION

SUB INITIALIZE.MESSAGE.OUT (MAX.FIELDS, MILEN(), MIPIC$(), MSI$, MIOFF())
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "INITIALIZE.MESSAGE.OUT"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	FOR I = 1 TO MAX.FIELDS
	   IF MILEN(I) <> 0 THEN
	      X1 = INSTR(MIPIC$(I), "X")
	      IF X1 = 0 THEN
		 MID$(MSI$, MIOFF(I), MILEN(I)) = STRING$(MILEN(I), "0")
	      ELSE
		 MID$(MSI$, MIOFF(I), MILEN(I)) = SPACE$(MILEN(I))
	      END IF
	   END IF
	NEXT I

END SUB

SUB INITIALIZE.SCREEN
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "INITIALIZE.SCREEN"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	PCOPY 0, 1
	SCREEN , , 1, 1
	COLOR 0, 3
	LOCATE 11, 18, 0: PRINT "ษออออออออออออออออออออออออออออออออออออออออออป";
	LOCATE 12, 18, 0: PRINT "บ       Initialization In Progress         บ";
	LOCATE 13, 18, 0: PRINT "บ               Please Wait                บ";
	LOCATE 14, 18, 0: PRINT "ศออออออออออออออออออออออออออออออออออออออออออผ";
	CALL PUT.SHADOW(11, 18, 44, 4, "บ")
END SUB

SUB MEMORY.CHECK
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "MEMORY.CHECK"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	X$ = STR$(INT(FRE(1) / 1024))
	X$ = RIGHT$(X$, LEN(X$) - 1)

	LOCATE 25, 75, 0
	PRINT X$; "kb";

	IF FRE(1) < 1000 THEN
	   COLOR 15, 0
	   LOCATE 25, 40, 0
	   PRINT " Not enough memory. "; X$; "kb ";
	   BEEP
	   CLOSE
	   END
	END IF
END SUB

SUB MOVE.TO.CT (K1, FEELDS, MAX.FIELDS, SISFN(), PRINT.FLAG, SOSFN(), SIROW(), SOROW(), SICOL(), SOCOL(), PIC.CODE, TXT$, SOLEN(), CT$(), TYP(), SOX)
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "MOVE.TO.CT"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	'
	' This routine moves the field to the CT$ array if the field
	' was a display field as well as an entry field. It return a
	' PRINT.FLAG to instruct the system wether to print the field
	' or not. If the field is an entry field the contents are moved
	' to CT$ and the PRINT.FLAG is to set to 0 which means do not
	' print.
	'
	FOR K = K1 TO MAX.FIELDS
	   IF SISFN(K) = 0 THEN
	      K1 = K + 1
	      PRINT.FLAG = 1
	      EXIT SUB
	   END IF
	   IF SISFN(K) < 201 THEN
	      IF SISFN(K) = SOSFN(SOX) AND (SIROW(K) <> SOROW(SOX) OR SICOL(K) <> SOCOL(SOX)) THEN
		 IF PIC.CODE = 1 THEN
		    TXT$ = LEFT$(TXT$, SOLEN(SOX))
		    PRINT TXT$;
		    PRINT.FLAG = 2
		 END IF
	      END IF

	      FOUND.INDEX = 0
	      FOR K2 = 1 TO FEELDS
		 IF SIROW(K2) = SOROW(SOX) AND SICOL(K2) = SOCOL(SOX) THEN
		    FOUND.INDEX = K2
		    K2 = FEELDS + 1
		 END IF
	      NEXT K2

	      IF FOUND.INDEX <> 0 THEN
		 SELECT CASE PIC.CODE
		    CASE 1
		       IF TXT$ = SPACE$(LEN(TXT$)) THEN
			  CT$(FOUND.INDEX) = ""
		       ELSE
			  IF TYP(FOUND.INDEX) = 3 THEN
			     CT$(FOUND.INDEX) = UCASE$(RTRIM$(TXT$))
			  ELSE
			     CT$(FOUND.INDEX) = RTRIM$(TXT$)
			  END IF
		       END IF
		       PRINT.FLAG = 0
		    CASE 2
		       IF VAL(TXT$) = 0 THEN
			  CT$(FOUND.INDEX) = ""
		       ELSE
			  CT$(FOUND.INDEX) = LTRIM$(STR$(VAL(TXT$)))
		       END IF
		       PRINT.FLAG = 0
		 END SELECT
		 IF PRINT.FLAG = 2 THEN
		    PRINT.FLAG = 0
		 END IF
		 K1 = K + 1
		 EXIT SUB
	      ELSE
		 IF PRINT.FLAG <> 2 THEN
		    PRINT.FLAG = 1
		 ELSE
		    PRINT.FLAG = 0
		 END IF
		 EXIT SUB
	      END IF
	   END IF
	NEXT K
	K1 = K
END SUB

SUB OPEN.HELP.FILE (RAM.DRIVE$, HELP.FILE.NUMBER$, HELP.FILE.OPEN)
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "OPEN.HELP.FILE"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	HELP.FILE.NAME$ = RAM.DRIVE$ + "HELP-" + HELP.FILE.NUMBER$ + ".DAT"
	OPEN HELP.FILE.NAME$ FOR OUTPUT AS #6
	HELP.FILE.OPEN = 1

END SUB

SUB OPEN.TRIGGER.FILE (RAM.DRIVE$, TRIGGER.FILE.NUMBER$, TRIGGER.FILE.OPEN)
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "OPEN.TRIGGER.FILE"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	TRIGGER.FILE.NAME$ = RAM.DRIVE$ + "TRIG-" + TRIGGER.FILE.NUMBER$ + ".DAT"
	OPEN TRIGGER.FILE.NAME$ FOR OUTPUT AS #7
	TRIGGER.FILE.OPEN = 1

END SUB

SUB PUT.SHADOW (XS%, YS%, LS%, ds%, C$)

	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	LOCATE XS% - 2, YS% + 1, 0
        CALL PUTSHADOW(LS%, 1)
	LOCATE XS% - 1, YS% + LS% - 2, 0
        CALL PUTSHADOW(2, ds% - 1)
	LOCATE XS% - 1, YS% + LS% - 1, 0
        CALL PUTSHADOW(2, ds% - 1)
	LOCATE XS% + ds% - 2, YS% + LS% - 1, 0: PRINT C$;

END SUB

SUB SEND.SCREEN.ROUTINE (LINES$())
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "SEND.SCREEN.ROUTINE"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	FOR I = 1 TO 24
	   LINES$(I) = ""
	   FOR J = 1 TO 80
	      LOCATE I, J, 0
	      X$ = CHR$(SCREEN(I, J))
	      LINES$(I) = LINES$(I) + X$
	   NEXT J
	NEXT I

END SUB

SUB SET.COLORS (ATT$, BGCLR, FGCLR, UNPRO, BLINK, DIMC)

	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	FOR I = 1 TO LEN(ATT$)
	    X = ASC(MID$(ATT$, I, 1))
	    SELECT CASE X
	       CASE 1
		  F = BGCLR
		  B = BGCLR
		  I = 999
	       CASE 2
		  F = UNPRO
		  B = BGCLR
		  I = 999
	       CASE 5
		  F = BLINK
		  B = BGCLR
		  I = 999
	       CASE 11
		  F = BGCLR
		  B = FGCLR
		  I = 999
	       CASE 16
		  F = FGCLR
		  B = BGCLR
	       CASE 20
		  F = DIMC
		  B = BGCLR
	    END SELECT
	NEXT I
	COLOR F, B
END SUB

SUB SPECIAL.KEYS.BUFFER (AL$, ALL.FUNC.KEYS$)

	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	'
	' Function keys modified scan codes.
	'
	F1$ = CHR$(27) + ";"
	F2$ = CHR$(27) + "<"
	F3$ = CHR$(27) + "="
	F4$ = CHR$(27) + ">"
	F5$ = CHR$(27) + "?"
	F6$ = CHR$(27) + "@"
	F7$ = CHR$(27) + "A"
	F8$ = CHR$(27) + "B"
	F9$ = CHR$(27) + "C"
	F10$ = CHR$(27) + "D"
	F11$ = CHR$(27) + "…"
	F12$ = CHR$(27) + ""
	'
	'
	' Shift Function keys modified scan codes.
	'
	SF1$ = CHR$(27) + "T"
	SF2$ = CHR$(27) + "U"
	SF3$ = CHR$(27) + "V"
	SF4$ = CHR$(27) + "W"
	SF5$ = CHR$(27) + "X"
	SF6$ = CHR$(27) + "Y"
	SF7$ = CHR$(27) + "Z"
	SF8$ = CHR$(27) + "["
	SF9$ = CHR$(27) + "\"
	SF10$ = CHR$(27) + "]"
	SF11$ = CHR$(27) + ""
	SF12$ = CHR$(27) + ""
	'
	'
	' CTRL Function keys modified scan codes.
	'
	CF1$ = CHR$(27) + "^"
	CF2$ = CHR$(27) + "_"
	CF3$ = CHR$(27) + "`"
	CF4$ = CHR$(27) + "a"
	CF5$ = CHR$(27) + "b"
	CF6$ = CHR$(27) + "c"
	CF7$ = CHR$(27) + "d"
	CF8$ = CHR$(27) + "e"
	CF9$ = CHR$(27) + "f"
	CF10$ = CHR$(27) + "g"
	CF11$ = CHR$(27) + ""
	CF12$ = CHR$(27) + ""
	'
	'
	' ALT Function keys modified scan codes.
	'
	AF1$ = CHR$(27) + "h"
	AF2$ = CHR$(27) + "i"
	AF3$ = CHR$(27) + "j"
	AF4$ = CHR$(27) + "k"
	AF5$ = CHR$(27) + "l"
	AF6$ = CHR$(27) + "m"
	AF7$ = CHR$(27) + "n"
	AF8$ = CHR$(27) + "o"
	AF9$ = CHR$(27) + "p"
	AF10$ = CHR$(27) + "q"
	AF11$ = CHR$(27) + ""
	AF12$ = CHR$(27) + ""
	'
	AL$ = AL$ + F1$
	AL$ = AL$ + F2$
	AL$ = AL$ + F3$
	AL$ = AL$ + F4$
	AL$ = AL$ + F5$
	AL$ = AL$ + F6$
	AL$ = AL$ + F7$
	AL$ = AL$ + F8$
	AL$ = AL$ + F9$
	AL$ = AL$ + F10$
	AL$ = AL$ + F11$
	AL$ = AL$ + F12$

	'
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + ";" + "<" + "=" + ">" + "?"
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + "@" + "A" + "B" + "C" + "D"
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + "…" + ""

	AL$ = AL$ + SF1$
	AL$ = AL$ + SF2$
	AL$ = AL$ + SF3$
	AL$ = AL$ + SF4$
	AL$ = AL$ + SF5$
	AL$ = AL$ + SF6$
	AL$ = AL$ + SF7$
	AL$ = AL$ + SF8$
	AL$ = AL$ + SF9$
	AL$ = AL$ + SF10$
	AL$ = AL$ + SF11$
	AL$ = AL$ + SF12$
	'
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + "T" + "U" + "V" + "W" + "X"
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + "Y" + "Z" + "[" + "\" + "]"
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + "" + ""

	AL$ = AL$ + CF1$
	AL$ = AL$ + CF2$
	AL$ = AL$ + CF3$
	AL$ = AL$ + CF4$
	AL$ = AL$ + CF5$
	AL$ = AL$ + CF6$
	AL$ = AL$ + CF7$
	AL$ = AL$ + CF8$
	AL$ = AL$ + CF9$
	AL$ = AL$ + CF10$
	AL$ = AL$ + CF11$
	AL$ = AL$ + CF12$
	'
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + "^" + "_" + "`" + "a" + "b"
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + "c" + "d" + "e" + "f" + "g"
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + "" + ""

	AL$ = AL$ + AF1$
	AL$ = AL$ + AF2$
	AL$ = AL$ + AF3$
	AL$ = AL$ + AF4$
	AL$ = AL$ + AF5$
	AL$ = AL$ + AF6$
	AL$ = AL$ + AF7$
	AL$ = AL$ + AF8$
	AL$ = AL$ + AF9$
	AL$ = AL$ + AF10$
	AL$ = AL$ + AF11$
	AL$ = AL$ + AF12$
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + "h" + "i" + "j" + "k" + "l"
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + "m" + "n" + "o" + "p" + "q"
	ALL.FUNC.KEYS$ = ALL.FUNC.KEYS$ + "" + ""
END SUB

SUB VALIDATE.FIELDS.PIC (FEELDS, CT$(), TYP(), ERROR.CODE, CURRENT.FIELD, SIPIC$())
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "VALIDATE.FIELDS.PIC"
	ON ERROR GOTO SYS.LIB.ERROR.ROUTINE
	FOR I = 1 TO FEELDS
	   IF CT$(I) <> "" AND TYP(I) = 1 THEN
	      GOSUB ANALIZE.SI.PIC
	      GOSUB ANALIZE.FIELD
	      IF ERROR.CODE = 1 THEN
		 CURRENT.FIELD = I
		 LOCATE 25, 1, 0
		 PRINT "Invalid value for this field ...";
		 BEEP
		 EXIT SUB
	      END IF
	   END IF

	NEXT I
	EXIT SUB

ANALIZE.SI.PIC:

	X1 = INSTR(SIPIC$(I), "#")
	IF X1 = 0 THEN
	   X1 = INSTR(SIPIC$(I), "Z")
	END IF

	IF X1 > 0 THEN
	   WHOLE.NUMBER = VAL(LEFT$(SIPIC$(I), LEN(SIPIC$(I)) - X1 + 1))
	   X1 = INSTR(SIPIC$(I), ".")
	   IF X1 > 0 THEN
	      DECIMAL.POINT = X1
	   ELSE
	      DECIMAL.POINT = 0
	   END IF
	   IF DECIMAL.POINT <> 0 THEN
	      R.PIC$ = RIGHT$(SIPIC$(I), LEN(SIPIC$(I)) - DECIMAL.POINT)
	      X1 = INSTR(R.PIC$, "#")
	      IF X1 = 0 THEN
		 X1 = INSTR(R.PIC$, "Z")
	      END IF
	      IF X1 > 0 THEN
		 DECIMAL.NUMBER = VAL(LEFT$(R.PIC$, LEN(R.PIC$) - X1 + 1))
	      ELSE
		 DECIMAL.NUMBER = 0
	      END IF
	   END IF
	   X1 = INSTR(SIPIC$(I), "-")
	   IF X1 > 0 THEN
	      NEGATIVE.SIGN = X1
	   ELSE
	      NEGATIVE.SIGN = 0
	   END IF
	END IF
	RETURN

ANALIZE.FIELD:

	DECIMAL.POINT.COUNTER = 0
	MINUS.SIGN.COUNTER = 0
	CT.NEGATIVE.SIGN = 0
	CT.WHOLE.NUMBER = 0
	CT.DECIMAL.NUMBER = 0

	FOR J = 1 TO LEN(CT$(I))
	   IF MID$(CT$(I), J, 1) = "." THEN
	      DECIMAL.POINT.COUNTER = DECIMAL.POINT.COUNTER + 1
	   END IF
	   IF MID$(CT$(I), J, 1) = "-" THEN
	      MINUS.SIGN.COUNTER = MINUS.SIGN.COUNTER + 1
	   END IF
	NEXT J

	IF DECIMAL.POINT.COUNTER > 1 THEN
	   ERROR.CODE = 1
	   RETURN
	END IF

	IF MINUS.SIGN.COUNTER > 1 THEN
	   ERROR.CODE = 1
	   RETURN
	END IF

	X1 = INSTR(CT$(I), "-")
	IF X1 > 0 THEN
	   CT.NEGATIVE.SIGN = X1
	   IF NEGATIVE.SIGN = 0 THEN
	      ERROR.CODE = 1
	      RETURN
	   END IF
	END IF

	X1 = INSTR(CT$(I), ".")

	IF X1 > 0 AND DECIMAL.POINT = 0 THEN
	   ERROR.CODE = 1
	   RETURN
	END IF

	IF X1 > 0 THEN
	   CT.WHOLE.NUMBER = X1 - 1
	   X1 = X1 + 1

	   Z1 = INSTR("0123456789", MID$(CT$(I), X1, 1))
	   Z2 = LEN(MID$(CT$(I), X1, 1))

	   DO UNTIL Z1 = 0 OR Z2 = 0
	      CT.DECIMAL.NUMBER = CT.DECIMAL.NUMBER + 1
	      X1 = X1 + 1
	      Z1 = INSTR("0123456789", MID$(CT$(I), X1, 1))
	      Z2 = LEN(MID$(CT$(I), X1, 1))
	   LOOP

	   IF CT.DECIMAL.NUMBER > DECIMAL.NUMBER THEN
	      ERROR.CODE = 1
	      RETURN
	   ELSE
	      CT$(I) = CT$(I) + STRING$(DECIMAL.NUMBER - CT.DECIMAL.NUMBER, "0")
	   END IF
	ELSE
	   IF CT.NEGATIVE.SIGN > 0 THEN
	      CT.WHOLE.NUMBER = LEN(CT$(I)) - 1
	   ELSE
	      CT.WHOLE.NUMBER = LEN(CT$(I))
	   END IF
	END IF

	IF X1 = 0 AND DECIMAL.POINT > 0 THEN
	   IF CT.DECIMAL.NUMBER > DECIMAL.NUMBER THEN
	      ERROR.CODE = 1
	      RETURN
	   ELSE
	      CT$(I) = CT$(I) + "." + STRING$(DECIMAL.NUMBER - CT.DECIMAL.NUMBER, "0")
	   END IF
	END IF

	IF CT.WHOLE.NUMBER > WHOLE.NUMBER THEN
	   ERROR.CODE = 1
	   RETURN
	ELSE
	   CT$(I) = STRING$(WHOLE.NUMBER - CT.WHOLE.NUMBER, "0") + CT$(I)
	END IF
	RETURN

END SUB

SUB VIDEO.CHECK (C.SCREEN, IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, COLORED)
SHARED ERROR.CODE
SHARED ERR.ROUTINE$

	ERR.ROUTINE$ = "VIDEO.CHECK"
	ON ERROR GOTO VIDEO.ERROR.ROUTINE
	ERROR.CODE = 0
	FOR I = 0 TO 100
	   SCREEN , , I, I
	   IF ERROR.CODE = 5 THEN
	      C.SCREEN = I - 2
	      SCREEN , , 0, 0
	      GOTO COLOR.MODE
	   END IF
	NEXT I
	C.SCREEN = I - 2
	SCREEN , , 0, 0

COLOR.MODE:

	COLORED = 0

	CALL INT86OLD(&H11, IN.ARRAY%(), OUT.ARRAY%())
	X% = OUT.ARRAY%(ax)

	CALL DECIMAL.TO.BINARY(X%, B$)
	IF MID$(B$, 10, 2) = "10" OR MID$(B$, 10, 2) = "01" THEN
	   COLORED = 1
	END IF

END SUB


'
'
'*=============================================================================*
'| File Id.  ............ SYS-LIB2.BAS                                         |
'| Author ............... Muawiya Abujubain COVENTRY GROUP LTD.                |
'|                                                                             |
'| Purpose  ............. This file includes routines that are called by       |
'|                        program SCR-CNFG.BAS.                                |
'*=============================================================================*
'
'
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
	      CLOSE
	      END
	END SELECT

SUB ANALIZE.COLOR (RA$)

	IF LEFT$(RA$, 2) = "H." THEN
	   RA$ = "High Intensity " + RIGHT$(RA$, LEN(RA$) - 2)
	END IF

END SUB

SUB ARE.YOU.SURE
	COLOR 15, 4
	LOCATE 17, 27, 0: PRINT "иммммммммммммммммммммммммм╩";
	LOCATE 18, 27, 0: PRINT "╨  Are you sure (Y/N) ?   ╨";
	LOCATE 19, 27, 0: PRINT "хммммммммммммммммммммммммм╪";
	CALL PUT.SHADOW(17, 27, 27, 3, "╨")
END SUB

SUB AUTO.FILE (FILENUM, ERROR.CODE, ID$, PASSWORD$, LINE.COMMANDS$())

	IF ERROR.CODE = 53 THEN
	   ERROR.CODE = 0
	   CLOSE #FILENUM
	   OPEN "AUTOEXEC.CS" FOR OUTPUT AS FILENUM
	   PRINT #FILENUM, "[HOME]+LOGON+[ENTER]"
	   PRINT #FILENUM, "//////"
	   PRINT #FILENUM, "//////"
	   PRINT #FILENUM, "[END]+LOGOFF+[ENTER]"
	   CLOSE #FILENUM
	   FILENUM = FREEFILE
	   ERROR.CODE = 0
	   OPEN "AUTOEXEC.CS" FOR INPUT AS FILENUM
	END IF
	LINE INPUT #FILENUM, A$
	LINE INPUT #FILENUM, ID$
	LINE INPUT #FILENUM, PASSWORD$
	CALL INCRIPT.DECRIPT(ID$)
	CALL INCRIPT.DECRIPT(PASSWORD$)
	LINE.COUNT = 0
	DO UNTIL EOF(FILENUM)
	   LINE INPUT #FILENUM, A$
	   X = INSTR(A$, "LOGOFF")
	   IF X = 0 THEN
	      LINE.COUNT = LINE.COUNT + 1
	      LINE.COMMANDS$(LINE.COUNT) = A$
	   END IF
	LOOP
	CLOSE #FILENUM
END SUB

SUB AUTO.START.MESSAGE (CMD$)

	COLOR 15, 5
	LOCATE 12, 11, 0: PRINT "жддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╥";
	LOCATE 13, 11, 0: PRINT "╨ You can configure up to 12 lines for the AutoStart file.  ╨";
	LOCATE 14, 11, 0: PRINT "╨ The function key  number reflects  the line number.  For  ╨";
	LOCATE 15, 11, 0: PRINT "╨ example  F1 = line 1  and  F2 = line 2  and so on. So to  ╨";
	LOCATE 16, 11, 0: PRINT "╨ configure line 12 press F12.                              ╨";
	LOCATE 17, 11, 0: PRINT "╨                                                           ╨";
	LOCATE 18, 11, 0: PRINT "сддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╫";
	CALL PUT.SHADOW(12, 11, 61, 7, "╨")

	IF CMD$ = "P" OR CMD$ = "" THEN
	   LOCATE 16, 42, 0
	   PRINT "To change your password press";
	   LOCATE 17, 38, 0
	   PRINT "ALT-P.";
	END IF
END SUB

SUB COLORS.ROUTINE (LA$, RA$, COLOR.INDEX, STORED.COLORS$(), BACKGROUND$, FOREGROUND$, INPUT.FIELDS$, BLINK$, DIM$, DISPLAY.MODE$)
	SELECT CASE LA$
	    CASE "BACKGROUND"
	       BACKGROUND$ = RA$
	       CALL ANALIZE.COLOR(RA$)
	       LOCATE 12, 28, 0
	       PRINT RA$;
	       COLOR.INDEX = COLOR.INDEX + 1
	       STORED.COLORS$(COLOR.INDEX) = RA$
	    CASE "NORMAL"
	       FOREGROUND$ = RA$
	       CALL ANALIZE.COLOR(RA$)
	       LOCATE 13, 28, 0
	       PRINT RA$;
	       COLOR.INDEX = COLOR.INDEX + 1
	       STORED.COLORS$(COLOR.INDEX) = RA$
	    CASE "UNPROTECTED"
	       INPUT.FIELDS$ = RA$
	       CALL ANALIZE.COLOR(RA$)
	       LOCATE 14, 28, 0
	       PRINT RA$;
	       COLOR.INDEX = COLOR.INDEX + 1
	       STORED.COLORS$(COLOR.INDEX) = RA$
	    CASE "BLINK"
	       BLINK$ = RA$
	       CALL ANALIZE.COLOR(RA$)
	       LOCATE 15, 28, 0
	       PRINT RA$;
	       COLOR.INDEX = COLOR.INDEX + 1
	       STORED.COLORS$(COLOR.INDEX) = RA$
	    CASE "DIM"
	       DIM$ = RA$
	       CALL ANALIZE.COLOR(RA$)
	       LOCATE 16, 28, 0
	       PRINT RA$;
	       COLOR.INDEX = COLOR.INDEX + 1
	       STORED.COLORS$(COLOR.INDEX) = RA$
	    CASE "DISP.MODE"
	       DIM$ = RA$
	       CALL ANALIZE.COLOR(RA$)
	       LOCATE 18, 28, 0
	       PRINT RA$;
	       DISPLAY.MODE$ = RA$
	END SELECT
END SUB

SUB COMMS.ROUTINE (LA$, RA$, PORT$, SPEED$, PARITY$, DATA.SIZE$, STOP.BITS$, PRN.PORT$, PRN.ENABLE$, PRN.DISABLE$, CMD$, P.SPEED$, P.DATA$, P.PARITY$, P.STOP$, CMDL, TIME.OUT$)
	SELECT CASE LA$
	   CASE "PORT"
	      PORT$ = LEFT$(RA$, 4)
	      LOCATE 10, 23, 0
	      PRINT PORT$;
	      RA$ = RIGHT$(RA$, LEN(RA$) - 5)
	      COMMA = INSTR(RA$, ",")
	      SPEED$ = LEFT$(RA$, COMMA - 1)
	      LOCATE 11, 23, 0
	      PRINT SPEED$;
	      RA$ = RIGHT$(RA$, LEN(RA$) - COMMA)
	      COMMA = INSTR(RA$, ",")
	      PARITY$ = LEFT$(RA$, COMMA - 1)
	      SELECT CASE PARITY$
		 CASE "N"
		    PARITY$ = "NONE"
		 CASE "O"
		    PARITY$ = "ODD"
		 CASE "E"
		    PARITY$ = "EVEN"
	      END SELECT
	      LOCATE 13, 23, 0
	      PRINT PARITY$;
	      RA$ = RIGHT$(RA$, LEN(RA$) - COMMA)
	      COMMA = INSTR(RA$, ",")
	      DATA.SIZE$ = LEFT$(RA$, COMMA - 1)
	      LOCATE 12, 23, 0
	      PRINT DATA.SIZE$;
	      RA$ = RIGHT$(RA$, LEN(RA$) - COMMA)
	      STOP.BITS$ = RA$
	      LOCATE 14, 23, 0
	      PRINT STOP.BITS$;
	   CASE "TIME.OUT"
	      TIME.OUT$ = RA$
	      LOCATE 15, 23, 0
	      PRINT RA$;
	   CASE "PRN.ENABLE"
	      PRN.ENABLE$ = RA$
	      IF CMD$ <> "2" THEN
		 LOCATE 17, 32, 0
		 PRINT RA$;
	      END IF
	   CASE "PRN.DISABLE"
	      PRN.DISABLE$ = RA$
	      IF CMD$ <> "2" THEN
		 LOCATE 18, 32, 0
		 PRINT RA$;
	      END IF
	   CASE "PRN.PORT"
	      IF LEFT$(RA$, 3) <> "COM" THEN
		 PRN.PORT$ = RA$
		 P.SPEED$ = ""
		 P.PARITY$ = ""
		 P.DATA$ = ""
		 P.STOP$ = ""
	      ELSE
		 PRN.PORT$ = LEFT$(RA$, 4)
		 RA$ = RIGHT$(RA$, LEN(RA$) - 5)
		 COMMA = INSTR(RA$, ",")
		 P.SPEED$ = LEFT$(RA$, COMMA - 1)
		 RA$ = RIGHT$(RA$, LEN(RA$) - COMMA)
		 COMMA = INSTR(RA$, ",")
		 P.PARITY$ = LEFT$(RA$, COMMA - 1)
		 RA$ = RIGHT$(RA$, LEN(RA$) - COMMA)
		 COMMA = INSTR(RA$, ",")
		 P.DATA$ = LEFT$(RA$, COMMA - 1)
		 RA$ = RIGHT$(RA$, LEN(RA$) - COMMA)
		 P.STOP$ = RA$
	      END IF
	      IF CMD$ <> "2" THEN
		 LOCATE 19, 32, 0: PRINT PRN.PORT$;
		 IF LEFT$(PRN.PORT$, 3) = "COM" THEN
		    LOCATE 19, 37, 0: PRINT "Speed       Data   Parity   Stop";
		    IF P.SPEED$ = "" THEN P.SPEED$ = "9600"
		    IF P.PARITY$ = "" THEN P.PARITY$ = "O"
		    IF P.DATA$ = "" THEN P.DATA$ = "8"
		    IF P.STOP$ = "" THEN P.STOP$ = "1"
		    LOCATE 19, 43, 0: PRINT P.SPEED$;
		    LOCATE 19, 63, 0: PRINT P.PARITY$;
		    LOCATE 19, 54, 0: PRINT P.DATA$;
		    LOCATE 19, 70, 0: PRINT P.STOP$;
		    CMDL = 13
		 ELSE
		    CMDL = 9
		 END IF
	      ELSE
                 CMDL = 6
	      END IF
	END SELECT
END SUB

SUB CREATE.DEFAULT.FILE

	OPEN "SCR-MNGR.INI" FOR OUTPUT AS #1
	PRINT #1, "*"
	PRINT #1, "*       AVAILABLE COLORS"
	PRINT #1, "*       ================"
	PRINT #1, "*"
	PRINT #1, "*    BLACK - BLUE - GREEN - CYAN"
	PRINT #1, "*    RED - MAGENTA - BROWN - WHITE"
	PRINT #1, "*"
	PRINT #1, "*      HIGH INTENSITY COLORS"
	PRINT #1, "*      ====================="
	PRINT #1, "*"
	PRINT #1, "*    H.BLACK - H.BLUE - H.GREEN - H.CYAN"
	PRINT #1, "*    H.RED - H.MAGENTA - H.BROWN - H.WHITE"
	PRINT #1, "*"
	PRINT #1, "BACKGROUND  = H.MAGENTA"
	PRINT #1, "NORMAL      = H.WHITE"
	PRINT #1, "UNPROTECTED = H.BROWN"
	PRINT #1, "BLINK       = H.CYAN"
	PRINT #1, "DIM         = H.RED"
	PRINT #1, "DISP.MODE   = BLOCK"
	PRINT #1, "PORT        = COM1:9600,N,8,1"
	PRINT #1, "TIME.OUT    = 30"
	PRINT #1, "PRN.PORT    = COM2:9600,N,8,1"
	PRINT #1, "PRN.ENABLE  = 0D"
	PRINT #1, "PRN.DISABLE = 0D"
	PRINT #1, "RAM.DRIVE   = D:"
	PRINT #1, "MIN.SPACE   = 100"
	PRINT #1, "EXT.DRIVE   = C:"
	PRINT #1, "EXT.MIN.SP  = 9999"
	PRINT #1, "REFRESH     = 5"
	PRINT #1, "EXT.ENABLE  = Y"
	PRINT #1, "*"
	PRINT #1, "*    FUNCTION KEY DEFINITIONS."
	PRINT #1, "*"
	PRINT #1, "*            #F1 means SHIFT F1"
	PRINT #1, "*            ^F1 means CTRL  F1"
	PRINT #1, "*            @F1 means ALT   F1"
	PRINT #1, "*"
	PRINT #1, "F1         ="
	PRINT #1, "F2         ="
	PRINT #1, "F3         ="
	PRINT #1, "F4         ="
	PRINT #1, "F5         ="
	PRINT #1, "F6         ="
	PRINT #1, "F7         ="
	PRINT #1, "F8         ="
	PRINT #1, "F9         ="
	PRINT #1, "F10        ="
	PRINT #1, "F11        ="
	PRINT #1, "F12        ="
	PRINT #1, "#F1        ="
	PRINT #1, "#F2        ="
	PRINT #1, "#F3        ="
	PRINT #1, "#F4        ="
	PRINT #1, "#F5        ="
	PRINT #1, "#F6        ="
	PRINT #1, "#F7        ="
	PRINT #1, "#F8        ="
	PRINT #1, "#F9        ="
	PRINT #1, "#F10       ="
	PRINT #1, "#F11       ="
	PRINT #1, "#F12       ="
	PRINT #1, "^F1        ="
	PRINT #1, "^F2        ="
	PRINT #1, "^F3        ="
	PRINT #1, "^F4        ="
	PRINT #1, "^F5        ="
	PRINT #1, "^F6        ="
	PRINT #1, "^F7        ="
	PRINT #1, "^F8        ="
	PRINT #1, "^F9        ="
	PRINT #1, "^F10       ="
	PRINT #1, "^F11       ="
	PRINT #1, "^F12       ="
	PRINT #1, "@F1        ="
	PRINT #1, "@F2        ="
	PRINT #1, "@F3        ="
	PRINT #1, "@F4        ="
	PRINT #1, "@F5        ="
	PRINT #1, "@F6        ="
	PRINT #1, "@F7        ="
	PRINT #1, "@F8        ="
	PRINT #1, "@F9        ="
	PRINT #1, "@F10       ="
	PRINT #1, "@F11       ="
	PRINT #1, "@F12       ="
	CLOSE #1
END SUB

SUB DATA.SIZE.WINDOW
	LOCATE 10, 47, 0: PRINT "здDataSizeд©";
	LOCATE 11, 47, 0: PRINT "Ё          Ё";
	LOCATE 12, 47, 0: PRINT "Ё          Ё";
	LOCATE 13, 47, 0: PRINT "юдддддддддды";
	CALL PUT.SHADOW(10, 47, 12, 4, "Ё")
END SUB

SUB DISPLAY.AFKEYS.MESSAGE

	CALL DISPLAY.FKEYS.WINDOW
	LOCATE 15, 19, 0: PRINT "Ё            ALT-F1 - ALT-F12            Ё";
END SUB

SUB DISPLAY.CFKEYS.MESSAGE

	CALL DISPLAY.FKEYS.WINDOW
	LOCATE 15, 19, 0: PRINT "Ё           CTRL-F1 - CTRL-F12           Ё";
END SUB

SUB DISPLAY.COLORS.SCREEN

        COLOR 15, 6

	LOCATE 7, 6, 0: PRINT "ждддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╥"
	LOCATE 8, 6, 0: PRINT "╨                      Client Screen Manager                       ╨"
	LOCATE 9, 6, 0: PRINT "╨                         Screen Options                           ╨"
	LOCATE 10, 6, 0: PRINT "гдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╤"
	LOCATE 11, 6, 0: PRINT "╨    Colors                                                        ╨"
	LOCATE 12, 6, 0: PRINT "╨       Background                                                 ╨"
	LOCATE 13, 6, 0: PRINT "╨       Foreground                                                 ╨"
	LOCATE 14, 6, 0: PRINT "╨       Input Fields                                               ╨"
	LOCATE 15, 6, 0: PRINT "╨       Blink                                                      ╨"
	LOCATE 16, 6, 0: PRINT "╨       Dim                                                        ╨"
	LOCATE 17, 6, 0: PRINT "╨                                                                  ╨"
	LOCATE 18, 6, 0: PRINT "╨    Display Mode                                                  ╨"
	LOCATE 19, 6, 0: PRINT "╨                                                                  ╨"
	LOCATE 20, 6, 0: PRINT "гдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╤"
	LOCATE 21, 6, 0: PRINT "╨ Space Bar - Choose Option    дды - Accept Option     ESC - Exit ╨"
	LOCATE 22, 6, 0: PRINT "сдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╫"
	CALL PUT.SHADOW(7, 6, 68, 16, "╨")
END SUB

SUB DISPLAY.COMMS.PARAM (I, PORT$, SPEED$, DATA.SIZE$, PARITY$, STOP.BITS$, PRN.PORT$, PRN.ENABLE$, PRN.DISABLE$, CMD$, P.SPEED$, P.DATA$, P.PARITY$, P.STOP$, CMDL, TIME.OUT$)
	SELECT CASE I
	   CASE 1
	      IF CMD$ <> "2" THEN
		 IF LEFT$(PRN.PORT$, 3) = "COM" THEN
		    COLOR 15, 1
		    LOCATE 19, 70, 0: PRINT P.STOP$;
		    LOCATE 19, 65, 0: PRINT "Stop";
		 ELSE
		    COLOR 15, 1
		    LOCATE 19, 32, 0: PRINT PRN.PORT$;
		    LOCATE 19, 14, 0: PRINT "Printer Port";
		 END IF
	      ELSE
		 COLOR 15, 1
                 LOCATE 15, 11, 0: PRINT "Time Out";
                 LOCATE 15, 23, 0: PRINT TIME.OUT$
	      END IF
	      COLOR 0, 3
	      LOCATE 10, 23, 0: PRINT PORT$;
	      COLOR 12, 1
	      LOCATE 10, 11, 0: PRINT "Port";
	   CASE 2
	      COLOR 15, 1
	      LOCATE 10, 11, 0: PRINT "Port";
	      LOCATE 10, 23, 0: PRINT PORT$;
	      COLOR 12, 1
	      LOCATE 11, 11, 0: PRINT "Speed";
	      COLOR 0, 3
	      LOCATE 11, 23, 0: PRINT SPEED$;
	   CASE 3
	      COLOR 15, 1
	      LOCATE 11, 11, 0: PRINT "Speed";
	      LOCATE 11, 23, 0: PRINT SPEED$;
	      COLOR 12, 1
	      LOCATE 12, 11, 0: PRINT "Data Size";
	      COLOR 0, 3
	      LOCATE 12, 23, 0: PRINT DATA.SIZE$;
	   CASE 4
	      COLOR 15, 1
	      LOCATE 12, 11, 0: PRINT "Data Size";
	      LOCATE 12, 23, 0: PRINT DATA.SIZE$;
	      COLOR 12, 1
	      LOCATE 13, 11, 0: PRINT "Parity";
	      COLOR 0, 3
	      LOCATE 13, 23, 0: PRINT PARITY$;
	   CASE 5
	      COLOR 15, 1
	      LOCATE 13, 11, 0: PRINT "Parity";
	      LOCATE 13, 23, 0: PRINT PARITY$;
	      COLOR 12, 1
	      LOCATE 14, 11, 0: PRINT "Stop Bits";
	      COLOR 0, 3
	      LOCATE 14, 23, 0: PRINT STOP.BITS$
	   CASE 6
	      COLOR 15, 1
	      LOCATE 14, 11, 0: PRINT "Stop Bits";
	      LOCATE 14, 23, 0: PRINT STOP.BITS$;
	      COLOR 12, 1
	      LOCATE 15, 11, 0: PRINT "Time Out";
	      COLOR 0, 3
	      LOCATE 15, 23, 0: PRINT TIME.OUT$;
	   CASE 7
	      COLOR 15, 1
	      LOCATE 15, 11, 0: PRINT "Time Out";
	      LOCATE 15, 23, 0: PRINT TIME.OUT$;
	      COLOR 12, 1
	      LOCATE 17, 14, 0: PRINT "Enable Printing";
	      COLOR 0, 3
	      LOCATE 17, 32, 0: PRINT PRN.ENABLE$;
	   CASE 8
	      COLOR 15, 1
	      LOCATE 17, 14, 0: PRINT "Enable Printing";
	      LOCATE 17, 32, 0: PRINT PRN.ENABLE$;
	      COLOR 12, 1
	      LOCATE 18, 14, 0: PRINT "Disable Printing";
	      COLOR 0, 3
	      LOCATE 18, 32, 0: PRINT PRN.DISABLE$;
	   CASE 9
	      COLOR 15, 1
	      LOCATE 18, 14, 0: PRINT "Disable Printing";
	      LOCATE 18, 32, 0: PRINT PRN.DISABLE$;
	      COLOR 12, 1
	      LOCATE 19, 14, 0: PRINT "Printer Port";
	      COLOR 0, 3
	      LOCATE 19, 32, 0: PRINT PRN.PORT$;
	   CASE 10
	      COLOR 15, 1
	      LOCATE 19, 14, 0: PRINT "Printer Port";
	      LOCATE 19, 32, 0: PRINT PRN.PORT$;
	      COLOR 12, 1
	      IF LEFT$(PRN.PORT$, 3) = "COM" THEN
		 LOCATE 19, 37, 0: PRINT "Speed";
		 COLOR 0, 3
		 LOCATE 19, 43, 0: PRINT P.SPEED$;
	      ELSE
		 LOCATE 10, 11, 0: PRINT "Port";
		 COLOR 0, 3
		 LOCATE 10, 23, 0: PRINT PORT$;
	      END IF
	   CASE 11
	      COLOR 15, 1
	      LOCATE 19, 37, 0: PRINT "Speed";
	      LOCATE 19, 43, 0: PRINT P.SPEED$
	      COLOR 12, 1
	      LOCATE 19, 49, 0: PRINT "Data";
	      COLOR 0, 3
	      LOCATE 19, 54, 0: PRINT P.DATA$;
	   CASE 12
	      COLOR 15, 1
	      LOCATE 19, 49, 0: PRINT "Data";
	      LOCATE 19, 54, 0: PRINT P.DATA$;
	      COLOR 12, 1
	      LOCATE 19, 56, 0: PRINT "Parity";
	      COLOR 0, 3
	      LOCATE 19, 63, 0: PRINT P.PARITY$;
	   CASE 13
	      COLOR 15, 1
	      LOCATE 19, 56, 0: PRINT "Parity";
	      LOCATE 19, 63, 0: PRINT P.PARITY$;
	      COLOR 12, 1
	      LOCATE 19, 65, 0: PRINT "Stop";
	      COLOR 0, 3
	      LOCATE 19, 70, 0: PRINT P.STOP$;
	   END SELECT
END SUB

SUB DISPLAY.COMMS.SCREEN (CMD$)

	COLOR 15, 1

	LOCATE 6, 6, 0: PRINT "ждддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╥"
	LOCATE 7, 6, 0: PRINT "╨                      Client Screen Manager                       ╨"
	LOCATE 8, 6, 0: PRINT "╨                Communications Parameters Configuration           ╨"
	LOCATE 9, 6, 0: PRINT "гдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╤"
	LOCATE 10, 6, 0: PRINT "╨    Port                                                          ╨"
	LOCATE 11, 6, 0: PRINT "╨    Speed                                                         ╨"
	LOCATE 12, 6, 0: PRINT "╨    Data Size                                                     ╨"
	LOCATE 13, 6, 0: PRINT "╨    Parity                                                        ╨"
	LOCATE 14, 6, 0: PRINT "╨    Stop Bits                                                     ╨"
	LOCATE 15, 6, 0: PRINT "╨    Time Out                                                      ╨"
	LOCATE 16, 6, 0: PRINT "╨                                                                  ╨"
	LOCATE 17, 6, 0: PRINT "╨                                                                  ╨"
	LOCATE 18, 6, 0: PRINT "╨                                                                  ╨"
	LOCATE 19, 6, 0: PRINT "╨                                                                  ╨"
	LOCATE 20, 6, 0: PRINT "гдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╤"
	LOCATE 21, 6, 0: PRINT "╨ Space Bar - Item Select      дды - Edit Item         ESC - Exit ╨"
	LOCATE 22, 6, 0: PRINT "сдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╫"
	CALL PUT.SHADOW(6, 6, 68, 17, "╨")
	IF CMD$ <> "2" THEN
	   LOCATE 16, 11, 0: PRINT "Printer Parameters :-";
	   LOCATE 17, 14, 0: PRINT "Enable Printing";
	   LOCATE 18, 14, 0: PRINT "Disable Printing";
	   LOCATE 19, 14, 0: PRINT "Printer Port";
	END IF

END SUB

SUB DISPLAY.EXTERNAL.DISK.SCREEN

	COLOR 15, 6

	LOCATE 7, 4, 0: PRINT "ждддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╥";
	LOCATE 8, 4, 0: PRINT "╨                      Client Screen Manager                       ╨";
	LOCATE 9, 4, 0: PRINT "╨               Disk Storage Parameters Configuration              ╨";
	LOCATE 10, 4, 0: PRINT "гдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╤";
	LOCATE 11, 4, 0: PRINT "╨ External Drive Parameters                                        ╨";
	LOCATE 12, 4, 0: PRINT "╨    Disk & Directory Id                                           ╨";
	LOCATE 13, 4, 0: PRINT "╨    Minimum Space Allowed                                         ╨";
	LOCATE 14, 4, 0: PRINT "╨    Refresh Frequency                                             ╨";
	LOCATE 15, 4, 0: PRINT "гдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╤";
	LOCATE 16, 4, 0: PRINT "╨                                                       ESC - Exit ╨";
	LOCATE 17, 4, 0: PRINT "сдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╫";
	CALL PUT.SHADOW(7, 4, 68, 11, "╨")

END SUB

SUB DISPLAY.FKEYS
	COLOR 0, 6
	LOCATE 3, 3, 0: PRINT "жддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╥";
	LOCATE 4, 3, 0: PRINT "╨                      Client Screen Manager                              ╨";
	LOCATE 5, 3, 0: PRINT "╨                                                                         ╨";
	LOCATE 6, 3, 0: PRINT "гддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╤";
	LOCATE 7, 3, 0: PRINT "╨   Command 1                           Command 13                        ╨";
	LOCATE 8, 3, 0: PRINT "╨   Command 2                           Command 14                        ╨";
	LOCATE 9, 3, 0: PRINT "╨   Command 3                           Command 15                        ╨";
	LOCATE 10, 3, 0: PRINT "╨   Command 4                           Command 16                        ╨";
	LOCATE 11, 3, 0: PRINT "╨   Command 5                           Command 17                        ╨";
	LOCATE 12, 3, 0: PRINT "╨   Command 6                           Command 18                        ╨";
	LOCATE 13, 3, 0: PRINT "╨   Command 7                           Command 19                        ╨";
	LOCATE 14, 3, 0: PRINT "╨   Command 8                           Command 20                        ╨";
	LOCATE 15, 3, 0: PRINT "╨   Command 9                           Command 21                        ╨";
	LOCATE 16, 3, 0: PRINT "╨   Command 10                          Command 22                        ╨";
	LOCATE 17, 3, 0: PRINT "╨   Command 11                          Command 23                        ╨";
	LOCATE 18, 3, 0: PRINT "╨   Command 12                          Command 24                        ╨";
	LOCATE 19, 3, 0: PRINT "гддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╤";
	LOCATE 20, 3, 0: PRINT "╨ F1 [ENTER]  F5 [TAB]          F9  [SHOW KEYS]         ды Enter Text    ╨";
	LOCATE 21, 3, 0: PRINT "╨ F2 [HOME]   F6 [BACK TAB]     F10 [INITIALIZE FIELDS] DEL Delete Command╨";
	LOCATE 22, 3, 0: PRINT "╨ F3 [END]    F7 [SEND SCREEN]  F11 [CALCULATOR]        Esc Exit          ╨";
	LOCATE 23, 3, 0: PRINT "╨ F4 [HELP]   F8 [REACLL INPUT] F12 [CALCULATOR W/TRANSFER]                ╨";
	LOCATE 24, 3, 0: PRINT "сддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╫";
	CALL PUT.SHADOW(3, 3, 75, 22, "╨")
	COLOR 7, 6
	LOCATE 20, 5, 0: PRINT "F1";
	LOCATE 21, 5, 0: PRINT "F2";
	LOCATE 22, 5, 0: PRINT "F3";
	LOCATE 23, 5, 0: PRINT "F4";
	LOCATE 20, 17, 0: PRINT "F5";
	LOCATE 21, 17, 0: PRINT "F6";
	LOCATE 22, 17, 0: PRINT "F7";
	LOCATE 23, 17, 0: PRINT "F8";
	LOCATE 20, 35, 0: PRINT "F9";
	LOCATE 21, 35, 0: PRINT "F10";
	LOCATE 22, 35, 0: PRINT "F11";
	LOCATE 23, 35, 0: PRINT "F12";
	LOCATE 20, 59, 0: PRINT "ды";
	LOCATE 21, 59, 0: PRINT "DEL";
	LOCATE 22, 59, 0: PRINT "Esc";
END SUB

SUB DISPLAY.FKEYS.COMMANDS (MENU.SELECTION$, F4$, F5$, F6$, F7$, F8$, FKEYS.COMMANDS$(), AUTO.COMMANDS$())

	ROW = 7
	COLOR 15, 6
	SELECT CASE MENU.SELECTION$
	   CASE F4$, F5$, F6$, F7$
	      FOR I2 = 1 TO 12
		 LOCATE ROW, 18, 0
		 PRINT FKEYS.COMMANDS$(I2);
		 ROW = ROW + 1
	      NEXT I2
	      ROW = 7
	      FOR I2 = 13 TO 24
		 LOCATE ROW, 54, 0
		 PRINT FKEYS.COMMANDS$(I2);
		 ROW = ROW + 1
	      NEXT I2
	   CASE F8$
	      FOR I2 = 1 TO 12
		 LOCATE ROW, 18, 0
		 PRINT AUTO.COMMANDS$(I2);
		 ROW = ROW + 1
	      NEXT I2
	      ROW = 7
	      FOR I2 = 13 TO 24
		 LOCATE ROW, 54, 0
		 PRINT AUTO.COMMANDS$(I2);
		 ROW = ROW + 1
	      NEXT I2
	END SELECT

END SUB

SUB DISPLAY.FKEYS.MESSAGE

	CALL DISPLAY.FKEYS.WINDOW
	LOCATE 15, 19, 0: PRINT "Ё                F1 - F12                Ё";
END SUB

SUB DISPLAY.FKEYS.WINDOW

	COLOR 15, 2
	LOCATE 13, 19, 0: PRINT "здддддддддддддддддддддддддддддддддддддддд©";
	LOCATE 14, 19, 0: PRINT "Ё  Press the function key to be defined  Ё";
	LOCATE 16, 19, 0: PRINT "Ё                   OR                   Ё";
	LOCATE 17, 19, 0: PRINT "Ё               Esc to Exit              Ё";
	LOCATE 18, 19, 0: PRINT "юдддддддддддддддддддддддддддддддддддддддды";
	CALL PUT.SHADOW(13, 19, 42, 6, "Ё")
END SUB

SUB DISPLAY.MENU

	COLOR 0, 3
	LOCATE 7, 19, 0: PRINT "жддддддддддддддддддддддддддддддддддддддддд╥"
	LOCATE 8, 19, 0: PRINT "╨          Client Screen Manager          ╨"
	LOCATE 9, 19, 0: PRINT "╨            Configuration Menu           ╨"
	LOCATE 10, 19, 0: PRINT "гддддддддддддддддддддддддддддддддддддддддд╤"
	LOCATE 11, 19, 0: PRINT "╨          Screen Options                 ╨"
	LOCATE 12, 19, 0: PRINT "╨          Communications                 ╨"
	LOCATE 13, 19, 0: PRINT "╨          Disk Storage                   ╨"
	LOCATE 14, 19, 0: PRINT "╨          Function Keys                  ╨"
	LOCATE 15, 19, 0: PRINT "╨          SHIFT Function Keys            ╨"
	LOCATE 16, 19, 0: PRINT "╨          CTRL Function Keys             ╨"
	LOCATE 17, 19, 0: PRINT "╨          ALT Function Keys              ╨"
	LOCATE 18, 19, 0: PRINT "╨          Auto Start Sequence            ╨"
	LOCATE 19, 19, 0: PRINT "╨          Calculator Tax Setup           ╨"
	LOCATE 20, 19, 0: PRINT "╨                                         ╨"
	LOCATE 21, 19, 0: PRINT "╨          Exit                           ╨"
	LOCATE 22, 19, 0: PRINT "сддддддддддддддддддддддддддддддддддддддддд╫"
	CALL PUT.SHADOW(7, 19, 43, 16, "╨")
	COLOR 15, 3
	LOCATE 11, 25, 0: PRINT "F1"
	LOCATE 12, 25, 0: PRINT "F2"
	LOCATE 13, 25, 0: PRINT "F3"
	LOCATE 14, 25, 0: PRINT "F4"
	LOCATE 15, 25, 0: PRINT "F5"
	LOCATE 16, 25, 0: PRINT "F6"
	LOCATE 17, 25, 0: PRINT "F7"
	LOCATE 18, 25, 0: PRINT "F8"
	LOCATE 19, 25, 0: PRINT "F9"
	LOCATE 21, 25, 0: PRINT "ESC"
END SUB

SUB DISPLAY.RAM.DISK.SCREEN

	COLOR 15, 1

	LOCATE 6, 6, 0: PRINT "ждддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╥";
	LOCATE 7, 6, 0: PRINT "╨                      Client Screen Manager                       ╨";
	LOCATE 8, 6, 0: PRINT "╨               Disk Storage Parameters Configuration              ╨";
	LOCATE 9, 6, 0: PRINT "гдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╤";
	LOCATE 10, 6, 0: PRINT "╨ Ram Drive Parameters                                             ╨";
	LOCATE 11, 6, 0: PRINT "╨    Disk & Directory Id                                           ╨";
	LOCATE 12, 6, 0: PRINT "╨    Minimum Space Allowed                                         ╨";
	LOCATE 13, 6, 0: PRINT "╨                                                                  ╨";
	LOCATE 14, 6, 0: PRINT "╨ Use External Drive (Y/N) ?                                       ╨";
	LOCATE 15, 6, 0: PRINT "гдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╤";
	LOCATE 16, 6, 0: PRINT "╨                                                       ESC - Exit ╨";
	LOCATE 17, 6, 0: PRINT "сдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╫";
	CALL PUT.SHADOW(6, 6, 68, 12, "╨")

END SUB

SUB DISPLAY.SFKEYS.MESSAGE

	CALL DISPLAY.FKEYS.WINDOW
	LOCATE 15, 19, 0: PRINT "Ё           SHIFT-F1 - SHIFT-F12         Ё";
END SUB

SUB INCRIPT.DECRIPT (T$)
	IF LEN(T$) < 6 THEN
	   T$ = T$ + SPACE$(6 - LEN(T$))
	END IF
	FOR J = 1 TO LEN(T$)
	   W$ = W$ + CHR$(ASC(MID$(T$, J, 1)) XOR 15)
	NEXT J
	T$ = W$
END SUB

SUB MESSAGE.1

	LOCATE 22, 6, 0: PRINT "здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©";
	LOCATE 23, 6, 0: PRINT "Ё Space Bar - Item Select      дды - Accept Item       ESC - Exit Ё";
	LOCATE 24, 6, 0: PRINT "юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды";
	CALL PUT.SHADOW(22, 6, 68, 3, "Ё")
END SUB

SUB MESSAGE.10
	LOCATE 19, 14, 0: PRINT "зддддддддддддддддддддддддддддддддддддддддддддддддддд©";
	LOCATE 20, 14, 0: PRINT "Ё Enter the minimum space allowed for the external  Ё";
	LOCATE 21, 14, 0: PRINT "Ё drive in Kilo Bytes. The system will stop copying Ё";
	LOCATE 22, 14, 0: PRINT "Ё the new formats, to the external drive, after     Ё";
	LOCATE 23, 14, 0: PRINT "Ё reaching this number.                             Ё";
	LOCATE 24, 14, 0: PRINT "юддддддддддддддддддддддддддддддддддддддддддддддддддды";
	CALL PUT.SHADOW(19, 14, 53, 6, "Ё")
END SUB

SUB MESSAGE.11
	LOCATE 19, 14, 0: PRINT "зддддддддддддддддддддддддддддддддддддддддддддддддддд©";
	LOCATE 20, 14, 0: PRINT "Ё The system will copy all the new formats, from    Ё";
	LOCATE 21, 14, 0: PRINT "Ё RAM drive to the external drive, every time this  Ё";
	LOCATE 22, 14, 0: PRINT "Ё number of new formats have been received. EnteringЁ";
	LOCATE 23, 14, 0: PRINT "Ё 1 here causes the system to copy all the formats. Ё";
	LOCATE 24, 14, 0: PRINT "юддддддддддддддддддддддддддддддддддддддддддддддддддды";
	CALL PUT.SHADOW(19, 14, 53, 6, "Ё")
END SUB

SUB MESSAGE.12
        LOCATE 20, 15, 0: PRINT "зддддддддддддддддддддддддддддддддддддддддддддддддд©";
        LOCATE 21, 15, 0: PRINT "Ё The maximum number of seconds the system will   Ё";
        LOCATE 22, 15, 0: PRINT "Ё wait for a message before assuming that the     Ё";
        LOCATE 23, 15, 0: PRINT "Ё SERVER is disconnected.                         Ё";
        LOCATE 24, 15, 0: PRINT "юддддддддддддддддддддддддддддддддддддддддддддддддды";
        CALL PUT.SHADOW(20, 15, 51, 5, "Ё")
END SUB

SUB MESSAGE.2
	LOCATE 20, 15, 0: PRINT "зддддддддддддддддддддддддддддддддддддддддддддддддд©";
	LOCATE 21, 15, 0: PRINT "Ё Enter the escape sequence in HEX representation Ё";
	LOCATE 22, 15, 0: PRINT "Ё such as 0D0A0F1E - Press дды  when finished.   Ё";
	LOCATE 23, 15, 0: PRINT "юддддддддддддддддддддддддддддддддддддддддддддддддды";
	CALL PUT.SHADOW(20, 15, 51, 4, "Ё")
END SUB

SUB MESSAGE.3
	LOCATE 19, 15, 0: PRINT "зддддддддддддддддддддддддддддддддддддддддддддддддд©";
	LOCATE 20, 15, 0: PRINT "Ё Enter the drive id followed by the directory id.Ё";
	LOCATE 21, 15, 0: PRINT "Ё A ':' has to follow the drive id. The directory Ё";
	LOCATE 22, 15, 0: PRINT "Ё id is optional.                                 Ё";
	LOCATE 23, 15, 0: PRINT "юддддддддддддддддддддддддддддддддддддддддддддддддды";
	CALL PUT.SHADOW(19, 15, 51, 5, "Ё")
END SUB

SUB MESSAGE.4
	LOCATE 19, 15, 0: PRINT "зддддддддддддддддддддддддддддддддддддддддддддддддд©";
	LOCATE 20, 15, 0: PRINT "Ё Enter the minimum space allowed in Kilo Bytes.  Ё";
	LOCATE 21, 15, 0: PRINT "Ё The system will start deleting the least used   Ё";
	LOCATE 22, 15, 0: PRINT "Ё files after reaching this number.               Ё";
	LOCATE 23, 15, 0: PRINT "юддддддддддддддддддддддддддддддддддддддддддддддддды";
	CALL PUT.SHADOW(19, 15, 51, 5, "Ё")
END SUB

SUB MESSAGE.5
	COLOR 15, 4
	LOCATE 22, 22, 0: PRINT "здддддддддддддддддддддддддддддддддд©";
	LOCATE 23, 22, 0: PRINT "Ё дды - Accept         ESC - Exit Ё";
	LOCATE 24, 22, 0: PRINT "юдддддддддддддддддддддддддддддддддды";
	CALL PUT.SHADOW(22, 22, 36, 3, "Ё")
END SUB

SUB MESSAGE.6
	COLOR 15, 0
	LOCATE 25, 1, 0
	PRINT "Enter new password again for verification !";
END SUB

SUB MESSAGE.7
	COLOR 15, 4
	LOCATE 17, 15, 0: PRINT "зддддддддддддддддддддддддддддддддддддддддддддддддд©";
	LOCATE 18, 15, 0: PRINT "Ё                                                 Ё";
	LOCATE 19, 15, 0: PRINT "Ё Space Bar - Select   дды - Accept   ESC - Exit Ё";
	LOCATE 20, 15, 0: PRINT "Ё                                                 Ё";
	LOCATE 21, 15, 0: PRINT "юддддддддддддддддддддддддддддддддддддддддддддддддды";
	CALL PUT.SHADOW(17, 15, 51, 5, "Ё")
END SUB

SUB MESSAGE.8
	LOCATE 19, 15, 0: PRINT "зддддддддддддддддддддддддддддддддддддддддддддддддд©";
	LOCATE 20, 15, 0: PRINT "Ё This parameter allows the user to define the    Ё";
	LOCATE 21, 15, 0: PRINT "Ё external drive parameters. The external drive isЁ";
	LOCATE 22, 15, 0: PRINT "Ё used to store the received formats.             Ё";
	LOCATE 23, 15, 0: PRINT "юддддддддддддддддддддддддддддддддддддддддддддддддды";
	CALL PUT.SHADOW(19, 15, 51, 5, "Ё")
END SUB

SUB MESSAGE.9
	LOCATE 19, 14, 0: PRINT "зддддддддддддддддддддддддддддддддддддддддддддддддддд©";
	LOCATE 20, 14, 0: PRINT "Ё  Enter the external drive id followed by the      Ё";
	LOCATE 21, 14, 0: PRINT "Ё  direcory id. A ':' has to follow the drive id.   Ё";
	LOCATE 22, 14, 0: PRINT "Ё  The directory id is optional. The system will    Ё";
	LOCATE 23, 14, 0: PRINT "Ё  copy the formats from RAM drive to this drive.   Ё";
	LOCATE 24, 14, 0: PRINT "юддддддддддддддддддддддддддддддддддддддддддддддддддды";
	CALL PUT.SHADOW(19, 14, 53, 6, "Ё")
END SUB

SUB PAINT.SCREEN

	CLS
	COLOR 15, 0
	S$ = STRING$(80, CHR$(178))
	FOR I = 1 TO 24
	   LOCATE I, 1, 0
	   PRINT S$;
	NEXT I
	COLOR 15, 9
	LOCATE 1, 1, 0: PRINT "здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©"
	LOCATE 2, 1, 0: PRINT "Ё                       Client Screen Manager v.1.0                            Ё"
	LOCATE 3, 1, 0: PRINT "юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды"
	COLOR 15, 0
	LOCATE 6, 5, 0: PRINT "╠╠╠╠╠";
	LOCATE 6, 14, 0: PRINT "╠╠╠╠╠";
	LOCATE 6, 24, 0: PRINT "╠╠";
	LOCATE 6, 28, 0: PRINT "╠╠";
	LOCATE 6, 33, 0: PRINT "╠╠╠╠╠╠╠";
	LOCATE 6, 43, 0: PRINT "╠╠";
	LOCATE 6, 48, 0: PRINT "╠╠";
	LOCATE 6, 54, 0: PRINT "╠╠╠╠╠╠";
	LOCATE 6, 63, 0: PRINT "╠╠╠╠╠╠";
	LOCATE 6, 74, 0: PRINT "╠╠";
	LOCATE 6, 78, 0: PRINT "╠╠";
	LOCATE 7, 9, 0: PRINT "╠";
	LOCATE 7, 18, 0: PRINT "╠";
	LOCATE 7, 25, 0: PRINT "╠";
	LOCATE 7, 29, 0: PRINT "╠";
	LOCATE 7, 39, 0: PRINT "╠";
	LOCATE 7, 44, 0: PRINT "╠";
	LOCATE 7, 49, 0: PRINT "╠";
	LOCATE 7, 59, 0: PRINT "╠";
	LOCATE 7, 68, 0: PRINT "╠";
	LOCATE 7, 75, 0: PRINT "╠";
	LOCATE 7, 79, 0: PRINT "╠";
	LOCATE 8, 5, 0: PRINT "╠";
	LOCATE 8, 14, 0: PRINT "╠";
	LOCATE 8, 19, 0: PRINT "╠";
	LOCATE 8, 25, 0: PRINT "╠";
	LOCATE 8, 29, 0: PRINT "╠";
	LOCATE 8, 35, 0: PRINT "╠";
	LOCATE 8, 45, 0: PRINT "╠";
	LOCATE 8, 49, 0: PRINT "╠";
	LOCATE 8, 57, 0: PRINT "╠";
	LOCATE 8, 65, 0: PRINT "╠";
	LOCATE 8, 69, 0: PRINT "╠";
	LOCATE 8, 75, 0: PRINT "╠";
	LOCATE 8, 79, 0: PRINT "╠";
	LOCATE 9, 4, 0: PRINT "╠";
	LOCATE 9, 14, 0: PRINT "╠";
	LOCATE 9, 19, 0: PRINT "╠";
	LOCATE 9, 25, 0: PRINT "╠";
	LOCATE 9, 29, 0: PRINT "╠";
	LOCATE 9, 35, 0: PRINT "╠╠╠";
	LOCATE 9, 46, 0: PRINT "╠";
	LOCATE 9, 49, 0: PRINT "╠";
	LOCATE 9, 57, 0: PRINT "╠";
	LOCATE 9, 65, 0: PRINT "╠";
	LOCATE 9, 69, 0: PRINT "╠";
	LOCATE 9, 75, 0: PRINT "╠";
	LOCATE 9, 79, 0: PRINT "╠";
	LOCATE 10, 4, 0: PRINT "╠";
	LOCATE 10, 14, 0: PRINT "╠";
	LOCATE 10, 19, 0: PRINT "╠";
	LOCATE 10, 25, 0: PRINT "╠";
	LOCATE 10, 29, 0: PRINT "╠";
	LOCATE 10, 37, 0: PRINT "╠";
	LOCATE 10, 49, 0: PRINT "╠";
	LOCATE 10, 57, 0: PRINT "╠";
	LOCATE 10, 68, 0: PRINT "╠";
	LOCATE 10, 78, 0: PRINT "╠";
	LOCATE 11, 4, 0: PRINT "╠";
	LOCATE 11, 9, 0: PRINT "╠";
	LOCATE 11, 14, 0: PRINT "╠";
	LOCATE 11, 19, 0: PRINT "╠";
	LOCATE 11, 25, 0: PRINT "╠";
	LOCATE 11, 26, 0: PRINT "╠";
	LOCATE 11, 29, 0: PRINT "╠";
	LOCATE 11, 35, 0: PRINT "╠";
	LOCATE 11, 39, 0: PRINT "╠";
	LOCATE 11, 44, 0: PRINT "╠";
	LOCATE 11, 49, 0: PRINT "╠";
	LOCATE 11, 57, 0: PRINT "╠";
	LOCATE 11, 67, 0: PRINT "╠";
	LOCATE 11, 77, 0: PRINT "╠";
	LOCATE 12, 5, 0: PRINT "╠╠╠";
	LOCATE 12, 9, 0: PRINT "╠";
	LOCATE 12, 14, 0: PRINT "╠╠╠";
	LOCATE 12, 19, 0: PRINT "╠";
	LOCATE 12, 28, 0: PRINT "╠";
	LOCATE 12, 35, 0: PRINT "╠╠╠";
	LOCATE 12, 39, 0: PRINT "╠";
	LOCATE 12, 44, 0: PRINT "╠";
	LOCATE 12, 49, 0: PRINT "╠";
	LOCATE 12, 57, 0: PRINT "╠╠";
	LOCATE 12, 65, 0: PRINT "╠";
	LOCATE 12, 68, 0: PRINT "╠";
	LOCATE 12, 77, 0: PRINT "╠";
	LOCATE 13, 18, 0: PRINT "╠";
	LOCATE 13, 27, 0: PRINT "╠";
	LOCATE 13, 65, 0: PRINT "╠";
	LOCATE 13, 69, 0: PRINT "╠";
	LOCATE 13, 78, 0: PRINT "╠";
	LOCATE 16, 19, 0: PRINT "╠╠╠╠╠";
	LOCATE 16, 27, 0: PRINT "╠╠╠╠╠╠";
	LOCATE 16, 38, 0: PRINT "╠╠╠╠╠";
	LOCATE 16, 48, 0: PRINT "╠╠";
	LOCATE 16, 52, 0: PRINT "╠╠";
	LOCATE 16, 57, 0: PRINT "╠╠╠╠╠╠";
	LOCATE 17, 23, 0: PRINT "╠";
	LOCATE 17, 32, 0: PRINT "╠";
	LOCATE 17, 42, 0: PRINT "╠";
	LOCATE 17, 49, 0: PRINT "╠";
	LOCATE 17, 53, 0: PRINT "╠";
	LOCATE 17, 62, 0: PRINT "╠╠";
	LOCATE 18, 19, 0: PRINT "╠";
	LOCATE 18, 29, 0: PRINT "╠";
	LOCATE 18, 33, 0: PRINT "╠";
	LOCATE 18, 38, 0: PRINT "╠";
	LOCATE 18, 43, 0: PRINT "╠";
	LOCATE 18, 49, 0: PRINT "╠";
	LOCATE 18, 53, 0: PRINT "╠";
	LOCATE 18, 59, 0: PRINT "╠";
	LOCATE 18, 63, 0: PRINT "╠";
	LOCATE 19, 18, 0: PRINT "╠";
	LOCATE 19, 29, 0: PRINT "╠";
	LOCATE 19, 33, 0: PRINT "╠";
	LOCATE 19, 38, 0: PRINT "╠";
	LOCATE 19, 43, 0: PRINT "╠";
	LOCATE 19, 49, 0: PRINT "╠";
	LOCATE 19, 53, 0: PRINT "╠";
	LOCATE 19, 59, 0: PRINT "╠╠";
	LOCATE 19, 63, 0: PRINT "╠";
	LOCATE 20, 18, 0: PRINT "╠";
	LOCATE 20, 21, 0: PRINT "╠╠╠";
	LOCATE 20, 32, 0: PRINT "╠";
	LOCATE 20, 38, 0: PRINT "╠";
	LOCATE 20, 43, 0: PRINT "╠";
	LOCATE 20, 49, 0: PRINT "╠";
	LOCATE 20, 53, 0: PRINT "╠";
	LOCATE 20, 62, 0: PRINT "╠";
	LOCATE 21, 18, 0: PRINT "╠";
	LOCATE 21, 23, 0: PRINT "╠";
	LOCATE 21, 31, 0: PRINT "╠";
	LOCATE 21, 38, 0: PRINT "╠";
	LOCATE 21, 43, 0: PRINT "╠";
	LOCATE 21, 49, 0: PRINT "╠";
	LOCATE 21, 53, 0: PRINT "╠";
	LOCATE 21, 59, 0: PRINT "╠";
	LOCATE 22, 19, 0: PRINT "╠╠";
	LOCATE 22, 23, 0: PRINT "╠";
	LOCATE 22, 29, 0: PRINT "╠";
	LOCATE 22, 32, 0: PRINT "╠";
	LOCATE 22, 38, 0: PRINT "╠╠╠";
	LOCATE 22, 43, 0: PRINT "╠";
	LOCATE 22, 49, 0: PRINT "╠╠";
	LOCATE 22, 53, 0: PRINT "╠";
	LOCATE 22, 59, 0: PRINT "╠";
	LOCATE 23, 29, 0: PRINT "╠";
	LOCATE 23, 33, 0: PRINT "╠";
	LOCATE 23, 42, 0: PRINT "╠";
	LOCATE 23, 52, 0: PRINT "╠";
	LOCATE 23, 59, 0: PRINT "╠";

	COLOR 4, 15

	LOCATE 7, 4, 0: PRINT "шшшшш";
	LOCATE 7, 13, 0: PRINT "шшшшш";
	LOCATE 7, 23, 0: PRINT "шш";
	LOCATE 7, 27, 0: PRINT "шш";
	LOCATE 7, 32, 0: PRINT "шшшшшшш";
	LOCATE 7, 42, 0: PRINT "шш";
	LOCATE 7, 47, 0: PRINT "шш";
	LOCATE 7, 53, 0: PRINT "шшшшшш";
	LOCATE 7, 62, 0: PRINT "шшшшшш";
	LOCATE 7, 73, 0: PRINT "шш";
	LOCATE 7, 77, 0: PRINT "шш";
	LOCATE 8, 3, 0: PRINT "шш";
	LOCATE 8, 8, 0: PRINT "ш";
	LOCATE 8, 12, 0: PRINT "шш";
	LOCATE 8, 17, 0: PRINT "шш";
	LOCATE 8, 23, 0: PRINT "шш";
	LOCATE 8, 27, 0: PRINT "шш";
	LOCATE 8, 33, 0: PRINT "шш";
	LOCATE 8, 38, 0: PRINT "ш";
	LOCATE 8, 42, 0: PRINT "шшш";
	LOCATE 8, 47, 0: PRINT "шш";
	LOCATE 8, 53, 0: PRINT "ш";
	LOCATE 8, 55, 0: PRINT "шш";
	LOCATE 8, 58, 0: PRINT "ш";
	LOCATE 8, 63, 0: PRINT "шш";
	LOCATE 8, 67, 0: PRINT "шш";
	LOCATE 8, 73, 0: PRINT "шш";
	LOCATE 8, 77, 0: PRINT "шш";
	LOCATE 9, 2, 0: PRINT "шш";
	LOCATE 9, 12, 0: PRINT "шш";
	LOCATE 9, 17, 0: PRINT "шш";
	LOCATE 9, 23, 0: PRINT "шш";
	LOCATE 9, 27, 0: PRINT "шш";
	LOCATE 9, 33, 0: PRINT "шш";
	LOCATE 9, 42, 0: PRINT "шшшш";
	LOCATE 9, 47, 0: PRINT "шш";
	LOCATE 9, 55, 0: PRINT "шш";
	LOCATE 9, 63, 0: PRINT "шш";
	LOCATE 9, 67, 0: PRINT "ш";
	LOCATE 9, 68, 0: PRINT "ш";
	LOCATE 9, 73, 0: PRINT "ш";
	LOCATE 9, 74, 0: PRINT "ш";
	LOCATE 9, 77, 0: PRINT "ш";
	LOCATE 9, 78, 0: PRINT "ш";
	LOCATE 10, 2, 0: PRINT "шш";
	LOCATE 10, 12, 0: PRINT "шш";
	LOCATE 10, 17, 0: PRINT "шш";
	LOCATE 10, 23, 0: PRINT "шш";
	LOCATE 10, 27, 0: PRINT "шш";
	LOCATE 10, 33, 0: PRINT "шшшш";
	LOCATE 10, 42, 0: PRINT "шшшшшшш";
	LOCATE 10, 55, 0: PRINT "шш";
	LOCATE 10, 63, 0: PRINT "шшшшшш";
	LOCATE 10, 74, 0: PRINT "шшшшш";
	LOCATE 11, 2, 0: PRINT "шш";
	LOCATE 11, 12, 0: PRINT "шш";
	LOCATE 11, 17, 0: PRINT "шш";
	LOCATE 11, 23, 0: PRINT "шш";
	LOCATE 11, 27, 0: PRINT "шш";
	LOCATE 11, 33, 0: PRINT "шш";
	LOCATE 11, 42, 0: PRINT "шш";
	LOCATE 11, 45, 0: PRINT "шшшш";
	LOCATE 11, 55, 0: PRINT "шш";
	LOCATE 11, 63, 0: PRINT "шшшш";
	LOCATE 11, 75, 0: PRINT "шш";
	LOCATE 12, 3, 0: PRINT "шш";
	LOCATE 12, 8, 0: PRINT "ш";
	LOCATE 12, 12, 0: PRINT "шш";
	LOCATE 12, 17, 0: PRINT "шш";
	LOCATE 12, 24, 0: PRINT "шшшш";
	LOCATE 12, 33, 0: PRINT "шш";
	LOCATE 12, 38, 0: PRINT "ш";
	LOCATE 12, 42, 0: PRINT "шш";
	LOCATE 12, 46, 0: PRINT "шшш";
	LOCATE 12, 55, 0: PRINT "шш";
	LOCATE 12, 63, 0: PRINT "шш";
	LOCATE 12, 66, 0: PRINT "шш";
	LOCATE 12, 75, 0: PRINT "шш";
	LOCATE 13, 4, 0: PRINT "шшшшш";
	LOCATE 13, 13, 0: PRINT "шшшшш";
	LOCATE 13, 25, 0: PRINT "шш";
	LOCATE 13, 32, 0: PRINT "шшшшшшш";
	LOCATE 13, 42, 0: PRINT "шш";
	LOCATE 13, 47, 0: PRINT "шш";
	LOCATE 13, 54, 0: PRINT "шшшш";
	LOCATE 13, 62, 0: PRINT "шшш";
	LOCATE 13, 67, 0: PRINT "шш";
	LOCATE 13, 74, 0: PRINT "шшшш";
	LOCATE 17, 18, 0: PRINT "шшшшш";
	LOCATE 17, 26, 0: PRINT "шшшшшш";
	LOCATE 17, 37, 0: PRINT "шшшшш";
	LOCATE 17, 47, 0: PRINT "шш";
	LOCATE 17, 51, 0: PRINT "шш";
	LOCATE 17, 56, 0: PRINT "шшшшшш";
	LOCATE 18, 17, 0: PRINT "шш";
	LOCATE 18, 22, 0: PRINT "ш";
	LOCATE 18, 27, 0: PRINT "шш";
	LOCATE 18, 31, 0: PRINT "шш";
	LOCATE 18, 36, 0: PRINT "шш";
	LOCATE 18, 41, 0: PRINT "шш";
	LOCATE 18, 47, 0: PRINT "шш";
	LOCATE 18, 51, 0: PRINT "шш";
	LOCATE 18, 57, 0: PRINT "шш";
	LOCATE 18, 61, 0: PRINT "шш";
	LOCATE 19, 16, 0: PRINT "шш";
	LOCATE 19, 27, 0: PRINT "шш";
	LOCATE 19, 31, 0: PRINT "шш";
	LOCATE 19, 36, 0: PRINT "шш";
	LOCATE 19, 41, 0: PRINT "шш";
	LOCATE 19, 47, 0: PRINT "шш";
	LOCATE 19, 51, 0: PRINT "шш";
	LOCATE 19, 57, 0: PRINT "шш";
	LOCATE 19, 61, 0: PRINT "шш";
	LOCATE 20, 16, 0: PRINT "шш";
	LOCATE 20, 27, 0: PRINT "шшшшш";
	LOCATE 20, 36, 0: PRINT "шш";
	LOCATE 20, 41, 0: PRINT "шш";
	LOCATE 20, 47, 0: PRINT "шш";
	LOCATE 20, 51, 0: PRINT "шш";
	LOCATE 20, 57, 0: PRINT "шшшшш";
	LOCATE 21, 16, 0: PRINT "шш";
	LOCATE 21, 20, 0: PRINT "шшш";
	LOCATE 21, 27, 0: PRINT "шшшш";
	LOCATE 21, 36, 0: PRINT "шш";
	LOCATE 21, 41, 0: PRINT "шш";
	LOCATE 21, 47, 0: PRINT "шш";
	LOCATE 21, 51, 0: PRINT "шш";
	LOCATE 21, 57, 0: PRINT "шш";
	LOCATE 22, 17, 0: PRINT "шш";
	LOCATE 22, 21, 0: PRINT "шш";
	LOCATE 22, 27, 0: PRINT "шш";
	LOCATE 22, 30, 0: PRINT "шш";
	LOCATE 22, 36, 0: PRINT "шш";
	LOCATE 22, 41, 0: PRINT "шш";
	LOCATE 22, 47, 0: PRINT "шш";
	LOCATE 22, 51, 0: PRINT "шш";
	LOCATE 22, 57, 0: PRINT "шш";
	LOCATE 23, 18, 0: PRINT "шш";
	LOCATE 23, 20, 0: PRINT "шшш";
	LOCATE 23, 26, 0: PRINT "шшш";
	LOCATE 23, 31, 0: PRINT "шш";
	LOCATE 23, 37, 0: PRINT "шшшшш";
	LOCATE 23, 48, 0: PRINT "шшшш";
	LOCATE 23, 56, 0: PRINT "шшш";
END SUB

SUB PARITY.WINDOW
	LOCATE 10, 47, 0: PRINT "здParityд©";
	LOCATE 11, 47, 0: PRINT "Ё        Ё";
	LOCATE 12, 47, 0: PRINT "Ё        Ё";
	LOCATE 13, 47, 0: PRINT "Ё        Ё";
	LOCATE 14, 47, 0: PRINT "юдддддддды";
	CALL PUT.SHADOW(10, 47, 10, 5, "Ё")
END SUB

SUB PASSWORD.SCREEN
	COLOR 15, 1
	LOCATE 9, 30, 0: PRINT "здддOld Passwordдддд©";
	LOCATE 10, 30, 0: PRINT "Ё         :         Ё";
	LOCATE 11, 30, 0: PRINT "юддддддддддддддддддды";
	CALL PUT.SHADOW(9, 30, 21, 3, "Ё")
	LOCATE 13, 30, 0: PRINT "здддNew Passwordдддд©";
	LOCATE 14, 30, 0: PRINT "Ё         :         Ё";
	LOCATE 15, 30, 0: PRINT "юддддддддддддддддддды";
	CALL PUT.SHADOW(13, 30, 21, 3, "Ё")
END SUB

SUB PORT.WINDOW
	LOCATE 10, 47, 0: PRINT "здPortд©";
	LOCATE 11, 47, 0: PRINT "Ё      Ё";
	LOCATE 12, 47, 0: PRINT "Ё      Ё";
	LOCATE 13, 47, 0: PRINT "юдддддды";
	CALL PUT.SHADOW(10, 47, 8, 4, "Ё")
END SUB

SUB PRINTER.PORT.WINDOW
	LOCATE 10, 47, 0: PRINT "здPrinter Portд©";
	LOCATE 11, 47, 0: PRINT "Ё              Ё";
	LOCATE 12, 47, 0: PRINT "Ё              Ё";
	LOCATE 13, 47, 0: PRINT "Ё              Ё";
	LOCATE 14, 47, 0: PRINT "юдддддддддддддды";
	CALL PUT.SHADOW(10, 47, 16, 5, "Ё")
END SUB

SUB PUT.SHADOW (XS%, YS%, LS%, DS%, C$)

	LOCATE XS% - 2, YS% + 1, 0
        CALL PUTSHADOW(LS%, 1)
	LOCATE XS% - 1, YS% + LS% - 2, 0
        CALL PUTSHADOW(2, DS% - 1)
	LOCATE XS% - 1, YS% + LS% - 1, 0
        CALL PUTSHADOW(2, DS% - 1)
	LOCATE XS% + DS% - 2, YS% + LS% - 1, 0: PRINT C$;

END SUB

SUB SAVE.AFKEYS.MESSAGE
	LOCATE 12, 23, 0: PRINT "иммммммммммммммммммммммммммммммммммм╩";
	LOCATE 13, 23, 0: PRINT "╨ Save ALT F-KEYS changes(Y/N)?     ╨";
	LOCATE 14, 23, 0: PRINT "хммммммммммммммммммммммммммммммммммм╪";
	CALL PUT.SHADOW(12, 23, 37, 3, "╨")
END SUB

SUB SAVE.AUTO.MESSAGE
	LOCATE 12, 23, 0: PRINT "иммммммммммммммммммммммммммммммммммм╩";
	LOCATE 13, 23, 0: PRINT "╨ Save AutoStart changes (Y/N) ?    ╨";
	LOCATE 14, 23, 0: PRINT "хммммммммммммммммммммммммммммммммммм╪";
	CALL PUT.SHADOW(12, 23, 37, 3, "╨")
END SUB

SUB SAVE.CFKEYS.MESSAGE
	LOCATE 12, 23, 0: PRINT "иммммммммммммммммммммммммммммммммммм╩";
	LOCATE 13, 23, 0: PRINT "╨ Save CTRL F-KEYS changes(Y/N)?    ╨";
	LOCATE 14, 23, 0: PRINT "хммммммммммммммммммммммммммммммммммм╪";
	CALL PUT.SHADOW(12, 23, 37, 3, "╨")
END SUB

SUB SAVE.COLORS.MESSAGE
	LOCATE 12, 23, 0: PRINT "иммммммммммммммммммммммммммммммммммм╩";
	LOCATE 13, 23, 0: PRINT "╨    Save screen options (Y/N) ?    ╨";
	LOCATE 14, 23, 0: PRINT "хммммммммммммммммммммммммммммммммммм╪";
	CALL PUT.SHADOW(12, 23, 37, 3, "╨")
END SUB

SUB SAVE.COMMS.MESSAGE
	LOCATE 12, 23, 0: PRINT "иммммммммммммммммммммммммммммммммммм╩";
	LOCATE 13, 23, 0: PRINT "╨    Save Comms changes (Y/N) ?     ╨";
	LOCATE 14, 23, 0: PRINT "хммммммммммммммммммммммммммммммммммм╪";
	CALL PUT.SHADOW(12, 23, 37, 3, "╨")
END SUB

SUB SAVE.DISK.MESSAGE
	LOCATE 12, 23, 0: PRINT "иммммммммммммммммммммммммммммммммммм╩";
	LOCATE 13, 23, 0: PRINT "╨    Save Disk changes (Y/N) ?      ╨";
	LOCATE 14, 23, 0: PRINT "хммммммммммммммммммммммммммммммммммм╪";
	CALL PUT.SHADOW(12, 23, 37, 3, "╨")
END SUB

SUB SAVE.FKEYS.MESSAGE
	LOCATE 12, 23, 0: PRINT "иммммммммммммммммммммммммммммммммммм╩";
	LOCATE 13, 23, 0: PRINT "╨    Save F-KEYS changes (Y/N) ?    ╨";
	LOCATE 14, 23, 0: PRINT "хммммммммммммммммммммммммммммммммммм╪";
	CALL PUT.SHADOW(12, 23, 37, 3, "╨")
END SUB

SUB SAVE.SFKEYS.MESSAGE
	LOCATE 12, 23, 0: PRINT "иммммммммммммммммммммммммммммммммммм╩";
	LOCATE 13, 23, 0: PRINT "╨ Save SHIFT F-KEYS changes(Y/N)?   ╨";
	LOCATE 14, 23, 0: PRINT "хммммммммммммммммммммммммммммммммммм╪";
	CALL PUT.SHADOW(12, 23, 37, 3, "╨")
END SUB

SUB SPEED.WINDOW
	LOCATE 10, 47, 0: PRINT "здSpeedд©";
	FOR J = 11 TO 20
	   LOCATE J, 47, 0: PRINT "Ё       Ё";
	NEXT J
	LOCATE 21, 47, 0: PRINT "юддддддды";
	CALL PUT.SHADOW(10, 47, 9, 12, "Ё")
END SUB

SUB STOP.BITS.WINDOW
	LOCATE 10, 47, 0: PRINT "здStop Bits©";
	LOCATE 11, 47, 0: PRINT "Ё          Ё";
	LOCATE 12, 47, 0: PRINT "Ё          Ё";
	LOCATE 13, 47, 0: PRINT "юдддддддддды";
	CALL PUT.SHADOW(10, 47, 12, 4, "Ё")
END SUB

SUB STORE.COLORS (CONFIG.INDEX, CONFIG.LINES$(), STORED.COLORS$(), DISPLAY.MODE$)
	FOR I = 1 TO 6
	   FOR J = 1 TO CONFIG.INDEX
	      A$ = CONFIG.LINES$(J)
	      IF LEFT$(A$, 1) <> "*" THEN
		 SELECT CASE I
		    CASE 1
		       IF LEFT$(A$, 13) = "BACKGROUND  =" THEN
			  CONFIG.LINES$(J) = "BACKGROUND  = " + STORED.COLORS$(I)
			  J = 101
		       END IF
		    CASE 2
		       IF LEFT$(A$, 13) = "NORMAL      =" THEN
			  CONFIG.LINES$(J) = "NORMAL      = " + STORED.COLORS$(I)
			  J = 101
		       END IF
		    CASE 3
		       IF LEFT$(A$, 13) = "UNPROTECTED =" THEN
			  CONFIG.LINES$(J) = "UNPROTECTED = " + STORED.COLORS$(I)
			  J = 101
		       END IF
		    CASE 4
		       IF LEFT$(A$, 13) = "BLINK       =" THEN
			  CONFIG.LINES$(J) = "BLINK       = " + STORED.COLORS$(I)
			  J = 101
		       END IF
		    CASE 5
		       IF LEFT$(A$, 13) = "DIM         =" THEN
			  CONFIG.LINES$(J) = "DIM         = " + STORED.COLORS$(I)
			  J = 101
		       END IF
		    CASE 6
		       IF LEFT$(A$, 13) = "DISP.MODE   =" THEN
			  CONFIG.LINES$(J) = "DISP.MODE   = " + DISPLAY.MODE$
			  J = 101
		       END IF
		 END SELECT
	      END IF
	   NEXT J
	NEXT I
END SUB

SUB STORE.COMMS (PORT$, SPEED$, PARITY$, DATA.SIZE$, STOP.BITS$, PRN.PORT$, PRN.ENABLE$, PRN.DISABLE$, P.SPEED$, P.DATA$, P.PARITY$, P.STOP$, CONFIG.INDEX, CONFIG.LINES$(), TIME.OUT$)
	FOR J = 1 TO CONFIG.INDEX
	   A$ = CONFIG.LINES$(J)
	   IF LEFT$(A$, 1) <> "*" THEN
	      SELECT CASE LEFT$(A$, 13)
		 CASE "PORT        ="
		    A$ = "PORT        = " + PORT$ + ":"
		    A$ = A$ + SPEED$ + ","
		    A$ = A$ + LEFT$(PARITY$, 1) + ","
		    A$ = A$ + DATA.SIZE$ + ","
		    A$ = A$ + STOP.BITS$
		 CASE "PRN.PORT    ="
		    IF LEFT$(PRN.PORT$, 3) = "COM" THEN
		       A$ = "PRN.PORT    = " + PRN.PORT$ + ":" + P.SPEED$ + ","
		       A$ = A$ + P.PARITY$ + ","
		       A$ = A$ + P.DATA$ + ","
		       A$ = A$ + P.STOP$
		    ELSE
		       A$ = "PRN.PORT    = " + PRN.PORT$
		    END IF
                 CASE "TIME.OUT    ="
                    A$ = "TIME.OUT    = " + TIME.OUT$
		 CASE "PRN.ENABLE  ="
		    A$ = "PRN.ENABLE  = " + PRN.ENABLE$
		 CASE "PRN.DISABLE ="
		    A$ = "PRN.DISABLE = " + PRN.DISABLE$
	      END SELECT
	      CONFIG.LINES$(J) = A$
	   END IF
	NEXT J
END SUB

SUB STORE.DISK (CONFIG.INDEX, CONFIG.LINES$(), RAM.DRIVE$, MIN.SPACE$, EXT.DRIVE$, EXT.MIN.SP$, R.FREQ$, EXT.ENABLE$)
	FOR J = 1 TO CONFIG.INDEX
	   A$ = CONFIG.LINES$(J)
	   IF LEFT$(A$, 1) <> "*" THEN
	      SELECT CASE LEFT$(A$, 13)
		 CASE "RAM.DRIVE   ="
		    A$ = "RAM.DRIVE   = " + RAM.DRIVE$
		 CASE "MIN.SPACE   ="
		    A$ = "MIN.SPACE   = " + MIN.SPACE$
		 CASE "EXT.DRIVE   ="
		    A$ = "EXT.DRIVE   = " + EXT.DRIVE$
		 CASE "EXT.MIN.SP  ="
		    A$ = "EXT.MIN.SP  = " + EXT.MIN.SP$
		 CASE "REFRESH     ="
		    A$ = "REFRESH     = " + R.FREQ$
		 CASE "EXT.ENABLE  ="
		    A$ = "EXT.ENABLE  = " + EXT.ENABLE$
	      END SELECT
	      CONFIG.LINES$(J) = A$
	   END IF
	NEXT J
END SUB

SUB WRITE.CONFIG.FILE (CONFIG.LINES$(), CONFIG.INDEX)

	OPEN "SCR-MNGR.INI" FOR OUTPUT AS #1
	FOR I = 1 TO CONFIG.INDEX
	   PRINT #1, CONFIG.LINES$(I)
	NEXT I
	CLOSE #1
END SUB

SUB VIDEO.CHECK (C.SCREEN)
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
	      EXIT SUB
	   END IF
	NEXT I
	C.SCREEN = I - 2
	SCREEN , , 0, 0
END SUB

SUB PRINT.P.PARAMETERS(P.SPEED$, P.PARITY$, P.DATA$, P.STOP$)
        COLOR 15, 1
        IF P.SPEED$ = "" THEN P.SPEED$ = "9600"
        IF P.PARITY$ = "" THEN P.PARITY$ = "O"
        IF P.DATA$ = "" THEN P.DATA$ = "8"
        IF P.STOP$ = "" THEN P.STOP$ = "1"
        LOCATE 19, 43, 0: PRINT P.SPEED$;
        LOCATE 19, 54, 0: PRINT P.DATA$;
        LOCATE 19, 63, 0: PRINT P.PARITY$;
        LOCATE 19, 70, 0: PRINT P.STOP$;
END SUB

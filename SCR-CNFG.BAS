DECLARE FUNCTION HEX.TO.DECIMAL! (HX$)
DECLARE FUNCTION COUNT.SPACES! (YY$)
'
'*=============================================================================*
'| Program Id. .......... SCR-CNFG.BAS                                         |
'| Author ............... Muawiya Abujubain COVENTRY GROUP LTD.                |
'|                                                                             |
'| Function ............. This program alows the user to set the configuration |
'|                        parameters for the CLIENT/SERVER screen manager      |
'|                        program.                                             |
'*=============================================================================*
'
        REM $INCLUDE: 'QB.BI'
        REM $DYNAMIC

        DIM IN.ARRAY%(7)      'define input and output
        DIM OUT.ARRAY%(7)     'arrays for int86.
        DIM COLORS$(16)       'available colors.
        DIM STORED.COLORS$(5) 'colors chosen and stored
        DIM CONFIG.LINES$(100)'array to hold the configuration file lines
        DIM SIROW(30)         'screen input row
        DIM SICOL(30)         'screen input column
        DIM SILEN(30)         'screen input length
        DIM TYP(30)           'field type 1=numeric 2=alpha 3=upper case
        DIM MENU.ITEM$(10)    'menu items used in comms section.
        DIM FKEYS.COMMANDS$(24) 'function keys commands
        DIM AUTO.COMMANDS$(24)  'AutoStart commands
        DIM LINE.COMMANDS$(12)  'AutoStart line commands
        '
        ' The following constant is defined to be used as a pointer
        ' to the CPU register AX. This is needed for the
        ' DATA.ENTRY.ROUTINE which handles the keyboard.
        '
        CONST ax = 0, bx = 1, cx = 2, dx = 3
        CONST F$ = " "
        '
        ' The following characters are the only ones to be accepted from
        ' keyboard.
        '          15  107 71
        '
        ' These characters are stored in ALLOWED.KEYS$.
        '
        ALLOWED.KEYS$ = "GO"
        '
        ' The following constatnts hold the 2nd character a function
        ' key generates.
        '
        CONST F1$ = ";"
        CONST F2$ = "<"
        CONST F3$ = "="
        CONST F4$ = ">"
        CONST F5$ = "?"
        CONST F6$ = "@"
        CONST F7$ = "A"
        CONST F8$ = "B"
        CONST F9$ = "C"
        CONST F10$ = "D"
        CONST F11$ = "…"
        CONST F12$ = "†"
        CONST SF1$ = "T"
        CONST SF2$ = "U"
        CONST SF3$ = "V"
        CONST SF4$ = "W"
        CONST SF5$ = "X"
        CONST SF6$ = "Y"
        CONST SF7$ = "Z"
        CONST SF8$ = "["
        CONST SF9$ = "\"
        CONST SF10$ = "]"
        CONST SF11$ = "‡"
        CONST SF12$ = "ˆ"
        CONST CF1$ = "^"
        CONST CF2$ = "_"
        CONST CF3$ = "`"
        CONST CF4$ = "a"
        CONST CF5$ = "b"
        CONST CF6$ = "c"
        CONST CF7$ = "d"
        CONST CF8$ = "e"
        CONST CF9$ = "f"
        CONST CF10$ = "g"
        CONST CF11$ = "‰"
        CONST CF12$ = "Š"
        CONST AF1$ = "h"
        CONST AF2$ = "i"
        CONST AF3$ = "j"
        CONST AF4$ = "k"
        CONST AF5$ = "l"
        CONST AF6$ = "m"
        CONST AF7$ = "n"
        CONST AF8$ = "o"
        CONST AF9$ = "p"
        CONST AF10$ = "q"
        CONST AF11$ = "‹"
        CONST AF12$ = "Œ"
        CONST DELETE$ = "S"
        CONST PASS$ = ""
        '
        ' The following constatnt define the combination keys to perform
        ' certain function.
        '
        ACCEPT$ = CHR$(13)                 ' RETURN or ENTER key
        HOME$ = CHR$(27) + CHR$(71)        ' HOME
        END.KEY$ = CHR$(27) + CHR$(79)     ' END
        TAB$ = CHR$(9)
        BACK.TAB$ = CHR$(27) + CHR$(15)
        AL$ = ACCEPT$ + HELP$ + HOME$ + END.KEY$ + TAB$ + CHR$(8)
        AL$ = AL$ + BACK.TAB$ + CHR$(27)
	ASCII$ = CHR$(13)
	FOR I = 32 TO 126
	   ASCII$ = ASCII$ + CHR$(I)
	NEXT I
        '
        CONFIG.INDEX = 0
        CAHNGE.MADE = 0
        SUB.FLAG = 0
        FEELDS = 0
        UNPRO = 0
        BGCLR = 7
        I.CU = 1
        '
        RESTORE SCREEN.COLORS
        FOR I = 1 TO 16
           READ COLORS$(I)
        NEXT I
        ON ERROR GOTO GENERAL.ERROR
        CALL VIDEO.CHECK(C.SCREEN)
        C.SCREEN% = C.SCREEN% + 1

SCREEN.COLORS:
        DATA "BLACK" , "BLUE" , "GREEN" , "CYAN"
        DATA "RED" , "MAGENTA" , "BROWN" , "WHITE"
        DATA "High Intensity BLACK" , "High Intensity BLUE"
        DATA "High Intensity GREEN" , "High Intensity CYAN"
        DATA "High Intensity RED" , "High Intensity MAGENTA"
        DATA "High Intensity BROWN" , "High Intensity WHITE"
        '
PORTS:
        DATA "COM1","COM2"

PRINTER.PORTS:
        DATA "LPT1","LPT2","COM 1/2"

SPEED:
        DATA "75   ","150  ","300  ","600  ","1200 "
        DATA "1275 ","2400 ","4800 ","9600 ","19200"

DATA.SIZE:
        DATA "7", "8"

PARITY:
        DATA "None", "Odd", "Even"

STOP.BITS:
        DATA "1", "2"

TIME.OUT:
        DATA 15,23,02,01

PRN.ENABLE:
        DATA 17,32,30,03

PRN.DISABLE:
        DATA 18,32,30,03

RAM.DISK.SCREEN:
        DATA 11,35,38,03
        DATA 12,35,04,01
        DATA 14,35,01,03

EXTERNAL.DISK.SCREEN:
        DATA 12,32,38,03
        DATA 13,32,04,01
        DATA 14,32,03,01

TEXT.COMMAND:
        DATA 11,36,22,02

PASSWORD:
        DATA 10,33,06,02
        DATA 10,42,06,02
        DATA 14,33,06,02
        DATA 14,42,06,02

MAIN.SECTION:
        CMD$ = COMMAND$

        IF CMD$ <> "" THEN
           SELECT CASE CMD$
              CASE "1"
                 MENU.SELECTION$ = F1$
                 GOTO COLOR.COMMAND
              CASE "2"
                 MENU.SELECTION$ = F2$
                 GOTO COMMS.COMMAND
              CASE "3"
                 MENU.SELECTION$ = F3$
                 GOTO DISK.COMMAND
              CASE "4"
                 MENU.SELECTION$ = F4$
                 GOTO FKEYS.COMMAND
              CASE "5"
                 MENU.SELECTION$ = F5$
                 GOTO FKEYS.COMMAND
              CASE "6"
                 MENU.SELECTION$ = F6$
                 GOTO FKEYS.COMMAND
              CASE "7"
                 MENU.SELECTION$ = F7$
                 GOTO FKEYS.COMMAND
              CASE "8"
                 MENU.SELECTION$ = F8$
                 GOTO AUTO.PASSWORD.CMD
              CASE "P"
                 MENU.SELECTION$ = F8$
                 GOTO AUTO.PASSWORD.CMD
              CASE "C"
                 SHELL "CAL-CNFG"
                 GOTO PROGRAM.END
              CASE ELSE
                 SCREEN , , 0, 0
                 CLS
                 PRINT "Invalid parameter "; CMD$
                 BEEP
                 END
           END SELECT
        END IF

        SCREEN , , 0, 0

        CALL PAINT.SCREEN
        COLOR 15, 0
        LOCATE 25, 1, 0: PRINT SPACE$(80);
        CALL DISPLAY.MENU

MAIN.LOOP:
        ERROR.CODE = 0
        GOSUB GET.SELECTION
        MENU.SELECTION$ = SELECTION$

        COLOR.INDEX = 0
        SELECT CASE MENU.SELECTION$
           CASE F1$
COLOR.COMMAND:
              PCOPY 0, C.SCREEN%
              SCREEN , , C.SCREEN%, C.SCREEN%
              CALL DISPLAY.COLORS.SCREEN
              GOSUB READ.CONFIGURATION
              IF ERROR.CODE <> 0 THEN
                 IF CMD$ <> "" THEN
                    GOTO PROGRAM.END
                 END IF
                 GOTO MAIN.LOOP
              END IF
              GOSUB SELECT.COLORS
              IF CHANGE.MADE = 1 THEN
                 GOSUB SAVE.CHANGES
                 CHANGE.MADE = 0
              END IF
              SCREEN , , 0, 0
              LOCATE 1, 1, 0
           CASE F2$
COMMS.COMMAND:
              PCOPY 0, C.SCREEN%
              SCREEN , , C.SCREEN%, C.SCREEN%
              CALL DISPLAY.COMMS.SCREEN(CMD$)
              GOSUB READ.CONFIGURATION
              IF ERROR.CODE <> 0 THEN
                 GOTO MAIN.LOOP
              END IF
              GOSUB INPUT.COMMS
              IF CHANGE.MADE = 1 THEN
                 GOSUB SAVE.CHANGES
                 CHANGE.MADE = 0
              END IF
              IF CMD$ <> "" THEN
                 GOTO PROGRAM.END
              END IF
              SCREEN , , 0, 0
              LOCATE 1, 1, 0
              CALL PAINT.SCREEN
              COLOR 15, 0
              LOCATE 25, 1, 0: PRINT SPACE$(80);
              CALL DISPLAY.MENU
           CASE F3$
DISK.COMMAND:
              PCOPY 0, C.SCREEN%
              SCREEN , , C.SCREEN%, C.SCREEN%
              CALL DISPLAY.RAM.DISK.SCREEN
              GOSUB READ.CONFIGURATION
              IF ERROR.CODE <> 0 THEN
                 GOTO MAIN.LOOP
              END IF
              GOSUB INPUT.DISK
              IF CHANGE.MADE = 1 THEN
                 GOSUB SAVE.CHANGES
                 CHANGE.MADE = 0
              END IF
              SCREEN , , 0, 0
              LOCATE 1, 1, 0
           CASE F4$, F5$, F6$, F7$
FKEYS.COMMAND:
              PCOPY 0, C.SCREEN%
              SCREEN , , C.SCREEN%, C.SCREEN%
              GOSUB READ.CONFIGURATION
              IF ERROR.CODE <> 0 THEN
                 GOTO MAIN.LOOP
              END IF
FKEYS.LOOP:
              SELECT CASE MENU.SELECTION$
                 CASE F4$
                    CALL DISPLAY.FKEYS.MESSAGE
                 CASE F5$
                    CALL DISPLAY.SFKEYS.MESSAGE
                 CASE F6$
                    CALL DISPLAY.CFKEYS.MESSAGE
                 CASE F7$
                    CALL DISPLAY.AFKEYS.MESSAGE
              END SELECT
              GOSUB GET.SELECTION
              IF SELECTION$ <> CHR$(27) THEN
                 GOSUB ANALIZE.COMMANDS
                 IF ERROR.CODE = 1 THEN
                    ERROR.CODE = 0
                    GOTO FKEYS.LOOP
                 END IF
                 GOSUB DISPLAY.FKEYS.SCREEN
                 GOSUB INPUT.FKEYS
                 GOTO FKEYS.LOOP
              END IF
              IF CHANGE.MADE = 1 THEN
                 GOSUB SAVE.CHANGES
                 CHANGE.MADE = 0
              END IF
              IF CMD$ <> "" THEN
                 GOTO PROGRAM.END
              END IF
              SCREEN , , 0, 0
              LOCATE 1, 1, 0
              CALL PAINT.SCREEN
              COLOR 15, 0
              LOCATE 25, 1, 0: PRINT SPACE$(80);
              CALL DISPLAY.MENU
           CASE F8$
AUTO.PASSWORD.CMD:
              PCOPY 0, C.SCREEN%
              SCREEN , , C.SCREEN%, C.SCREEN%
              GOSUB READ.AUTO.FILE
              IF ERROR.CODE <> 0 THEN
                 GOTO MAIN.LOOP
              END IF
AUTO.LOOP:
              IF CMD$ = "8" OR CMD$ = "" THEN
                 CALL AUTO.START.MESSAGE(CMD$)
              END IF
              IF CMD$ = "P" THEN
                 GOTO PASSWORD.COMMAND
              END IF
              GOSUB GET.SELECTION
              IF SELECTION$ <> CHR$(27) THEN
                 IF CMD$ = "P" OR CMD$ = "" THEN
                    IF SELECTION$ = PASS$ THEN
PASSWORD.COMMAND:
                       CALL PASSWORD.SCREEN
                       CALL MESSAGE.5
                       FEELDS = 4
                       RESTORE PASSWORD
                       FOR I2 = 1 TO FEELDS
                          READ SIROW(I2), SICOL(I2), SILEN(I2), TYP(I2)
                          CT$(I2) = ""
                       NEXT I2
PASSWORD.LOOP:
                       BGCLR = 0
                       GOSUB DATA.ENTRY.ROUTINE
                       BGCLR = 7
                       IF EXIT.FLAG = 1 THEN
                          EXIT.FLAG = 0
                          IF CMD$ <> "" THEN
                             GOTO PROGRAM.END
                          END IF
                          GOTO AUTO.LOOP
                       END IF
                       IF LEN(CT$(1)) < 6 THEN
                          CT$(1) = CT$(1) + SPACE$(6 - LEN(CT$(1)))
                       END IF
                       IF LEN(CT$(2)) < 6 THEN
                          CT$(2) = CT$(2) + SPACE$(6 - LEN(CT$(2)))
                       END IF
                       IF CT$(1) = ID$ AND CT$(2) = PASSWORD$ THEN
                          NEW.ID$ = CT$(3)
                          NEW.PASSWORD$ = CT$(4)
                          CT$(3) = ""
                          CT$(4) = ""
                          I.CU = 3
                          CALL MESSAGE.6
                          BGCLR = 0
                          GOSUB DATA.ENTRY.ROUTINE
                          BGCLR = 7
                          I.CU = 1
                          IF EXIT.FLAG = 1 THEN
                             EXIT.FLAG = 0
                             IF CMD$ <> "" THEN
                                GOTO PROGRAM.END
                             END IF
                             GOTO AUTO.LOOP
                          END IF
                          IF CT$(3) = NEW.ID$ AND CT$(4) = NEW.PASSWORD$ THEN
                             CALL ARE.YOU.SURE
                             X = 18: Y = 51: Y.OR.N$ = ""
                             GOSUB GET.YES.OR.NO
                             IF Y.OR.N$ = "N" THEN
                                GOTO PASSWORD.LOOP
                             END IF
                             CHANGE.MADE = 1
                             ID$ = CT$(3)
                             PASSWORD$ = CT$(4)
                             IF CMD$ = "P" THEN
                                IF CHANGE.MADE = 1 THEN
                                   GOSUB SAVE.CHANGES
                                   CHANGE.MADE = 0
                                END IF
                                GOTO PROGRAM.END
                             END IF
                          ELSE
                             LOCATE 25, 1, 0
                             COLOR 15, 0
                             PRINT "Wrong new password ... Press any key to continue";
                             BEEP
                             DO
                                AA$ = INPUT$(1)
                             LOOP WHILE AA$ = ""
                             COLOR 7, 0
                             LOCATE 25, 1, 0: PRINT SPACE$(80);
                             GOTO PASSWORD.LOOP
                          END IF
                       ELSE
                          LOCATE 25, 1, 0
                          COLOR 15, 0
                          PRINT "Wrong old password ... Press any key to continue";
                          BEEP
                          DO
                             AA$ = INPUT$(1)
                          LOOP WHILE AA$ = ""
                          COLOR 7, 0
                          LOCATE 25, 1, 0: PRINT SPACE$(80);
                          GOTO PASSWORD.LOOP
                       END IF
                       GOTO AUTO.LOOP
                    END IF
                 END IF
                 GOSUB ANALIZE.COMMANDS
                 IF ERROR.CODE = 1 THEN
                    ERROR.CODE = 0
                    GOTO AUTO.LOOP
                 END IF
                 GOSUB DISPLAY.FKEYS.SCREEN
                 GOSUB INPUT.FKEYS
                 GOTO AUTO.LOOP
              END IF
              IF CHANGE.MADE = 1 THEN
                 GOSUB SAVE.CHANGES
                 CHANGE.MADE = 0
              END IF
              IF CMD$ <> "" THEN
                 GOTO PROGRAM.END
              END IF
              SCREEN , , 0, 0
              LOCATE 1, 1, 0
              CALL PAINT.SCREEN
              COLOR 15, 0
              LOCATE 25, 1, 0: PRINT SPACE$(80);
              CALL DISPLAY.MENU
           CASE F9$
              PCOPY 0, C.SCREEN%
              SCREEN , , C.SCREEN%, C.SCREEN%
              LOCATE 25, 1, 0
              COLOR 15, 0
              PRINT "Loading module CAL-CNFG ... Please wait !!!";
              LOCATE 1, 1, 0
              SHELL "CAL-CNFG"
              SCREEN , , 0, 0
              LOCATE 1, 1, 0
        END SELECT

        IF MENU.SELECTION$ = CHR$(27) THEN
           GOTO PROGRAM.END
        END IF
        IF CMD$ <> "" THEN
           GOTO PROGRAM.END
        END IF
        GOTO MAIN.LOOP

PROGRAM.END:
        CLOSE
        SCREEN , , 0, 0
        COLOR 7, 0
        CLS
        SCREEN , , C.SCREEN%, C.SCREEN%
        COLOR 7, 0
        CLS
        END

GET.SELECTION:
        C.COUNT = 0
        IN.ARRAY%(ax) = &H700

GET.BYTE:
        CALL INT86OLD(&H21, IN.ARRAY%(), OUT.ARRAY%())
        W$ = RIGHT$(HEX$(OUT.ARRAY%(ax)), 2)
        W$ = CHR$(HEX.TO.DECIMAL(W$))

        IF W$ = CHR$(27) THEN
           SELECTION$ = W$
           RETURN
        END IF

        IF W$ = CHR$(13) THEN
           SELECTION$ = W$
           RETURN
        END IF

        IF W$ = CHR$(0) THEN
           C.COUNT = C.COUNT + 1
           GOTO GET.BYTE
        ELSE
           IF C.COUNT <> 0 THEN
              SELECTION$ = W$
              C.COUNT = 0
              RETURN
           END IF
           GOTO GET.SELECTION
        END IF

DISPLAY.FKEYS.SCREEN:

        COLOR 0, 6
        CALL DISPLAY.FKEYS
        COLOR 0, 6
        LOCATE 5, 1, 0
        GOSUB CENTER.LINE
        CALL DISPLAY.FKEYS.COMMANDS(MENU.SELECTION$, F4$, F5$, F6$, F7$, F8$, FKEYS.COMMANDS$(), AUTO.COMMANDS$())
        RETURN

READ.CONFIGURATION:

        IF CONFIG.INDEX = 0 THEN
           ERROR.CODE = 0
           OPEN "SCR-MNGR.INI" FOR INPUT AS #1
           IF ERROR.CODE <> 0 THEN
              IF ERROR.CODE = 53 THEN
                 CLOSE #1
                 CALL CREATE.DEFAULT.FILE
                 GOTO READ.CONFIGURATION
              ELSE
                 COLOR 15, 4
                 LOCATE 25, 1, 0: PRINT SPACE$(80);
                 LOCATE 25, 1, 0
                 PRINT "Error "; ERROR.CODE; " Occured, Please consult DP department - Press Any Key";
              END IF
              BEEP
              DO
                 AA$ = INPUT$(1)
              LOOP WHILE AA$ = ""
              COLOR 7, 0
              LOCATE 25, 1, 0: PRINT SPACE$(80);
              RETURN
           END IF

           DO UNTIL EOF(1)
              CONFIG.INDEX = CONFIG.INDEX + 1
              LINE INPUT #1, CONFIG.LINES$(CONFIG.INDEX)
           LOOP
           CLOSE #1
        END IF

        FOR I = 1 TO CONFIG.INDEX
           A$ = CONFIG.LINES$(I)
           IF LEFT$(A$, 1) = "*" THEN
              GOTO GET.NEXT.CONFIG.RECORD
           END IF
           SP = INSTR(A$, " ")
           EQ = INSTR(A$, "=")
           LA$ = LTRIM$(LEFT$(A$, SP - 1))
           RA$ = LTRIM$(RIGHT$(A$, LEN(A$) - EQ))
           RA$ = RTRIM$(RA$)

           SELECT CASE MENU.SELECTION$
              CASE F1$
                 CALL COLORS.ROUTINE(LA$, RA$, COLOR.INDEX, STORED.COLORS$(), BACKGROUND$, FOREGROUND$, INPUT.FIELDS$, BLINK$, DIM$, DISPLAY.MODE$)
              CASE F2$
                 CALL COMMS.ROUTINE(LA$, RA$, PORT$, SPEED$, PARITY$, DATA.SIZE$, STOP.BITS$, PRN.PORT$, PRN.ENABLE$, PRN.DISABLE$, CMD$, P.SPEED$, P.DATA$, P.PARITY$, P.STOP$, CMDL, TIME.OUT$)
              CASE F3$
                 SELECT CASE LA$
                    CASE "RAM.DRIVE"
                       RAM.DRIVE$ = RA$
                    CASE "MIN.SPACE"
                       MIN.SPACE$ = RA$
                    CASE "EXT.DRIVE"
                       EXT.DRIVE$ = RA$
                    CASE "EXT.MIN.SP"
                       EXT.MIN.SP$ = RA$
                    CASE "REFRESH"
                       R.FREQ$ = RA$
                    CASE "EXT.ENABLE"
                       EXT.ENABLE$ = RA$
                 END SELECT
              CASE F4$
                 SELECT CASE LA$
                    CASE "F1"
                       F1.VALUE$ = RA$
                    CASE "F2"
                       F2.VALUE$ = RA$
                    CASE "F3"
                       F3.VALUE$ = RA$
                    CASE "F4"
                       F4.VALUE$ = RA$
                    CASE "F5"
                       F5.VALUE$ = RA$
                    CASE "F6"
                       F6.VALUE$ = RA$
                    CASE "F7"
                       F7.VALUE$ = RA$
                    CASE "F8"
                       F8.VALUE$ = RA$
                    CASE "F9"
                       F9.VALUE$ = RA$
                    CASE "F10"
                       F10.VALUE$ = RA$
                    CASE "F11"
                       F11.VALUE$ = RA$
                    CASE "F12"
                       F12.VALUE$ = RA$
                 END SELECT
              CASE F5$
                 SELECT CASE LA$
                    CASE "#F1"
                       SF1.VALUE$ = RA$
                    CASE "#F2"
                       SF2.VALUE$ = RA$
                    CASE "#F3"
                       SF3.VALUE$ = RA$
                    CASE "#F4"
                       SF4.VALUE$ = RA$
                    CASE "#F5"
                       SF5.VALUE$ = RA$
                    CASE "#F6"
                       SF6.VALUE$ = RA$
                    CASE "#F7"
                       SF7.VALUE$ = RA$
                    CASE "#F8"
                       SF8.VALUE$ = RA$
                    CASE "#F9"
                       SF9.VALUE$ = RA$
                    CASE "#F10"
                       SF10.VALUE$ = RA$
                    CASE "#F11"
                       SF11.VALUE$ = RA$
                    CASE "#F12"
                       SF12.VALUE$ = RA$
                 END SELECT
              CASE F6$
                 SELECT CASE LA$
                    CASE "^F1"
                       CF1.VALUE$ = RA$
                    CASE "^F2"
                       CF2.VALUE$ = RA$
                    CASE "^F3"
                       CF3.VALUE$ = RA$
                    CASE "^F4"
                       CF4.VALUE$ = RA$
                    CASE "^F5"
                       CF5.VALUE$ = RA$
                    CASE "^F6"
                       CF6.VALUE$ = RA$
                    CASE "^F7"
                       CF7.VALUE$ = RA$
                    CASE "^F8"
                       CF8.VALUE$ = RA$
                    CASE "^F9"
                       CF9.VALUE$ = RA$
                    CASE "^F10"
                       CF10.VALUE$ = RA$
                    CASE "^F11"
                       CF11.VALUE$ = RA$
                    CASE "^F12"
                       CF12.VALUE$ = RA$
                 END SELECT
              CASE F7$
                 SELECT CASE LA$
                    CASE "@F1"
                       AF1.VALUE$ = RA$
                    CASE "@F2"
                       AF2.VALUE$ = RA$
                    CASE "@F3"
                       AF3.VALUE$ = RA$
                    CASE "@F4"
                       AF4.VALUE$ = RA$
                    CASE "@F5"
                       AF5.VALUE$ = RA$
                    CASE "@F6"
                       AF6.VALUE$ = RA$
                    CASE "@F7"
                       AF7.VALUE$ = RA$
                    CASE "@F8"
                       AF8.VALUE$ = RA$
                    CASE "@F9"
                       AF9.VALUE$ = RA$
                    CASE "@F10"
                       AF10.VALUE$ = RA$
                    CASE "@F11"
                       AF11.VALUE$ = RA$
                    CASE "@F12"
                       AF12.VALUE$ = RA$
                 END SELECT
           END SELECT

GET.NEXT.CONFIG.RECORD:

        NEXT I
        RETURN

SELECT.COLORS:

        K = 0
        COLOR 0, 6

        FOR I = 1 TO 6
           IF I < 6 THEN
              FOR J = 1 TO 16
                 IF STORED.COLORS$(I) = COLORS$(J) THEN
                    K = J
                    J = 99
                 END IF
              NEXT J
              IF K = 0 THEN
                 K = 1
              END IF
           ELSE
              DM$ = DISPLAY.MODE$
           END IF

           SELECT CASE I
              CASE 1
                 LOCATE 12, 28, 0
              CASE 2
                 LOCATE 13, 28, 0
              CASE 3
                 LOCATE 14, 28, 0
              CASE 4
                 LOCATE 15, 28, 0
              CASE 5
                 LOCATE 16, 28, 0
              CASE 6
                 LOCATE 18, 28, 0
                 PRINT DM$;
           END SELECT
           IF I < 6 THEN
              PRINT COLORS$(K);
           END IF
SELECT.LOOP:
           W$ = INKEY$
           IF W$ = "" THEN
              GOTO SELECT.LOOP
           END IF
           IF W$ = " " THEN
              CHANGE.MADE = 1
              IF I < 6 THEN
                 K = K + 1
                 IF K > 16 THEN
                    K = 1
                 END IF
              END IF
              LOCATE , 28, 0
              PRINT SPC(30);
              LOCATE , 28, 0
              IF I < 6 THEN
                 PRINT COLORS$(K);
              ELSE
                 IF DM$ = "BLOCK" THEN
                    DM$ = "SCROLL"
                 ELSE
                    DM$ = "BLOCK"
                 END IF
                 PRINT DM$;
              END IF

              GOTO SELECT.LOOP
           END IF

           IF W$ = CHR$(27) THEN
              GOTO SELECT.COLORS.EXIT
           END IF

           IF W$ <> CHR$(13) THEN
              GOTO SELECT.LOOP
           END IF
           '
           ' The user pressed ENTER
           '
           IF I < 6 THEN
              STORED.COLORS$(I) = COLORS$(K)
           ELSE
              DISPLAY.MODE$ = DM$
           END IF
           COLOR 15, 6
           LOCATE , 28, 0
           PRINT SPC(30);
           LOCATE , 28, 0
           IF I < 6 THEN
              PRINT COLORS$(K);
           ELSE
              PRINT DM$;
           END IF
           COLOR 0, 6
        NEXT I

SELECT.COLORS.EXIT:

        FOR I = 1 TO 5
           X = INSTR(STORED.COLORS$(I), "High Intensity ")
           IF X > 0 THEN
              STORED.COLORS$(I) = "H." + RIGHT$(STORED.COLORS$(I), LEN(STORED.COLORS$(I)) - 15)
           END IF
        NEXT I
        RETURN

INPUT.COMMS:
        I.START = 1
INPUT.COMMS.LOOP:
        FOR I = I.START TO CMDL
           SUB.FLAG = 0
           CALL DISPLAY.COMMS.PARAM(I, PORT$, SPEED$, DATA.SIZE$, PARITY$, STOP.BITS$, PRN.PORT$, PRN.ENABLE$, PRN.DISABLE$, CMD$, P.SPEED$, P.DATA$, P.PARITY$, P.STOP$, CMDL, TIME.OUT$)
SELECT.PARAM:
           W$ = INKEY$
           IF W$ = "" THEN
              GOTO SELECT.PARAM
           END IF
           IF W$ = CHR$(27) THEN
              RETURN
           END IF
           IF W$ = " " THEN
               GOTO NEXT.PARAM
           END IF

           IF W$ = CHR$(13) THEN
              SELECT CASE I
                 CASE 1
                    PCOPY C.SCREEN%, 0
                    SCREEN , , 0, 0
                    COLOR 15, 4
                    CALL MESSAGE.1
                    CALL PORT.WINDOW
                    RESTORE PORTS
                    MAX = 2: ROW = 11: COL = 49: ITEM$ = PORT$
                    GOSUB SUB.MENU
                    IF EXIT.FLAG = 1 THEN
                       EXIT.FLAG = 0
                    ELSE
                       IF PORT$ <> MENU.ITEM$(SLCT) THEN
                          PORT$ = MENU.ITEM$(SLCT)
                          CHANGE.MADE = 1
                       END IF
                    END IF
                    IF LEFT$(PRN.PORT$, 3) = "COM" THEN
                       SELECT CASE PORT$
                          CASE "COM1"
                             PRN.PORT$ = "COM2"
                          CASE "COM2"
                             PRN.PORT$ = "COM1"
                       END SELECT
                    END IF
                    SCREEN , , C.SCREEN%, C.SCREEN%
                    COLOR 0, 3
                    LOCATE 10, 23, 0
                    PRINT PORT$;
                    IF CMD$ <> "2" THEN
                       COLOR 15, 1
                       LOCATE 19, 32, 0
                       PRINT PRN.PORT$;
                       IF LEFT$(PRN.PORT$, 3) = "LPT" THEN
                          PRINT SPC(34);
                          CMDL = 9
                       ELSE
                          LOCATE 19, 37, 0
                          PRINT "Speed       Data   Parity   Stop";
                          CALL PRINT.P.PARAMETERS(P.SPEED$, P.PARITY$, P.DATA$, P.STOP$)
                          CMDL = 13
                       END IF
                    END IF
                 CASE 2
SPEED.ROUTINE:
                    PCOPY C.SCREEN%, 0
                    SCREEN , , 0, 0
                    COLOR 15, 4
                    CALL MESSAGE.1
                    CALL SPEED.WINDOW
                    RESTORE SPEED
                    MAX = 10: ROW = 11: COL = 49
                    IF SUB.FLAG = 0 THEN
                       ITEM$ = SPEED$
                    ELSE
                       ITEM$ = P.SPEED$
                    END IF
                    GOSUB SUB.MENU
                    IF EXIT.FLAG = 1 THEN
                       EXIT.FLAG = 0
                    ELSE
                       IF SUB.FLAG = 0 THEN
                          IF SPEED$ <> RTRIM$(MENU.ITEM$(SLCT)) THEN
                             SPEED$ = RTRIM$(MENU.ITEM$(SLCT))
                             CHANGE.MADE = 1
                          END IF
                       ELSE
                          IF P.SPEED$ <> RTRIM$(MENU.ITEM$(SLCT)) THEN
                             P.SPEED$ = RTRIM$(MENU.ITEM$(SLCT))
                             CHANGE.MADE = 1
                          END IF
                       END IF
                    END IF
                    SCREEN , , C.SCREEN%, C.SCREEN%
                    COLOR 15, 1
                    IF SUB.FLAG = 0 THEN
                       LOCATE 11, 23, 0
                       PRINT SPC(10);
                       COLOR 0, 3
                       LOCATE 11, 23, 0
                       PRINT SPEED$;
                    ELSE
                       LOCATE 19, 43, 0
                       PRINT SPC(6);
                       COLOR 0, 3
                       LOCATE 19, 43, 0
                       PRINT P.SPEED$;
                       RETURN
                    END IF
                 CASE 3
DATA.SIZE.ROUTINE:
                    PCOPY C.SCREEN%, 0
                    SCREEN , , 0, 0
                    COLOR 15, 4
                    CALL MESSAGE.1
                    CALL DATA.SIZE.WINDOW
                    RESTORE DATA.SIZE
                    MAX = 2: ROW = 11: COL = 49
                    IF SUB.FLAG = 0 THEN
                       ITEM$ = DATA.SIZE$
                    ELSE
                       ITEM$ = P.DATA$
                    END IF
                    GOSUB SUB.MENU
                    IF EXIT.FLAG = 1 THEN
                       EXIT.FLAG = 0
                    ELSE
                       IF SUB.FLAG = 0 THEN
                          IF DATA.SIZE$ <> RTRIM$(MENU.ITEM$(SLCT)) THEN
                             DATA.SIZE$ = RTRIM$(MENU.ITEM$(SLCT))
                             CHANGE.MADE = 1
                          END IF
                       ELSE
                          IF P.DATA$ <> RTRIM$(MENU.ITEM$(SLCT)) THEN
                             P.DATA$ = RTRIM$(MENU.ITEM$(SLCT))
                             CHANGE.MADE = 1
                          END IF
                       END IF
                    END IF
                    SCREEN , , C.SCREEN%, C.SCREEN%
                    COLOR 15, 1
                    IF SUB.FLAG = 0 THEN
                       LOCATE 12, 23, 0
                       PRINT SPC(10);
                       COLOR 0, 3
                       LOCATE 12, 23, 0
                       PRINT DATA.SIZE$;
                    ELSE
                       LOCATE 19, 54, 0
                       PRINT " ";
                       COLOR 0, 3
                       LOCATE 19, 54, 0
                       PRINT P.DATA$;
                       RETURN
                    END IF
                 CASE 4
PARITY.ROUTINE:
                    PCOPY C.SCREEN%, 0
                    SCREEN , , 0, 0
                    COLOR 15, 4
                    CALL MESSAGE.1
                    CALL PARITY.WINDOW
                    RESTORE PARITY
                    MAX = 3: ROW = 11: COL = 49: ITEM$ = PARITY$
                    IF SUB.FLAG = 1 THEN
                       ITEM$ = P.PARITY$
                    END IF
                    GOSUB SUB.MENU
                    IF EXIT.FLAG = 1 THEN
                       EXIT.FLAG = 0
                    ELSE
                       IF SUB.FLAG = 0 THEN
                          IF PARITY$ <> RTRIM$(MENU.ITEM$(SLCT)) THEN
                             PARITY$ = RTRIM$(MENU.ITEM$(SLCT))
                             CHANGE.MADE = 1
                          END IF
                       ELSE
                          IF P.PARITY$ <> RTRIM$(MENU.ITEM$(SLCT)) THEN
                             P.PARITY$ = RTRIM$(MENU.ITEM$(SLCT))
                             CHANGE.MADE = 1
                          END IF
                       END IF
                    END IF
                    SCREEN , , C.SCREEN%, C.SCREEN%
                    COLOR 15, 1
                    IF SUB.FLAG = 0 THEN
                       LOCATE 13, 23, 0
                       PRINT SPC(10);
                       COLOR 0, 3
                       LOCATE 13, 23, 0
                       PRINT PARITY$;
                    ELSE
                       P.PARITY$ = LEFT$(P.PARITY$, 1)
                       LOCATE 19, 63, 0
                       PRINT " ";
                       COLOR 0, 3
                       LOCATE 19, 63, 0
                       PRINT P.PARITY$;
                       RETURN
                    END IF
                 CASE 5
STOP.BITS.ROUTINE:
                    PCOPY C.SCREEN%, 0
                    SCREEN , , 0, 0
                    COLOR 15, 4
                    CALL MESSAGE.1
                    CALL STOP.BITS.WINDOW
                    RESTORE STOP.BITS
                    MAX = 2: ROW = 11: COL = 49: ITEM$ = STOP.BITS$
                    IF SUB.FLAG = 1 THEN
                       ITEM$ = P.STOP$
                    END IF
                    GOSUB SUB.MENU
                    IF EXIT.FLAG = 1 THEN
                       EXIT.FLAG = 0
                    ELSE
                       IF SUB.FLAG = 0 THEN
                          IF STOP.BITS$ <> RTRIM$(MENU.ITEM$(SLCT)) THEN
                             STOP.BITS$ = RTRIM$(MENU.ITEM$(SLCT))
                             CHANGE.MADE = 1
                          END IF
                       ELSE
                          IF P.STOP$ <> RTRIM$(MENU.ITEM$(SLCT)) THEN
                             P.STOP$ = RTRIM$(MENU.ITEM$(SLCT))
                             CHANGE.MADE = 1
                          END IF
                       END IF
                    END IF
                    SCREEN , , C.SCREEN%, C.SCREEN%
                    COLOR 15, 1
                    IF SUB.FLAG = 0 THEN
                       LOCATE 14, 23, 0
                       PRINT SPC(10);
                       COLOR 0, 3
                       LOCATE 14, 23, 0
                       PRINT STOP.BITS$;
                    ELSE
                       LOCATE 19, 70, 0
                       PRINT " ";
                       COLOR 0, 3
                       LOCATE 19, 70, 0
                       PRINT P.STOP$;
                       RETURN
                    END IF
                 CASE 6
                    PCOPY C.SCREEN%, 0
                    SCREEN , , 0, 0
                    COLOR 15, 4
                    CALL MESSAGE.12
                    RESTORE TIME.OUT
                    FEELDS = 1
                    FOR I2 = 1 TO FEELDS
                       READ SIROW(I2), SICOL(I2), SILEN(I2), TYP(I2)
                    NEXT I2
                    CT$(1) = TIME.OUT$
                    GOSUB DATA.ENTRY.ROUTINE
                    IF EXIT.FLAG = 1 THEN
                       EXIT.FLAG = 0
                    ELSE
                       X$ = LTRIM$(CT$(1))
                       IF TIME.OUT$ <> X$ THEN
                          TIME.OUT$ = LTRIM$(CT$(1))
                          CHANGE.MADE = 1
                       END IF
                    END IF
                    SCREEN , , C.SCREEN%, C.SCREEN%
                    COLOR 15, 1
                    LOCATE 15, 23, 0
                    PRINT SPC(02);
                    COLOR 0, 3
                    LOCATE 15, 23, 0
                    PRINT TIME.OUT$;
                 CASE 7
                    PCOPY C.SCREEN%, 0
                    SCREEN , , 0, 0
                    COLOR 15, 4
                    CALL MESSAGE.2
                    RESTORE PRN.ENABLE
                    FEELDS = 1
                    FOR I2 = 1 TO FEELDS
                       READ SIROW(I2), SICOL(I2), SILEN(I2), TYP(I2)
                    NEXT I2
                    CT$(1) = PRN.ENABLE$
CASE.7.LOOP:
                    GOSUB DATA.ENTRY.ROUTINE
                    IF EXIT.FLAG = 1 THEN
                       EXIT.FLAG = 0
                    ELSE
                       FOR I2 = 1 TO LEN(CT$(1))
                          X = INSTR("0123456789ABCDEF", MID$(CT$(1), I2, 1))
                          IF X = 0 THEN
                             COLOR 15, 0
                             LOCATE 25, 1, 0
                             PRINT "Invalid character '"; MID$(CT$(1), I2, 1); "'";
                             BEEP
                             GOTO CASE.7.LOOP
                          END IF
                       NEXT I2
                       X$ = LTRIM$(CT$(1))
                       X = LEN(X$) MOD 2
                       IF X > 0 THEN
                          Y$ = "0" + RIGHT$(X$, 1)
                          X$ = LEFT$(X$, LEN(X$) - 1)
                          X$ = X$ + Y$
                          CT$(1) = X$
                       END IF
                       IF PRN.ENABLE$ <> X$ THEN
                          PRN.ENABLE$ = LTRIM$(CT$(1))
                          CHANGE.MADE = 1
                       END IF
                    END IF
                    SCREEN , , C.SCREEN%, C.SCREEN%
                    COLOR 15, 1
                    LOCATE 17, 32, 0
                    PRINT SPC(30);
                    COLOR 0, 3
                    LOCATE 17, 32, 0
                    PRINT PRN.ENABLE$;
                 CASE 8
                    PCOPY C.SCREEN%, 0
                    SCREEN , , 0, 0
                    COLOR 15, 4
                    CALL MESSAGE.2
                    RESTORE PRN.DISABLE
                    FEELDS = 1
                    FOR I2 = 1 TO FEELDS
                       READ SIROW(I2), SICOL(I2), SILEN(I2), TYP(I2)
                    NEXT I2
                    CT$(1) = PRN.DISABLE$
CASE.8.LOOP:
                    GOSUB DATA.ENTRY.ROUTINE
                    IF EXIT.FLAG = 1 THEN
                       EXIT.FLAG = 0
                    ELSE
                       FOR I2 = 1 TO LEN(CT$(1))
                          X = INSTR("0123456789ABCDEF", MID$(CT$(1), I2, 1))
                          IF X = 0 THEN
                             COLOR 15, 0
                             LOCATE 25, 1, 0
                             PRINT "Invalid character '"; MID$(CT$(1), I2, 1); "'";
                             BEEP
                             GOTO CASE.8.LOOP
                          END IF
                       NEXT I2
                       X$ = LTRIM$(CT$(1))
                       X = LEN(X$) MOD 2
                       IF X > 0 THEN
                          Y$ = "0" + RIGHT$(X$, 1)
                          X$ = LEFT$(X$, LEN(X$) - 1)
                          X$ = X$ + Y$
                          CT$(1) = X$
                       END IF
                       IF PRN.DISABLE$ <> X$ THEN
                          PRN.DISABLE$ = LTRIM$(CT$(1))
                          CHANGE.MADE = 1
                       END IF
                    END IF
                    SCREEN , , C.SCREEN%, C.SCREEN%
                    COLOR 15, 1
                    LOCATE 18, 32, 0
                    PRINT SPC(30);
                    COLOR 0, 3
                    LOCATE 18, 32, 0
                    PRINT PRN.DISABLE$;
                 CASE 9
                    PCOPY C.SCREEN%, 0
                    SCREEN , , 0, 0
                    COLOR 15, 4
                    CALL MESSAGE.1
                    CALL PRINTER.PORT.WINDOW
                    RESTORE PRINTER.PORTS
                    MAX = 3: ROW = 11: COL = 51
                    IF LEFT$(PRN.PORT$, 3) = "COM" THEN
                       ITEM$ = "COM 1/2"
                    ELSE
                       ITEM$ = PRN.PORT$
                    END IF
                    GOSUB SUB.MENU
                    IF EXIT.FLAG = 1 THEN
                       EXIT.FLAG = 0
                    ELSE
                       IF LEFT$(MENU.ITEM$(SLCT), 3) = "COM" THEN
                          SELECT CASE PORT$
                             CASE "COM1"
                                MENU.ITEM$(SLCT) = "COM2"
                             CASE "COM2"
                                MENU.ITEM$(SLCT) = "COM1"
                           END SELECT
                       END IF
                       IF PRN.PORT$ <> MENU.ITEM$(SLCT) THEN
                          PRN.PORT$ = MENU.ITEM$(SLCT)
                          CHANGE.MADE = 1
                       END IF
                    END IF
                    SCREEN , , C.SCREEN%, C.SCREEN%
                    COLOR 0, 3
                    LOCATE 19, 32, 0
                    PRINT PRN.PORT$;
                    COLOR 15, 1
                    LOCATE 19, 37, 0
                    IF LEFT$(PRN.PORT$, 3) = "LPT" THEN
                       PRINT SPC(34);
                       CMDL = 9
                       I.START = 9
                       GOTO INPUT.COMMS.LOOP
                    ELSE
                       PRINT "Speed       Data   Parity   Stop";
                       CALL PRINT.P.PARAMETERS(P.SPEED$, P.PARITY$, P.DATA$, P.STOP$)
                       CMDL = 13
                       I.START = 9
                       GOTO INPUT.COMMS.LOOP
                    END IF
                 CASE 10
                    SUB.FLAG = 1
                    GOSUB SPEED.ROUTINE
                 CASE 11
                    SUB.FLAG = 1
                    GOSUB DATA.SIZE.ROUTINE
                 CASE 12
                    SUB.FLAG = 1
                    GOSUB PARITY.ROUTINE
                 CASE 13
                    SUB.FLAG = 1
                    GOSUB STOP.BITS.ROUTINE
              END SELECT
           END IF

           GOTO SELECT.PARAM
NEXT.PARAM:
           IF I = CMDL THEN
              I = 0
           END IF
        NEXT I

INPUT.DISK:
        FEELDS = 3
        SCREEN.IMAGE = 1
        RESTORE RAM.DISK.SCREEN
        FOR I2 = 1 TO FEELDS
           READ SIROW(I2), SICOL(I2), SILEN(I2), TYP(I2)
        NEXT I2
        CT$(1) = RAM.DRIVE$
        CT$(2) = MIN.SPACE$
        CT$(3) = EXT.ENABLE$

INPUT.DISK.LOOP:

        GOSUB DATA.ENTRY.ROUTINE

        IF EXIT.FLAG = 1 THEN
           RETURN
        END IF

        IF MID$(CT$(1), 2, 1) <> ":" THEN
           COLOR 15, 0
           LOCATE 25, 1, 0
           PRINT "Drive id should end with ':'";
           BEEP
           GOTO INPUT.DISK.LOOP
        END IF

        IF RIGHT$(CT$(1), 1) <> "\" THEN
           CT$(1) = CT$(1) + "\"
        END IF

        IF RTRIM$(CT$(1)) <> RTRIM$(UCASE$(RAM.DRIVE$)) THEN
           CHANGE.MADE = 1
           RAM.DRIVE$ = RTRIM$(CT$(1))
        END IF

        IF RTRIM$(CT$(2)) <> RTRIM$(MIN.SPACE$) THEN
           CHANGE.MADE = 1
           MIN.SPACE$ = RTRIM$(CT$(2))
        END IF

        IF CT$(3) <> EXT.ENABLE$ THEN
           CHANGE.MADE = 1
           EXT.ENABLE$ = CT$(3)
        END IF

        IF VAL(MIN.SPACE$) = 0 THEN
           COLOR 15, 0
           LOCATE 25, 1, 0
           PRINT "Must be greater than 0";
           BEEP
           I.CU = 2
           GOTO INPUT.DISK.LOOP
        END IF

        IF CT$(3) <> "Y" AND CT$(3) <> "N" THEN
           COLOR 15, 0
           LOCATE 25, 1, 0
           PRINT "Must enter 'Y' or 'N'";
           BEEP
           I.CU = 3
           GOTO INPUT.DISK.LOOP
        END IF

        IF CT$(3) <> EXT.ENABLE$ THEN
           CHANGE.MADE = 1
           EXT.ENABLE$ = CT$(3)
        END IF

        IF CT$(3) = "Y" THEN
           GOTO INPUT.EXT.DISK
        ELSE
           EXT.DRIVE$ = ""
           EXT.MIN.SP$ = ""
           R.FREQ$ = ""
        END IF

        RETURN

INPUT.EXT.DISK:

        CALL DISPLAY.EXTERNAL.DISK.SCREEN
        FEELDS = 3
        SCREEN.IMAGE = 2
        RESTORE EXTERNAL.DISK.SCREEN
        FOR I2 = 1 TO FEELDS
           READ SIROW(I2), SICOL(I2), SILEN(I2), TYP(I2)
        NEXT I2

        CT$(1) = EXT.DRIVE$
        CT$(2) = EXT.MIN.SP$
        CT$(3) = R.FREQ$

INPUT.EXT.DISK.LOOP:

        GOSUB DATA.ENTRY.ROUTINE

        IF EXIT.FLAG = 1 THEN
           IF CT$(1) = "" OR CT$(2) = "" OR CT$(3) = "" THEN
              EXT.ENABLE$ = "N"
              CHANGE.MADE = 1
           END IF
           RETURN
        END IF

        IF MID$(CT$(1), 2, 1) <> ":" THEN
           COLOR 15, 0
           LOCATE 25, 1, 0
           PRINT "Drive id should end with ':'";
           BEEP
           GOTO INPUT.EXT.DISK.LOOP
        END IF

        IF RIGHT$(CT$(1), 1) <> "\" THEN
           CT$(1) = CT$(1) + "\"
        END IF

        IF RTRIM$(CT$(1)) <> RTRIM$(UCASE$(EXT.DRIVE$)) THEN
           CHANGE.MADE = 1
           EXT.DRIVE$ = RTRIM$(CT$(1))
        END IF

        IF RTRIM$(CT$(2)) <> RTRIM$(EXT.MIN.SP$) THEN
           CHANGE.MADE = 1
           EXT.MIN.SP$ = RTRIM$(CT$(2))
        END IF

        IF VAL(EXT.MIN.SP$) = 0 THEN
           COLOR 15, 0
           LOCATE 25, 1, 0
           PRINT "Must be greater than 0";
           BEEP
           I.CU = 2
           GOTO INPUT.EXT.DISK.LOOP
        END IF

        IF RTRIM$(CT$(3)) <> RTRIM$(R.FREQ$) THEN
           CHANGE.MADE = 1
           R.FREQ$ = RTRIM$(CT$(3))
        END IF

        IF VAL(R.FREQ$) = 0 THEN
           COLOR 15, 0
           LOCATE 25, 1, 0
           PRINT "Must be greater than 0";
           BEEP
           I.CU = 3
           GOTO INPUT.EXT.DISK.LOOP
        END IF

        RETURN

INPUT.FKEYS:
        DEFINED.KEY$ = SELECTION$
        TEXT2$ = ""
        FKEY.CHANGE = 0
        FOR I2 = 1 TO 24
          IF I2 = 1 OR I2 = 13 THEN
             ROW = 7
          END IF
          IF I2 < 13 THEN
             COL = 7
             COL2 = 18
          ELSE
             COL = 43
             COL2 = 54
          END IF
          SELECT CASE I2
            CASE 1
               LOCATE 18, 43, 0
               COLOR 0, 6
               PRINT "Command 24";
               LOCATE ROW, COL, 0
               COLOR 15, 6
               PRINT "Command "; LTRIM$(STR$(I2));
            CASE 13
               LOCATE 18, 7, 0
               COLOR 0, 6
               PRINT "Command 12";
               LOCATE ROW, COL, 0
               COLOR 15, 6
               PRINT "Command "; LTRIM$(STR$(I2));
            CASE ELSE
               LOCATE ROW - 1, COL, 0
               COLOR 0, 6
               PRINT "Command "; LTRIM$(STR$(I2 - 1));
               LOCATE ROW, COL, 0
               COLOR 15, 6
               PRINT "Command "; LTRIM$(STR$(I2));
          END SELECT
FKEY.INPUT.LOOP:
          GOSUB GET.SELECTION
          IF SELECTION$ <> CHR$(27) THEN
             CHANGE.MADE = 1
             FKEY.CHANGE = 1
          END IF
          TEXT$ = ""
          SELECT CASE SELECTION$
             CASE F1$
                TEXT$ = "[ENTER]"
             CASE F2$
                TEXT$ = "[HOME]"
             CASE F3$
                TEXT$ = "[END]"
             CASE F4$
                TEXT$ = "[HELP]"
             CASE F5$
                TEXT$ = "[TAB]"
             CASE F6$
                TEXT$ = "[BACK TAB]"
             CASE F7$
                TEXT$ = "[SEND SCREEN]"
             CASE F8$
                TEXT$ = "[RECALL INPUT]"
             CASE F9$
                TEXT$ = "[SHOW KEYS]"
             CASE F10$
                TEXT$ = "[INITIALIZE FIELDS]"
             CASE F11$
                TEXT$ = "[CALCULATOR]"
             CASE F12$
                TEXT$ = "[CALCULATOR W/TRANSFER]"
             CASE DELETE$
                TEXT$ = ""
             CASE CHR$(13)
                PCOPY C.SCREEN%, 0
                SCREEN , , 0, 0
                COLOR 15, 3
                LOCATE 10, 20, 0: PRINT "ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·";
                LOCATE 11, 20, 0: PRINT "º Text command                            º";
                LOCATE 12, 20, 0: PRINT "ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶";
                LOCATE 13, 20, 0: PRINT "º ÄÄÙ - Accept               Esc  - Exit º";
                LOCATE 14, 20, 0: PRINT "ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½";
                CALL PUT.SHADOW(10, 20, 43, 5, "º")
                FEELDS = 1
                RESTORE TEXT.COMMAND
                FOR I3 = 1 TO FEELDS
                   READ SIROW(I3), SICOL(I3), SILEN(I3), TYP(I3)
                NEXT I3
                CT$(1) = ""
                SELECTION$ = ""
                GOSUB DATA.ENTRY.ROUTINE
                SCREEN , , C.SCREEN%, C.SCREEN%
                IF EXIT.FLAG = 1 THEN
                   EXIT.FLAG = 0
                   I2 = I2 - 1
                   GOTO NEXT.COMMAND
                ELSE
                   TEXT$ = CT$(1)
                END IF
             CASE CHR$(27)
                GOTO STORE.COMMAND
             CASE ELSE
                I2 = I2 - 1
                GOTO NEXT.COMMAND
          END SELECT

          IF TEXT$ <> "" THEN
             IF LEN(TEXT2$) = 0 THEN
                TEXT2$ = TEXT$
             ELSE
                TEXT2$ = TEXT2$ + "+" + TEXT$
             END IF
          END IF

          LOCATE ROW, COL2, 0
          COLOR 15, 6
          PRINT SPC(23);
          LOCATE ROW, COL2, 0
          PRINT TEXT$;
          ROW = ROW + 1
NEXT.COMMAND:
        NEXT I2

STORE.COMMAND:
        IF FKEY.CHANGE = 0 THEN
           RETURN
        END IF

        SELECT CASE MENU.SELECTION$
           CASE F4$, F5$, F6$, F7$
              SELECT CASE DEFINED.KEY$
                 CASE F1$
                     F1.VALUE$ = TEXT2$
                 CASE F2$
                     F2.VALUE$ = TEXT2$
                 CASE F3$
                     F3.VALUE$ = TEXT2$
                 CASE F4$
                     F4.VALUE$ = TEXT2$
                 CASE F5$
                     F5.VALUE$ = TEXT2$
                 CASE F6$
                     F6.VALUE$ = TEXT2$
                 CASE F7$
                     F7.VALUE$ = TEXT2$
                 CASE F8$
                     F8.VALUE$ = TEXT2$
                 CASE F9$
                     F9.VALUE$ = TEXT2$
                 CASE F10$
                     F10.VALUE$ = TEXT2$
                 CASE F11$
                     F11.VALUE$ = TEXT2$
                 CASE F12$
                     F12.VALUE$ = TEXT2$
                 CASE SF1$
                     SF1.VALUE$ = TEXT2$
                 CASE SF2$
                     SF2.VALUE$ = TEXT2$
                 CASE SF3$
                     SF3.VALUE$ = TEXT2$
                 CASE SF4$
                     SF4.VALUE$ = TEXT2$
                 CASE SF5$
                     SF5.VALUE$ = TEXT2$
                 CASE SF6$
                     SF6.VALUE$ = TEXT2$
                 CASE SF7$
                     SF7.VALUE$ = TEXT2$
                 CASE SF8$
                     SF8.VALUE$ = TEXT2$
                 CASE SF9$
                     SF9.VALUE$ = TEXT2$
                 CASE SF10$
                     SF10.VALUE$ = TEXT2$
                 CASE SF11$
                     SF11.VALUE$ = TEXT2$
                 CASE SF12$
                     SF12.VALUE$ = TEXT2$
                 CASE CF1$
                     CF1.VALUE$ = TEXT2$
                 CASE CF2$
                     CF2.VALUE$ = TEXT2$
                 CASE CF3$
                     CF3.VALUE$ = TEXT2$
                 CASE CF4$
                     CF4.VALUE$ = TEXT2$
                 CASE CF5$
                     CF5.VALUE$ = TEXT2$
                 CASE CF6$
                     CF6.VALUE$ = TEXT2$
                 CASE CF7$
                     CF7.VALUE$ = TEXT2$
                 CASE CF8$
                     CF8.VALUE$ = TEXT2$
                 CASE CF9$
                     CF9.VALUE$ = TEXT2$
                 CASE CF10$
                     CF10.VALUE$ = TEXT2$
                 CASE CF11$
                     CF11.VALUE$ = TEXT2$
                 CASE CF12$
                     CF12.VALUE$ = TEXT2$
                 CASE AF1$
                     AF1.VALUE$ = TEXT2$
                 CASE AF2$
                     AF2.VALUE$ = TEXT2$
                 CASE AF3$
                     AF3.VALUE$ = TEXT2$
                 CASE AF4$
                     AF4.VALUE$ = TEXT2$
                 CASE AF5$
                     AF5.VALUE$ = TEXT2$
                 CASE AF6$
                     AF6.VALUE$ = TEXT2$
                 CASE AF7$
                     AF7.VALUE$ = TEXT2$
                 CASE AF8$
                     AF8.VALUE$ = TEXT2$
                 CASE AF9$
                     AF9.VALUE$ = TEXT2$
                 CASE AF10$
                     AF10.VALUE$ = TEXT2$
                 CASE AF11$
                     AF11.VALUE$ = TEXT2$
                 CASE AF12$
                     AF12.VALUE$ = TEXT2$
              END SELECT
           CASE F8$
              SELECT CASE DEFINED.KEY$
                 CASE F1$
                     LINE.COMMANDS$(1) = TEXT2$
                 CASE F2$
                     LINE.COMMANDS$(2) = TEXT2$
                 CASE F3$
                     LINE.COMMANDS$(3) = TEXT2$
                 CASE F4$
                     LINE.COMMANDS$(4) = TEXT2$
                 CASE F5$
                     LINE.COMMANDS$(5) = TEXT2$
                 CASE F6$
                     LINE.COMMANDS$(6) = TEXT2$
                 CASE F7$
                     LINE.COMMANDS$(7) = TEXT2$
                 CASE F8$
                     LINE.COMMANDS$(8) = TEXT2$
                 CASE F9$
                     LINE.COMMANDS$(9) = TEXT2$
                 CASE F10$
                     LINE.COMMANDS$(10) = TEXT2$
                 CASE F11$
                     LINE.COMMANDS$(11) = TEXT2$
                 CASE F12$
                     LINE.COMMANDS$(12) = TEXT2$
              END SELECT

        END SELECT
        RETURN

ANALIZE.COMMANDS:

        COMMAND.INDEX = 1

        ERASE FKEYS.COMMANDS$, AUTO.COMMANDS$
        REM $DYNAMIC
        REDIM FKEYS.COMMANDS$(24), AUTO.COMMANDS$(24)

        SELECT CASE MENU.SELECTION$
           CASE F4$
              SELECT CASE SELECTION$
                 CASE F1$
                    TEXT$ = F1.VALUE$
                    A1$ = "Defining F1 Function Key"
                 CASE F2$
                    TEXT$ = F2.VALUE$
                    A1$ = "Defining F2 Function Key"
                 CASE F3$
                    TEXT$ = F3.VALUE$
                    A1$ = "Defining F3 Function Key"
                 CASE F4$
                    TEXT$ = F4.VALUE$
                    A1$ = "Defining F4 Function Key"
                 CASE F5$
                    TEXT$ = F5.VALUE$
                    A1$ = "Defining F5 Function Key"
                 CASE F6$
                    TEXT$ = F6.VALUE$
                    A1$ = "Defining F6 Function Key"
                 CASE F7$
                    TEXT$ = F7.VALUE$
                    A1$ = "Defining F7 Function Key"
                 CASE F8$
                    TEXT$ = F8.VALUE$
                    A1$ = "Defining F8 Function Key"
                 CASE F9$
                    TEXT$ = F9.VALUE$
                    A1$ = "Defining F9 Function Key"
                 CASE F10$
                    TEXT$ = F10.VALUE$
                    A1$ = "Defining F10 Function Key"
                 CASE F11$
                    TEXT$ = F11.VALUE$
                    A1$ = "Defining F11 Function Key"
                 CASE F12$
                    TEXT$ = F12.VALUE$
                    A1$ = "Defining F12 Function Key"
                 CASE ELSE
                    ERROR.CODE = 1
                    RETURN
              END SELECT
           CASE F5$
              SELECT CASE SELECTION$
                 CASE SF1$
                    TEXT$ = SF1.VALUE$
                    A1$ = "Defining SHIFT-F1 Function Key"
                 CASE SF2$
                    TEXT$ = SF2.VALUE$
                    A1$ = "Defining SHIFT-F2 Function Key"
                 CASE SF3$
                    TEXT$ = SF3.VALUE$
                    A1$ = "Defining SHIFT-F3 Function Key"
                 CASE SF4$
                    TEXT$ = SF4.VALUE$
                    A1$ = "Defining SHIFT-F4 Function Key"
                 CASE SF5$
                    TEXT$ = SF5.VALUE$
                    A1$ = "Defining SHIFT-F5 Function Key"
                 CASE SF6$
                    TEXT$ = SF6.VALUE$
                    A1$ = "Defining SHIFT-F6 Function Key"
                 CASE SF7$
                    TEXT$ = SF7.VALUE$
                    A1$ = "Defining SHIFT-F7 Function Key"
                 CASE SF8$
                    TEXT$ = SF8.VALUE$
                    A1$ = "Defining SHIFT-F8 Function Key"
                 CASE SF9$
                    TEXT$ = SF9.VALUE$
                    A1$ = "Defining SHIFT-F9 Function Key"
                 CASE SF10$
                    TEXT$ = SF10.VALUE$
                    A1$ = "Defining SHIFT-F10 Function Key"
                 CASE SF11$
                    TEXT$ = SF11.VALUE$
                    A1$ = "Defining SHIFT-F11 Function Key"
                 CASE SF12$
                    TEXT$ = SF12.VALUE$
                    A1$ = "Defining SHIFT-F12 Function Key"
                 CASE ELSE
                    ERROR.CODE = 1
                    RETURN
              END SELECT
           CASE F6$
              SELECT CASE SELECTION$
                 CASE CF1$
                    TEXT$ = CF1.VALUE$
                    A1$ = "Defining CTRL-F1 Function Key"
                 CASE CF2$
                    TEXT$ = CF2.VALUE$
                    A1$ = "Defining CTRL-F2 Function Key"
                 CASE CF3$
                    TEXT$ = CF3.VALUE$
                    A1$ = "Defining CTRL-F3 Function Key"
                 CASE CF4$
                    TEXT$ = CF4.VALUE$
                    A1$ = "Defining CTRL-F4 Function Key"
                 CASE CF5$
                    TEXT$ = CF5.VALUE$
                    A1$ = "Defining CTRL-F5 Function Key"
                 CASE CF6$
                    TEXT$ = CF6.VALUE$
                    A1$ = "Defining CTRL-F6 Function Key"
                 CASE CF7$
                    TEXT$ = CF7.VALUE$
                    A1$ = "Defining CTRL-F7 Function Key"
                 CASE CF8$
                    TEXT$ = CF8.VALUE$
                    A1$ = "Defining CTRL-F8 Function Key"
                 CASE CF9$
                    TEXT$ = CF9.VALUE$
                    A1$ = "Defining CTRL-F9 Function Key"
                 CASE CF10$
                    TEXT$ = CF10.VALUE$
                    A1$ = "Defining CTRL-F10 Function Key"
                 CASE CF11$
                    TEXT$ = CF11.VALUE$
                    A1$ = "Defining CTRL-F11 Function Key"
                 CASE CF12$
                    TEXT$ = CF12.VALUE$
                    A1$ = "Defining CTRL-F12 Function Key"
                 CASE ELSE
                    ERROR.CODE = 1
                    RETURN
              END SELECT
           CASE F7$
              SELECT CASE SELECTION$
                 CASE AF1$
                    TEXT$ = AF1.VALUE$
                    A1$ = "Defining ALT-F1 Function Key"
                 CASE AF2$
                    TEXT$ = AF2.VALUE$
                    A1$ = "Defining ALT-F2 Function Key"
                 CASE AF3$
                    TEXT$ = AF3.VALUE$
                    A1$ = "Defining ALT-F3 Function Key"
                 CASE AF4$
                    TEXT$ = AF4.VALUE$
                    A1$ = "Defining ALT-F4 Function Key"
                 CASE AF5$
                    TEXT$ = AF5.VALUE$
                    A1$ = "Defining ALT-F5 Function Key"
                 CASE AF6$
                    TEXT$ = AF6.VALUE$
                    A1$ = "Defining ALT-F6 Function Key"
                 CASE AF7$
                    TEXT$ = AF7.VALUE$
                    A1$ = "Defining ALT-F7 Function Key"
                 CASE AF8$
                    TEXT$ = AF8.VALUE$
                    A1$ = "Defining ALT-F8 Function Key"
                 CASE AF9$
                    TEXT$ = AF9.VALUE$
                    A1$ = "Defining ALT-F9 Function Key"
                 CASE AF10$
                    TEXT$ = AF10.VALUE$
                    A1$ = "Defining ALT-F10 Function Key"
                 CASE AF11$
                    TEXT$ = AF11.VALUE$
                    A1$ = "Defining ALT-F11 Function Key"
                 CASE AF12$
                    TEXT$ = AF12.VALUE$
                    A1$ = "Defining ALT-F12 Function Key"
                 CASE ELSE
                    ERROR.CODE = 1
                    RETURN
              END SELECT
           CASE F8$
              SELECT CASE SELECTION$
                 CASE F1$
                    TEXT$ = LINE.COMMANDS$(1)
                    A1$ = "Defining AutoStart Line 1"
                 CASE F2$
                    TEXT$ = LINE.COMMANDS$(2)
                    A1$ = "Defining AutoStart Line 2"
                 CASE F3$
                    TEXT$ = LINE.COMMANDS$(3)
                    A1$ = "Defining AutoStart Line 3"
                 CASE F4$
                    TEXT$ = LINE.COMMANDS$(4)
                    A1$ = "Defining AutoStart Line 4"
                 CASE F5$
                    TEXT$ = LINE.COMMANDS$(5)
                    A1$ = "Defining AutoStart Line 5"
                 CASE F6$
                    TEXT$ = LINE.COMMANDS$(6)
                    A1$ = "Defining AutoStart Line 6"
                 CASE F7$
                    TEXT$ = LINE.COMMANDS$(7)
                    A1$ = "Defining AutoStart Line 7"
                 CASE F8$
                    TEXT$ = LINE.COMMANDS$(8)
                    A1$ = "Defining AutoStart Line 8"
                 CASE F9$
                    TEXT$ = LINE.COMMANDS$(9)
                    A1$ = "Defining AutoStart Line 9"
                 CASE F10$
                    TEXT$ = LINE.COMMANDS$(10)
                    A1$ = "Defining AutoStart Line 10"
                 CASE F11$
                    TEXT$ = LINE.COMMANDS$(11)
                    A1$ = "Defining AutoStart Line 11"
                 CASE F12$
                    TEXT$ = LINE.COMMANDS$(12)
                    A1$ = "Defining AutoStart Line 12"
                 CASE ELSE
                    ERROR.CODE = 1
                    RETURN
              END SELECT
        END SELECT

ANALIZE.COMMANDS.LOOP:

        SELECT CASE MENU.SELECTION$
           CASE F4$, F5$, F6$, F7$
              X = INSTR(TEXT$, "+")
              IF X = 0 THEN
                 FKEYS.COMMANDS$(COMMAND.INDEX) = TEXT$
                 RETURN
              END IF
              FKEYS.COMMANDS$(COMMAND.INDEX) = LEFT$(TEXT$, X - 1)
              TEXT$ = RIGHT$(TEXT$, LEN(TEXT$) - X)
              COMMAND.INDEX = COMMAND.INDEX + 1
              GOTO ANALIZE.COMMANDS.LOOP
           CASE F8$
              X = INSTR(TEXT$, "+")
              IF X = 0 THEN
                 AUTO.COMMANDS$(COMMAND.INDEX) = TEXT$
                 RETURN
              END IF
              AUTO.COMMANDS$(COMMAND.INDEX) = LEFT$(TEXT$, X - 1)
              TEXT$ = RIGHT$(TEXT$, LEN(TEXT$) - X)
              COMMAND.INDEX = COMMAND.INDEX + 1
              GOTO ANALIZE.COMMANDS.LOOP
        END SELECT
SAVE.CHANGES:
        COLOR 15, 4
        SELECT CASE MENU.SELECTION$
           CASE F1$
              CALL SAVE.COLORS.MESSAGE
              X = 13: Y = 56: Y.OR.N$ = ""
              GOSUB GET.YES.OR.NO
              IF Y.OR.N$ = "Y" THEN
                 CALL STORE.COLORS(CONFIG.INDEX, CONFIG.LINES$(), STORED.COLORS$(), DISPLAY.MODE$)
                 CALL WRITE.CONFIG.FILE(CONFIG.LINES$(), CONFIG.INDEX)
              END IF
           CASE F2$
              CALL SAVE.COMMS.MESSAGE
              X = 13: Y = 56: Y.OR.N$ = ""
              GOSUB GET.YES.OR.NO
              IF Y.OR.N$ = "Y" THEN
                 CALL STORE.COMMS(PORT$, SPEED$, PARITY$, DATA.SIZE$, STOP.BITS$, PRN.PORT$, PRN.ENABLE$, PRN.DISABLE$, P.SPEED$, P.DATA$, P.PARITY$, P.STOP$, CONFIG.INDEX, CONFIG.LINES$(), TIME.OUT$)
                 CALL WRITE.CONFIG.FILE(CONFIG.LINES$(), CONFIG.INDEX)
              END IF
           CASE F3$
              CALL SAVE.DISK.MESSAGE
              X = 13: Y = 56: Y.OR.N$ = ""
              GOSUB GET.YES.OR.NO
              IF Y.OR.N$ = "Y" THEN
                 CALL STORE.DISK(CONFIG.INDEX, CONFIG.LINES$(), RAM.DRIVE$, MIN.SPACE$, EXT.DRIVE$, EXT.MIN.SP$, R.FREQ$, EXT.ENABLE$)
                 CALL WRITE.CONFIG.FILE(CONFIG.LINES$(), CONFIG.INDEX)
              END IF
           CASE F4$
              CALL SAVE.FKEYS.MESSAGE
              X = 13: Y = 56: Y.OR.N$ = ""
              GOSUB GET.YES.OR.NO
              IF Y.OR.N$ = "Y" THEN
                 FOR J = 1 TO CONFIG.INDEX
                    A$ = CONFIG.LINES$(J)
                    IF LEFT$(A$, 1) <> "*" THEN
                       SELECT CASE LEFT$(A$, 12)
                          CASE "F1         ="
                             A$ = "F1         = " + F1.VALUE$
                          CASE "F2         ="
                             A$ = "F2         = " + F2.VALUE$
                          CASE "F3         ="
                             A$ = "F3         = " + F3.VALUE$
                          CASE "F4         ="
                             A$ = "F4         = " + F4.VALUE$
                          CASE "F5         ="
                             A$ = "F5         = " + F5.VALUE$
                          CASE "F6         ="
                             A$ = "F6         = " + F6.VALUE$
                          CASE "F7         ="
                             A$ = "F7         = " + F7.VALUE$
                          CASE "F8         ="
                             A$ = "F8         = " + F8.VALUE$
                          CASE "F9         ="
                             A$ = "F9         = " + F9.VALUE$
                          CASE "F10        ="
                             A$ = "F10        = " + F10.VALUE$
                          CASE "F11        ="
                             A$ = "F11        = " + F11.VALUE$
                          CASE "F12        ="
                             A$ = "F12        = " + F12.VALUE$
                       END SELECT
                       CONFIG.LINES$(J) = A$
                    END IF
                 NEXT J
                 CALL WRITE.CONFIG.FILE(CONFIG.LINES$(), CONFIG.INDEX)
              END IF
           CASE F5$
              CALL SAVE.SFKEYS.MESSAGE
              X = 13: Y = 56: Y.OR.N$ = ""
              GOSUB GET.YES.OR.NO
              IF Y.OR.N$ = "Y" THEN
                 FOR J = 1 TO CONFIG.INDEX
                    A$ = CONFIG.LINES$(J)
                    IF LEFT$(A$, 1) <> "*" THEN
                       SELECT CASE LEFT$(A$, 12)
                          CASE "#F1        ="
                             A$ = "#F1        = " + SF1.VALUE$
                          CASE "#F2        ="
                             A$ = "#F2        = " + SF2.VALUE$
                          CASE "#F3        ="
                             A$ = "#F3        = " + SF3.VALUE$
                          CASE "#F4        ="
                             A$ = "#F4        = " + SF4.VALUE$
                          CASE "#F5        ="
                             A$ = "#F5        = " + SF5.VALUE$
                          CASE "#F6        ="
                             A$ = "#F6        = " + SF6.VALUE$
                          CASE "#F7        ="
                             A$ = "#F7        = " + SF7.VALUE$
                          CASE "#F8        ="
                             A$ = "#F8        = " + SF8.VALUE$
                          CASE "#F9        ="
                             A$ = "#F9        = " + SF9.VALUE$
                          CASE "#F10       ="
                             A$ = "#F10       = " + SF10.VALUE$
                          CASE "#F11       ="
                             A$ = "#F11       = " + SF11.VALUE$
                          CASE "#F12       ="
                             A$ = "#F12       = " + SF12.VALUE$
                       END SELECT
                       CONFIG.LINES$(J) = A$
                    END IF
                 NEXT J
                 CALL WRITE.CONFIG.FILE(CONFIG.LINES$(), CONFIG.INDEX)
              END IF
           CASE F6$
              CALL SAVE.CFKEYS.MESSAGE
              X = 13: Y = 56: Y.OR.N$ = ""
              GOSUB GET.YES.OR.NO
              IF Y.OR.N$ = "Y" THEN
                 FOR J = 1 TO CONFIG.INDEX
                    A$ = CONFIG.LINES$(J)
                    IF LEFT$(A$, 1) <> "*" THEN
                       SELECT CASE LEFT$(A$, 12)
                          CASE "^F1        ="
                             A$ = "^F1        = " + CF1.VALUE$
                          CASE "^F2        ="
                             A$ = "^F2        = " + CF2.VALUE$
                          CASE "^F3        ="
                             A$ = "^F3        = " + CF3.VALUE$
                          CASE "^F4        ="
                             A$ = "^F4        = " + CF4.VALUE$
                          CASE "^F5        ="
                             A$ = "^F5        = " + CF5.VALUE$
                          CASE "^F6        ="
                             A$ = "^F6        = " + CF6.VALUE$
                          CASE "^F7        ="
                             A$ = "^F7        = " + CF7.VALUE$
                          CASE "^F8        ="
                             A$ = "^F8        = " + CF8.VALUE$
                          CASE "^F9        ="
                             A$ = "^F9        = " + CF9.VALUE$
                          CASE "^F10       ="
                             A$ = "^F10       = " + CF10.VALUE$
                          CASE "^F11       ="
                             A$ = "^F11       = " + CF11.VALUE$
                          CASE "^F12       ="
                             A$ = "^F12       = " + CF12.VALUE$
                       END SELECT
                       CONFIG.LINES$(J) = A$
                    END IF
                 NEXT J
                 CALL WRITE.CONFIG.FILE(CONFIG.LINES$(), CONFIG.INDEX)
              END IF
           CASE F7$
              CALL SAVE.AFKEYS.MESSAGE
              X = 13: Y = 56: Y.OR.N$ = ""
              GOSUB GET.YES.OR.NO
              IF Y.OR.N$ = "Y" THEN
                 FOR J = 1 TO CONFIG.INDEX
                    A$ = CONFIG.LINES$(J)
                    IF LEFT$(A$, 1) <> "*" THEN
                       SELECT CASE LEFT$(A$, 12)
                          CASE "@F1        ="
                             A$ = "@F1        = " + AF1.VALUE$
                          CASE "@F2        ="
                             A$ = "@F2        = " + AF2.VALUE$
                          CASE "@F3        ="
                             A$ = "@F3        = " + AF3.VALUE$
                          CASE "@F4        ="
                             A$ = "@F4        = " + AF4.VALUE$
                          CASE "@F5        ="
                             A$ = "@F5        = " + AF5.VALUE$
                          CASE "@F6        ="
                             A$ = "@F6        = " + AF6.VALUE$
                          CASE "@F7        ="
                             A$ = "@F7        = " + AF7.VALUE$
                          CASE "@F8        ="
                             A$ = "@F8        = " + AF8.VALUE$
                          CASE "@F9        ="
                             A$ = "@F9        = " + AF9.VALUE$
                          CASE "@F10       ="
                             A$ = "@F10       = " + AF10.VALUE$
                          CASE "@F11       ="
                             A$ = "@F11       = " + AF11.VALUE$
                          CASE "@F12       ="
                             A$ = "@F12       = " + AF12.VALUE$
                       END SELECT
                       CONFIG.LINES$(J) = A$
                    END IF
                 NEXT J
                 CALL WRITE.CONFIG.FILE(CONFIG.LINES$(), CONFIG.INDEX)
              END IF
           CASE F8$
              CALL SAVE.AUTO.MESSAGE
              X = 13: Y = 56: Y.OR.N$ = ""
              GOSUB GET.YES.OR.NO
              IF Y.OR.N$ = "Y" THEN
                 CALL INCRIPT.DECRIPT(ID$)
                 CALL INCRIPT.DECRIPT(PASSWORD$)
                 FILENUM = FREEFILE
                 OPEN "AUTOEXEC.CS" FOR OUTPUT AS FILENUM
                 PRINT #FILENUM, "[HOME]+LOGON+[ENTER]"
                 PRINT #FILENUM, ID$
                 PRINT #FILENUM, PASSWORD$
                 LINE.COUNT = 3
                 FOR I2 = 1 TO 12
                    IF LINE.COMMANDS$(I2) <> "" THEN
                       PRINT #FILENUM, LINE.COMMANDS$(I2)
                       LINE.COUNT = LINE.COUNT + 1
                    END IF
                 NEXT I2
                 IF LINE.COUNT > 3 THEN
                    PRINT #FILENUM, "[HOME]+LOGOFF+[ENTER]"
                 ELSE
                    PRINT #FILENUM, "[END]+LOGOFF+[ENTER]"
                 END IF
                 CLOSE #FILENUM
              END IF
        END SELECT
        RETURN

GET.YES.OR.NO:

        COLOR 0, 7
        LOCATE X, Y, 1, 0, 31
        Y.OR.N$ = INKEY$
        Y.OR.N$ = UCASE$(Y.OR.N$)
        PRINT Y.OR.N$;
        IF Y.OR.N$ <> "Y" AND Y.OR.N$ <> "N" THEN
           GOTO GET.YES.OR.NO
        END IF
        RETURN

DATA.ENTRY.ROUTINE:

        EXIT.FLAG = 0

DATA.ENTRY.ROUTINE.START:

        FOR CURRENT.FIELD = 1 TO FEELDS
           LOCATE SIROW(CURRENT.FIELD), SICOL(CURRENT.FIELD), 0
           COLOR 0, 7
           IF BGCLR = 0 THEN
              PRINT STRING$(LEN(CT$(CURRENT.FIELD)), "*"); STRING$(SILEN(CURRENT.FIELD) - LEN(CT$(CURRENT.FIELD)), F$);
           ELSE
              PRINT CT$(CURRENT.FIELD); STRING$(SILEN(CURRENT.FIELD) - LEN(CT$(CURRENT.FIELD)), F$);
           END IF
        NEXT CURRENT.FIELD

        FOR CURRENT.FIELD = I.CU TO FEELDS

           IF MENU.SELECTION$ = F3$ THEN
              COLOR 15, 4
              IF SCREEN.IMAGE = 1 THEN
                 SELECT CASE CURRENT.FIELD
                    CASE 1
                       CALL MESSAGE.3
                    CASE 2
                       CALL MESSAGE.4
                    CASE 3
                       CALL MESSAGE.8
                 END SELECT
              ELSE
                 SELECT CASE CURRENT.FIELD
                    CASE 1
                       CALL MESSAGE.9
                    CASE 2
                       CALL MESSAGE.10
                    CASE 3
                       CALL MESSAGE.11
                 END SELECT
              END IF
           END IF

           COLOR 0, 7
           INS.FLAG = 0
JUMP1:
           IF C1 = 1 THEN GOTO JUMP2
           LOCATE SIROW(CURRENT.FIELD), SICOL(CURRENT.FIELD), 0
           IF BGCLR = 0 THEN
              COLOR 0, 7
              PRINT STRING$(LEN(CT$(CURRENT.FIELD)), "*");
              COLOR BGCLR, BGCLR
           ELSE
              PRINT CT$(CURRENT.FIELD);
           END IF

JUMP2:
           X = SIROW(CURRENT.FIELD)
           Y = SICOL(CURRENT.FIELD)

           FOR POZ = 1 TO SILEN(CURRENT.FIELD) + 1

             IF POZ = SILEN(CURRENT.FIELD) + 1 THEN
                GOTO NEXT.POSITION
             END IF

GET.CHARACTER:
             LOCATE X, Y, 1, 0, 31

             IN.ARRAY%(ax) = &H700
             CALL INT86OLD(&H21, IN.ARRAY%(), OUT.ARRAY%())
             W$ = RIGHT$(HEX$(OUT.ARRAY%(ax)), 2)
             W$ = CHR$(HEX.TO.DECIMAL(W$))

             IF W$ = CHR$(0) THEN
                SPECIAL.CHAR.FLAG = 1
                GOTO GET.CHARACTER
             END IF

             IF SPECIAL.CHAR.FLAG = 1 THEN
                SPECIAL.CHAR.FLAG = 0
                W$ = CHR$(0) + W$
             ELSE
                SPECIAL.CHAR.FLAG = 0
             END IF

             IF W$ = CHR$(27) THEN
                EXIT.FLAG = 1
                RETURN
             END IF

             IF LEN(W$) = 2 THEN
                Z$ = RIGHT$(W$, 1)
                SELECT CASE Z$
                   CASE CHR$(77)
                      CALL PROCESS.RIGHT.ARROW(X, Y, R.KEY)
                   CASE CHR$(75)
                      CALL PROCESS.LEFT.ARROW(CURRENT.FIELD, POZ, SILEN(), SIROW(), SICOL(), L.KEY, RP.KEY, Y, X)
                   CASE CHR$(72)
                      CALL PROCESS.UP.ARROW(SIROW(), SICOL(), CURRENT.FIELD, FEELDS)
                      GOTO NEXT.FIELD
                   CASE CHR$(80)
                      CALL PROCESS.DOWN.ARROW(SIROW(), SICOL(), CURRENT.FIELD, FEELDS)
                      GOTO NEXT.FIELD
                   CASE CHR$(82)
                      CALL PROCESS.INSERT.KEY(POZ, CT$(), CURRENT.FIELD, SIROW(), SICOL(), SILEN(), X, Y, L3)
                      GOTO GET.CHARACTER
                   CASE CHR$(83)
                      CALL PROCESS.DELETE.KEY(POZ, CT$(), CURRENT.FIELD, SIROW(), SICOL(), SILEN(), F$, X, Y)
                      GOTO GET.CHARACTER
                END SELECT
             END IF

             IF W$ = CHR$(27) OR W$ = CHR$(3) THEN
                POZ = POZ - 1
                GOTO NEXT.POSITION
             END IF

             IF E.KEY = 1 THEN
                E.KEY = 0
                GOTO NEXT.FIELD
             END IF

             IF L.KEY = 2 THEN
                L.KEY = 0
                GOTO JUMP1
             END IF

             IF R.KEY = 1 THEN
                R.KEY = 0
                RP.KEY = 1
                GOTO NEXT.POSITION
             END IF

             IF RP.KEY = 1 AND W$ <> "" THEN
               SP = INSTR(AL$, W$)
             END IF

             IF RP.KEY = 1 AND (SP > 0 OR LEN(W$) = 2 OR POZ = 1) THEN
                GOTO JUMP4
             END IF

             IF RP.KEY = 1 AND W$ <> "" THEN
                GOSUB ANALIZE.RIGHT.ARROW
                IF ERROR.CODE = 1 THEN
                   ERROR.CODE = 0
                   GOTO GET.CHARACTER
                END IF
                Y = Y + 1
                GOTO NEXT.POSITION
             END IF

             IF L.KEY = 1 THEN
                L.KEY = 0
                POZ = POZ - 2
                RP.KEY = 1
                GOTO NEXT.POSITION
             END IF

JUMP4:
             IF W$ = "" THEN GOTO GET.CHARACTER

             IF POZ = 1 AND W$ = CHR$(8) THEN
                GOTO GET.CHARACTER
             END IF

             IF POZ - LEN(CT$(CURRENT.FIELD)) > 1 AND W$ = CHR$(8) THEN
                POZ = LEN(CT$(CURRENT.FIELD))
                Y = SICOL(CURRENT.FIELD) + LEN(CT$(CURRENT.FIELD))
                LOCATE X, Y, 1, 0, 31
                GOTO NEXT.POSITION
             END IF

             IF POZ > LEN(CT$(CURRENT.FIELD)) THEN
                BS.KEY = 0
             END IF

             IF W$ = TAB$ AND POZ = 1 THEN GOTO JUMP7

             IF W$ = TAB$ THEN
                POZ = SILEN(CURRENT.FIELD) + 1
                Y = SICOL(CURRENT.FIELD)
                GOTO NEXT.POSITION
             END IF

             IF W$ = CHR$(8) THEN
                GOTO PROCESS.BACK.SPACE
             END IF

             CALL VALIDATE.INPUT (ALLOWED.KEYS$, W$, ERROR.CODE, BACK.TAB$, ASCII$, TYP(), ACCEPT$, HELP$, HOME$, END.KEY$, CURRENT.FIELD)

             IF ERROR.CODE = 1 THEN
                ERROR.CODE = 0
                LOCATE 25, 1, 0
                COLOR 15, 0
                PRINT "Invalid character for the field type ...";
                BEEP
                GOTO GET.CHARACTER
             END IF

             COLOR BGCLR, UNPRO
             LOCATE 25, 1, 0: PRINT SPC(79);
             COLOR UNPRO, BGCLR

CONTINUE.CHECK:

             SELECT CASE W$
                CASE BACK.TAB$
                   C1 = 1
                   IF CURRENT.FIELD = 1 THEN
                      CURRENT.FIELD = CURRENT.FIELD - 1
                      GOTO NEXT.FIELD
                   ELSE
                      CURRENT.FIELD = CURRENT.FIELD - 2
                      GOTO NEXT.FIELD
                   END IF
                CASE ACCEPT$
                   EXIT.FLAG = 2
                   RETURN
                CASE HOME$
                   CURRENT.FIELD = 0
                   GOTO NEXT.FIELD
                CASE END.KEY$
                   CURRENT.FIELD = FEELDS - 1
                   GOTO NEXT.FIELD
             END SELECT

             IF POZ = 1 THEN
                IF INS.FLAG = 0 THEN
                   IF BS.KEY <> 1 THEN
                      IF LEN(CT$(CURRENT.FIELD)) <= SILEN(CURRENT.FIELD) THEN
                         IF BGCLR = 0 THEN
                            COLOR 0, 7
                            LOCATE SIROW(CURRENT.FIELD), SICOL(CURRENT.FIELD), 1, 0, 31
                            PRINT STRING$(LEN(CT$(CURRENT.FIELD)), "*"); STRING$(SILEN(CURRENT.FIELD) - LEN(CT$(CURRENT.FIELD)), F$);
                            COLOR BGCLR, BGCLR
                         ELSE
                            LOCATE SIROW(CURRENT.FIELD), SICOL(CURRENT.FIELD), 1, 0, 31
                            PRINT CT$(CURRENT.FIELD); STRING$(SILEN(CURRENT.FIELD) - LEN(CT$(CURRENT.FIELD)), F$);
                         END IF
                      END IF
                   END IF
                ELSE
                   IF LEN(CT$(CURRENT.FIELD)) > 0 THEN
                      CT$(CURRENT.FIELD) = RIGHT$(CT$(CURRENT.FIELD), LEN(CT$(CURRENT.FIELD)) - 1)
                   END IF
                   CT$(CURRENT.FIELD) = W$ + CT$(CURRENT.FIELD)
                   LOCATE SIROW(CURRENT.FIELD), SICOL(CURRENT.FIELD), 1, 0, 31
                   IF BGCLR = 0 THEN
                      COLOR 0, 7
                      PRINT STRING$(LEN(CT$(CURRENT.FIELD)), "*");
                      COLOR BGCLR, BGCLR
                   ELSE
                      PRINT CT$(CURRENT.FIELD);
                   END IF
                   L3 = L3 - 1
                   Y = Y + 1
                   LOCATE X, Y, 1, 0, 31
                   GOTO NEXT.POSITION
                END IF
             END IF

             IF INS.FLAG = 1 AND POZ > 1 THEN
                IF POZ < LEN(CT$(CURRENT.FIELD)) AND L3 > 0 THEN
                   L1 = POZ - 1
                   L2 = LEN(CT$(CURRENT.FIELD)) - L1 - 1
                   X1$ = LEFT$(CT$(CURRENT.FIELD), L1)
                   X2$ = RIGHT$(CT$(CURRENT.FIELD), L2)
                   L3 = L3 - 1
                   CT$(CURRENT.FIELD) = X1$ + W$ + X2$
                   LOCATE SIROW(CURRENT.FIELD), SICOL(CURRENT.FIELD), 0
                   IF BGCLR = 0 THEN
                      COLOR 0, 7
                      PRINT STRING$(LEN(CT$(CURRENT.FIELD)), "*");
                      COLOR BGCLR, BGCLR
                   ELSE
                      PRINT CT$(CURRENT.FIELD);
                   END IF
                   Y = Y + 1
                   LOCATE X, Y, 1, 0, 31
                   GOTO NEXT.POSITION
                END IF
             END IF

             IF INS.FLAG = 1 AND L3 <= 0 THEN
                IF POZ <= LEN(CT$(CURRENT.FIELD)) THEN
                   L1 = POZ - 1
                   L2 = LEN(CT$(CURRENT.FIELD)) - POZ
                   X1$ = LEFT$(CT$(CURRENT.FIELD), L1)
                   X2$ = RIGHT$(CT$(CURRENT.FIELD), L2)
                   CT$(CURRENT.FIELD) = X1$ + W$ + X2$
                   LOCATE SIROW(CURRENT.FIELD), SICOL(CURRENT.FIELD), 0
                   IF BGCLR = 0 THEN
                      COLOR 0, 7
                      PRINT STRING$(LEN(CT$(CURRENT.FIELD)), "*");
                      COLOR BGCLR, BGCLR
                   ELSE
                      PRINT CT$(CURRENT.FIELD);
                   END IF
                   Y = Y + 1
                   LOCATE X, Y, 1, 0, 31
                   GOTO NEXT.POSITION
                END IF
             END IF

             IF POZ <= LEN(CT$(CURRENT.FIELD)) AND BS.KEY = 1 THEN
                X$ = LEFT$(CT$(CURRENT.FIELD), POZ - 1)
                Y$ = RIGHT$(CT$(CURRENT.FIELD), LEN(CT$(CURRENT.FIELD)) - POZ)
                CT$(CURRENT.FIELD) = X$ + W$ + Y$
                LOCATE X, Y, 1, 0, 31
                IF BGCLR = 0 THEN
                   COLOR 0, 7
                   PRINT "*";
                   COLOR BGCLR, BGCLR
                ELSE
                   PRINT W$;
                END IF
                Y = Y + 1
                GOTO NEXT.POSITION
             END IF

             IF LEN(CT$(CURRENT.FIELD)) <> 0 THEN
                IF POZ = 1 THEN
                   CT$(CURRENT.FIELD) = W$ + RIGHT$(CT$(CURRENT.FIELD), LEN(CT$(CURRENT.FIELD)) - 1)
                ELSE
                   SELECT CASE LEN(CT$(CURRENT.FIELD))
                      CASE IS > POZ
                         X$ = LEFT$(CT$(CURRENT.FIELD), POZ - 1)
                         Y$ = RIGHT$(CT$(CURRENT.FIELD), LEN(CT$(CURRENT.FIELD)) - POZ)
                         CT$(CURRENT.FIELD) = X$ + W$ + Y$
                      CASE IS <> POZ
                         CT$(CURRENT.FIELD) = CT$(CURRENT.FIELD) + W$
                      CASE IS = POZ
                         Y$ = LEFT$(CT$(CURRENT.FIELD), LEN(CT$(CURRENT.FIELD)) - 1)
                         CT$(CURRENT.FIELD) = Y$ + W$
                   END SELECT
                END IF
             ELSE
                CT$(CURRENT.FIELD) = CT$(CURRENT.FIELD) + W$
             END IF

             LOCATE X, Y, 1, 0, 31
             IF BGCLR = 0 THEN
                COLOR 0, 7
                PRINT "*";
                COLOR BGCLR, BGCLR
             ELSE
                PRINT W$;
             END IF
             Y = Y + 1
             GOTO JUMP6

PROCESS.BACK.SPACE:
             XX = LEN(CT$(CURRENT.FIELD)) - 1
             IF XX >= 0 AND POZ = XX + 2 THEN
                CT$(CURRENT.FIELD) = LEFT$(CT$(CURRENT.FIELD), XX)
             ELSE
                XX$ = LEFT$(CT$(CURRENT.FIELD), POZ - 2)
                YY$ = RIGHT$(CT$(CURRENT.FIELD), LEN(CT$(CURRENT.FIELD)) - POZ + 1)
                I1 = COUNT.SPACES(YY$)
                CT$(CURRENT.FIELD) = XX$ + " " + YY$
                LOCATE SIROW(CURRENT.FIELD), SICOL(CURRENT.FIELD), 0
                IF BGCLR = 0 THEN
                   COLOR 0, 7
                   PRINT STRING$(LEN(XX$), "*"); F$; STRING$(I1, F$); STRING$(LEN(LTRIM$(YY$)), "*");
                   COLOR BGCLR, BGCLR
                ELSE
                   PRINT XX$; F$; STRING$(I1, F$); LTRIM$(YY$);
                END IF
                BS.KEY = 1
             END IF
             Y = Y - 1
             LOCATE X, Y, 1, 0, 31
             IF BGCLR = 0 THEN
                COLOR 0, 7
                PRINT F$; CHR$(29);
                COLOR BGCLR, BGCLR
             ELSE
                PRINT F$; CHR$(29);
             END IF
             POZ = POZ - 2

JUMP6:
             IF POZ < SILEN(CURRENT.FIELD) + 1 THEN
                GOTO NEXT.POSITION
             ELSE
                GOTO GET.CHARACTER
             END IF

NEXT.POSITION:
           NEXT POZ

JUMP7:
           IF CURRENT.FIELD = FEELDS THEN
              CURRENT.FIELD = 0
              GOTO NEXT.FIELD
           END IF

NEXT.FIELD:
           CT$(CURRENT.FIELD) = RTRIM$(CT$(CURRENT.FIELD))
        NEXT CURRENT.FIELD

DATA.ENTRY.EXIT:

        RETURN

ANALIZE.RIGHT.ARROW:

        CALL VALIDATE.INPUT (ALLOWED.KEYS$, W$, ERROR.CODE, BACK.TAB$, ASCII$, TYP(), ACCEPT$, HELP$, HOME$, END.KEY$, CURRENT.FIELD)

        IF ERROR.CODE = 1 THEN
           LOCATE 25, 1, 0
           PRINT "Invalid character for the field type ...";
           BEEP
           RETURN
        END IF

        IF POZ <= LEN(CT$(CURRENT.FIELD)) THEN
           X$ = LEFT$(CT$(CURRENT.FIELD), POZ - 1)
           Y$ = RIGHT$(CT$(CURRENT.FIELD), LEN(CT$(CURRENT.FIELD)) - POZ)
           CT$(CURRENT.FIELD) = X$ + W$ + Y$
           PRINT W$;
           RETURN
        END IF

        X$ = STRING$(POZ - (LEN(CT$(CURRENT.FIELD)) + 1), " ")
        CT$(CURRENT.FIELD) = CT$(CURRENT.FIELD) + X$ + W$
        PRINT W$;
        RP.KEY = 0
        RETURN

SUB.MENU:
      COLOR 0, 4
      EXIT.FLAG = 0
      FOR I8 = 1 TO MAX
         READ MENU.ITEM$(I8)
      NEXT I8
      FOR J = 1 TO MAX
         IF UCASE$(ITEM$) = RTRIM$(UCASE$(MENU.ITEM$(J))) THEN
            SLCT = J
            J = MAX + 1
         END IF
      NEXT J
      J8 = COL
      I8 = ROW
      FOR K = 1 TO MAX
         LOCATE I8, J8, 0: PRINT MENU.ITEM$(K);
         I8 = I8 + 1
      NEXT K
      X8 = ROW - 1
      Y8 = COL
      J8 = X8 + SLCT - 1
      GOSUB 1000
      RETURN

1000
      LOCATE J8 + 1, Y8, 0
      COLOR 15, 4
      PRINT UCASE$(MENU.ITEM$(SLCT));

1020
      W$ = INKEY$
      IF W$ = "" THEN 1020
      IF W$ = CHR$(13) THEN RETURN
      IF W$ = CHR$(27) THEN
         EXIT.FLAG = 1
         RETURN
      END IF
      IF W$ <> " " THEN 1020
      SLCT = SLCT + 1
      IF SLCT > MAX THEN
         J8 = X8
         SLCT = 1
         LOCATE J8 + MAX, Y8, 0
         COLOR 0, 4
         PRINT MENU.ITEM$(MAX);
         GOTO 1000
      END IF
      J8 = J8 + 1
      LOCATE J8, Y8, 0
      COLOR 0, 4
      PRINT MENU.ITEM$(SLCT - 1);
      LOCATE J8 + 1, Y8, 0
      COLOR 15, 4
      PRINT UCASE$(MENU.ITEM$(SLCT));
      GOTO 1020

CENTER.LINE:

      L = LEN(A1$)
      X = (74 - L) / 2
      LOCATE , X, 0
      PRINT A1$;
      RETURN

READ.AUTO.FILE:

      FILENUM = FREEFILE
      ERROR.CODE = 0
      OPEN "AUTOEXEC.CS" FOR INPUT AS FILENUM
      CALL AUTO.FILE(FILENUM, ERROR.CODE, ID$, PASSWORD$, LINE.COMMANDS$())
      RETURN

GENERAL.ERROR:

      ERROR.CODE = ERR
      RESUME NEXT

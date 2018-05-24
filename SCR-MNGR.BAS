DECLARE SUB DISPLAY.ERROR.MESSAGE (ERROR.CODE, ERR.ROUTINE$)
DECLARE SUB VALIDATE.INPUT (ALLOWED.KEYS$, W$, ERROR.CODE!, BACK.TAB$, ASCII$, TYP!(), ACCEPT$, HELP$, HOME$, END.KEY$, CURRENT.FIELD!)
DECLARE SUB INITIALIZE.SCREEN ()
DECLARE SUB SPECIAL.KEYS.BUFFER (AL$, ALL.FUNC.KEYS$)
DECLARE SUB MEMORY.CHECK ()
DECLARE SUB SET.COLORS (ATT$, BGCLR!, FGCLR!, UNPRO!, BLINK!, DIMC!)
DECLARE SUB ANALIZE.PIC (PIC$)
DECLARE SUB INITIALIZE.MESSAGE.OUT (MAX.FIELDS!, MILEN!(), MIPIC$(), MSI$, MIOFF!())
DECLARE SUB DECIMAL.TO.BINARY (X%, B$)
DECLARE SUB VALIDATE.FIELDS.PIC (FEELDS!, CT$(), TYP!(), ERROR.CODE!, CURRENT.FIELD!, SIPIC$())
DECLARE SUB DOS (C.SCREEN!)
DECLARE SUB CONFIG (W$, C.SCREEN!, COLORS$, COMMS$, DISK$, FKEYS$, SFKEYS$, CFKEYS$, AFKEYS$, AUTO$, PASSWORD$, CALC.CONFIG$, BGCLR!)
DECLARE SUB DISPLAY.FUNCTION.KEYS (C.SCREEN!)
DECLARE SUB SEND.SCREEN.ROUTINE (LINES$())
DECLARE SUB DISPLAY.WINDOW (NX!, NY!, C.SCREEN!)
DECLARE SUB HELP.DISPLAY (H1$, H2$, H3$, H4$, H5$)
DECLARE SUB PROCESS.PGUP.PGDN.KEY (H.SCREEN!, W$, C.SCREEN!)
DECLARE SUB PROCESS.RIGHT.ARROW (X!, Y!, R.KEY!)
DECLARE SUB PROCESS.LEFT.ARROW (CURRENT.FIELD!, POZ!, SILEN!(), SIROW!(), SICOL!(), L.KEY!, RP.KEY!, Y!, X!)
DECLARE SUB PROCESS.UP.ARROW (SIROW!(), SICOL!(), CURRENT.FIELD!, FEELDS!)
DECLARE SUB PROCESS.DOWN.ARROW (SIROW!(), SICOL!(), CURRENT.FIELD!, FEELDS!)
DECLARE SUB PROCESS.INSERT.KEY (POZ!, CT$(), CURRENT.FIELD!, SIROW!(), SICOL!(), SILEN!(), X!, Y!, L3!)
DECLARE SUB PROCESS.DELETE.KEY (POZ!, CT$(), CURRENT.FIELD!, SIROW!(), SICOL!(), SILEN!(), F$, X!, Y!)
DECLARE SUB CALC (OUT.BUFFER$, C.SCREEN, BGCLR, SWidth%, VideoAddr%, PrL%, PrR%, PrLr%, PrM%, PrLM%, PrMR%, PrAll%, PrNone%, ButtonLeft%, ButtonRight%, ButtonMiddle%, True%, False%, COLORED)
DECLARE SUB GET.TRIGGER.MESSAGE (RAM.DRIVE$, TRIGGER.MESSAGE$, FMT.INDEX!, TRIGGER.CODE$)
DECLARE SUB DECOMPRESS.FORMAT.FILE (RAM.DRIVE$, FMT$, TRIGGER.FILE.OPEN!, HELP.FILE.OPEN!, HELP.FILE.NUMBER$, TRIGGER.FILE.NUMBER$, FMT.COUNT!, MAX.FIELDS!, CLIENT.VER$)
DECLARE SUB CREATE.HELP.FILE (RAM.DRIVE$, FMT.COUNT!, HELP.FILE.NUMBER$, HELP.FILE.OPEN!, FMT.DESC$, LAST.FIELD.NUM!, R$)
DECLARE SUB CREATE.TRIGGER.FILE (RAM.DRIVE$, FMT.COUNT!, TRIGGER.FILE.NUMBER$, TRIGGER.FILE.OPEN!, FMT.DESC$, R$)
DECLARE SUB MOVE.TO.CT (K1!, FEELDS!, MAX.FIELDS!, SISFN!(), PRINT.FLAG!, SOSFN!(), SIROW!(), SOROW!(), SICOL!(), SOCOL!(), PIC.CODE!, TXT$, SOLEN!(), CT$(), TYP!(), SOX!)
DECLARE SUB COPY.TO.RAM (ERROR.CODE!, FMT.COUNT!, DIR.RECORD$, EXT.DRIVE$, RAM.DRIVE$, FORMATS$(), IN.ARRAY%(), OUT.ARRAY%(), ax!, bx!, cx!, dx!, MIN.SPACE#, BGCLR, DISPLAY.MODE$, C.SCREEN)
DECLARE SUB COPY.TO.EXT (EXT.DRIVE$, R.FREQ!, FMT.START, FMT.COUNT!, FORMATS$(), RAM.DRIVE$, ERROR.CODE!, IN.ARRAY%(), OUT.ARRAY%(), ax!, bx!, cx!, dx!, EXT.MIN.SP#, BGCLR, DISPLAY.MODE$, C.SCREEN)
DECLARE SUB VIDEO.CHECK (C.SCREEN, IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, COLORED)
DECLARE SUB DISK.SPACE (IN.ARRAY%(), OUT.ARRAY%(), ax!, bx!, cx!, dx!, AVAILABLE.SPACE#, DRIVE$)
DECLARE FUNCTION COUNT.SPACES! (YY$)
DECLARE FUNCTION HEX.TO.DECIMAL! (HX$)
DECLARE FUNCTION HEX.TO.CHAR$ (HX$)
'
'*=============================================================================*
'| Program Id. .......... SCR-MNGR.BAS                                         |
'| Author ............... Muawiya Abujubain COVENTRY GROUP LTD.                |
'|                                                                             |
'| Function ............. This program handles the communications between      |
'|                        the client and the server. It also acts as a screen  |
'|                        manager that accept input from the user and displays |
'|                        the output that is received from the server.         |
'|                        Another function the program performs, is the version|
'|                        control of the screen formats between the client and |
'|                        the server.                                          |
'*=============================================================================*
'
'*================*
'* IMPORTANT NOTE *
'*=================
'
' The above declarations are the minimum amount of declarations that have to
' exist in the program to be compiled.
'
'===========================================================================

	REM $INCLUDE: 'QB.BI'
	REM $INCLUDE: 'MOUSE.BI'

	ON ERROR GOTO ERROR.ROUTINE
       
	GOSUB INITIALIZE

	GOSUB MAINLINE
	GOTO PROGRAM.END

INITIALIZE:

	CALL INITIALIZE.SCREEN

	CONST MAX.FIELDS = 200
	CONST MAX.TRIGS.AND.DUMMY = 220

	REM $DYNAMIC
	DIM SCROW(MAX.FIELDS)              'screen constant row
	DIM SCCOL(MAX.FIELDS)              'screen constant column
	DIM SCTXT$(MAX.FIELDS)             'screen constant text
	DIM SCATT$(MAX.FIELDS)             'screen constant attributes
	DIM SIROW(MAX.FIELDS)              'screen input row
	DIM SICOL(MAX.FIELDS)              'screen input column
	DIM SILEN(MAX.FIELDS)              'screen input length
	DIM SIPIC$(MAX.FIELDS)             'screen input picture
	DIM SISFN(MAX.FIELDS)              'field number in msi
	DIM SIATT$(MAX.FIELDS)             'screen input attributes
	DIM TYP(MAX.FIELDS)                'field type 1=numeric 2=alpha
					   '           3=upper case
	DIM SOROW(MAX.FIELDS)              'screen output row
	DIM SOCOL(MAX.FIELDS)              'screen output column
	DIM SOLEN(MAX.FIELDS)              'screen output length
	DIM SOPIC$(MAX.FIELDS)             'screen output picture
	DIM SOSFN(MAX.FIELDS)              'field number in mso
	DIM SOATT$(MAX.FIELDS)             'screen output attributes
	DIM MIOFF(MAX.FIELDS)              'message input offset
	DIM MILEN(MAX.FIELDS)              'message input length
	DIM MIPIC$(MAX.FIELDS)             'message input picture
	'
	DIM MOOFF(MAX.TRIGS.AND.DUMMY)     'message output offset
	DIM MOLEN(MAX.TRIGS.AND.DUMMY)     'message output length
	DIM CT$(MAX.FIELDS)                'input array ... must = max number
					   'of fields

	DIM FORMATS$(400)  'available formats array. Each element of
			   'the array is made of 10 bytes. These
			   'bytes are described as follows :-
			   '  FORMAT NAME          PIC X(6)
			   '  VERSION NUMBER       PIC 9(2)
			   '  FORMAT-USED-COUNTER  PIC 9(2)
			   '  COPIED-FLAG          PIC X(1) 'Y' OR 'N'

	DIM IN.ARRAY%(9)   'define input and output
	DIM OUT.ARRAY%(9)  'arrays for int86.
	DIM LINES$(24)     'screen image array.

	DIM R.SILEN(MAX.FIELDS)   'screen input length - RECALL INPUT FUNCTION
	DIM R.SIPIC$(MAX.FIELDS)  'screen input picture - RECALL INPUT FUNCTION
	DIM R.CT$(MAX.FIELDS)     'input array - RECALL INPUT FUNCTION
	DIM R.TYP(MAX.FIELDS)     'field type  - RECALL INPUT FUNCTION

	True% = -1
	False% = 0
	CONST SWidth% = 80

	'Mouse button press definitions

	CONST PrL% = 1
	CONST PrR% = 2
	CONST PrLr% = 3
	CONST PrM% = 4
	CONST PrLM% = 5
	CONST PrMR% = 6
	CONST PrAll% = 7
	CONST PrNone% = 0

	'Button definitions
  
	CONST ButtonLeft% = 0
	CONST ButtonRight% = 1
	CONST ButtonMiddle% = 2

	'
	' The following constants are defined to be used as pointers
	' to the CPU registers. This is needed for the
	' DATA.ENTRY.ROUTINE which handles the keyboard.
	'
	CONST ax = 0, bx = 1, cx = 2, dx = 3, bp = 4, si = 5, di = 6
	CONST FL = 7, ds = 8, es = 9
	CALL VIDEO.CHECK(C.SCREEN, IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, COLORED)
	FLGS$ = SPACE$(15)
	'
	' COLOR codes constatns
	'
	CONST BLACK = 0, BLUE = 1, GREEN = 2, CYAN = 3
	CONST RED = 4, MAGENTA = 5, BROWN = 6, WHITE = 7
	CONST H.BLACK = 8, H.BLUE = 9, H.GREEN = 10, H.CYAN = 11
	CONST H.RED = 12, H.MAGENTA = 13, H.BROWN = 14, H.WHITE = 15
	'
	' This constant F$ is used to fill the input fields with
	' the stored character.
	'
	CONST F$ = "_"
	'
	' The following constants are conditions that are used to
	' control the logic flow in routine LOADFORMAT. The variable
	' FORMAT.LOAD.CONTROL is set to one of these conditions before
	' executing the LOADFORMAT routine.
	'
	CONST COMPLETE.PROCESS = 0
	CONST CHECK.FORMAT = 1
	CONST FIRST.PART.LOADED = 2
	CONST COMM.PARAM$ = ",RB4096,TB4096,OP10"
	'
	' The following variables define the combination keys to perform
	' certain functions.
	'
	'================
	'*IMPORTANT NOTE*
	'================
	'
	' The system generates two characters when a function key or a
	' special key is pressed. The first character is the NULL character
	' (ASCII 0) and the second character is the one checked for. But I
	' programmed the DATA.ENTRY.ROUTINE to replace the first character
	' of a special key with the ESCAPE character (ASCII 27) because
	' it is much easier to deal with a normal character than the NULL
	' character.
	'
	' For the above reason the reader will find that the
	' definition of all the keys that start with ALT and the
	' function keys, have the ESCAPE character as the first
	' character.
	'
	ACCEPT$ = CHR$(13)                 ' RETURN or ENTER key
	HELP$ = CHR$(27) + CHR$(35)        ' ALT + H
					   ' to display the help window.

	SEND.SCREEN$ = CHR$(27) + CHR$(31) ' ALT + S
					   ' to activate the send screen
					   ' function.

	INIT.FIELDS$ = CHR$(27) + CHR$(23) ' ALT + I
					   ' to initialize input fields on
					   ' the screen.

	RECALL.INPUT$ = CHR$(27) + CHR$(19)' ALT + R
					   ' to restore the last input.

	EXIT.TO.DOS$ = CHR$(27) + CHR$(32) ' ALT + D
					   ' provides a shell to dos.

	SHOW.KEYS$ = CHR$(27) + CHR$(33)   ' ALT + F
					   ' to display the function keys
					   ' assignments

	CALCULATOR$ = CHR$(27) + CHR$(46)  ' ALT + C
					   ' to start the calculator.

	CALCULATOR.T$ = CHR$(27) + CHR$(20)' ALT + T
					   ' to activate the calculator and
					   ' transfer the value of the current
					   ' to it.

	COLORS$ = CHR$(27) + CHR$(120)     ' ALT + 1
					   ' starts the colors configuration
					   ' module.

	COMMS$ = CHR$(27) + CHR$(121)      ' ALT + 2
					   ' starts the communications
					   ' configuration module.

	DISK$ = CHR$(27) + CHR$(122)       ' ALT + 3
					   ' starts the disk
					   ' configuration module.

	FKEYS$ = CHR$(27) + CHR$(123)      ' ALT + 4
					   ' starts the function keys
					   ' configuration module.

	SFKEYS$ = CHR$(27) + CHR$(124)     ' ALT + 5
					   ' starts the shift function keys
					   ' configuration module.

	CFKEYS$ = CHR$(27) + CHR$(125)     ' ALT + 6
					   ' starts the control function keys
					   ' configuration module.

	AFKEYS$ = CHR$(27) + CHR$(126)     ' ALT + 7
					   ' starts the alternate function keys
					   ' configuration module.

	AUTO$ = CHR$(27) + CHR$(127)       ' ALT + 8
					   ' starts the auto start sequence
					   ' configuration module.

	CALC.CONFIG$ = CHR$(27) + CHR$(128)' ALT + 9
					   ' starts the calculator tax setup
					   ' module.

	PASSWORD$ = CHR$(27) + CHR$(25)    ' ALT + P
					   ' starts the password
					   ' configuration module.

	HOME$ = CHR$(27) + CHR$(71)        ' HOME
	END.KEY$ = CHR$(27) + CHR$(79)     ' END
	TAB$ = CHR$(9)
	BACK.TAB$ = CHR$(27) + CHR$(15)
	ASCII$ = CHR$(13)
	FOR I = 32 TO 126
	   ASCII$ = ASCII$ + CHR$(I)
	NEXT I
	'
	' All the of above defined keys are grouped in a buffer called AL$.
	' This buffer is used when testing for a pressed key. If the pressed
	' key is not of the type of the current field and is not found in the
	' AL$ buffer, an error message is displayed for the user indicating
	' that he/she had pressed an invalid key.
	'
	AL$ = ACCEPT$ + HELP$ + HOME$ + END.KEY$ + TAB$ + CHR$(8)
	AL$ = AL$ + BACK.TAB$ + CHR$(27) + EXIT.TO.DOS$ + SHOW.KEYS$
	AL$ = AL$ + INIT.FIELDS$ + SEND.SCREEN$ + RECALL.INPUT$ + CALCULATOR.T$
	AL$ = AL$ + COLORS$ + COMMS$ + DISK$ + FKEYS$ + SFKEYS$ + CALC.CONFIG$
	AL$ = AL$ + CFKEYS$ + AFKEYS$ + AUTO$ + PASSWORD$ + CALCULATOR$
	'
	ALL.FUNC.KEYS$ = ""       ' ALL.FUNC.KEYS$ holds the second character
				  ' of all the function keys. This is done for
				  ' validation purposes in VALIDATE.INPUT
				  ' routine.
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
	F12$ = CHR$(27) + "†"
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
	SF11$ = CHR$(27) + "‡"
	SF12$ = CHR$(27) + "ˆ"
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
	CF11$ = CHR$(27) + "‰"
	CF12$ = CHR$(27) + "Š"
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
	AF11$ = CHR$(27) + "‹"
	AF12$ = CHR$(27) + "Œ"
	'
	' Add the definition of all the function keys to the AL$ and to
	' ALL.FUNC.KEYS$ buffer by calling SPECIAL.KEYS.BUFFER subroutine.
	'
	CALL SPECIAL.KEYS.BUFFER(AL$, ALL.FUNC.KEYS$)
	'
	' The following characters are the only ones to be accepted from
	' keyboard.
	'          15  107 71  79  50  35  138 23  31 19 32  33
	'          120 121 122 123 124 125 126 127 25 46 128 20
	'
	' These characters are stored in ALLOWED.KEYS$.
	'
	ALLOWED.KEYS$ = "GO2#Š !xyz{|}~.€" + ALL.FUNC.KEYS$
	'
	' H.SCREEN and M.SCREEN are counters used for the SCREEN HISTORY
	' facility.
	'
	H.SCREEN = -1
	M.SCREEN = -1
	'
	' Initialize some variables.
	'
	CURRENT.FMT$ = "NO-FMT"   ' This variable hold the current
				  ' screen format name. Its maximum
				  ' length is 6 bytes long.
	'
	SEND.SCREEN.FLAG = 0      ' This flag is turned on when the
				  ' user presses the key assigned to
				  ' the SEND SCREEN function. The system
				  ' will then scan the screen and sends
				  ' as a message to the SERVER.
	'
	FMT.COUNT = 0             ' This variable is a counter for the
				  ' available formats.
	'
	FORMAT.LOAD.CONTROL = 0   ' This variable is used to control
				  ' the flow of the logic in routine
				  ' LOADFORMAT.
	'
	TRIGGER.FILE.OPEN = 0     ' This is a flag to indecate whether
				  ' a trigger file has been opened or
				  ' not. 0 means the file is currently
				  ' and 1 means the file is currently
				  ' open.
	'
	HELP.FILE.OPEN = 0        ' This is a flag to indecate whether
				  ' a help file has been opened or
				  ' not. 0 means the file is currently
				  ' and 1 means the file is currently
				  ' open.
	'
	COLORS.FLAG = 0
	'
	I.CU = 0                  ' This variable holds the starting
				  ' cursor field when a screen is first
				  ' displayed 'SET NEW-CURSOR'.

INIT1000:

	'
	' Load the configuration file.
	'
	GOSUB LOADCONFIG
	'
	' Delete whatever is in the RAM drive by creating a dummy file there,
	' then issuing the KILL command. The reason I do that is, if there
	' was nothing is the RAM drive the system will display an error
	' message if I issue the KILL command, therefore I create a dummy file
	' then I delete everything.
	'
	OPEN RAM.DRIVE$ + "DUMMY" FOR OUTPUT AS #8
	CLOSE #8
	KILL RAM.DRIVE$ + "*.*"
	'
	' Check whether the external drive is enabled or not by
	' looking at the EXT.ENABLE$ flag which is loaded from the
	' configuration file. If the flag contains "Y" then the external
	' drive is enabled. Therefore I open the file which should
	' contain the directory information of the external drive,
	' SCR-MNGR.DIR. The opening of the file is done in two
	' stages :-
	'
	'  1. Open the file for RANDOM and check its size by using the
	'     LOF() function. If the LOF() function returns a 0, that
	'     means that the file did not exist. Therefore the file is
	'     closed and the buffer to hold the directory information
	'     DIR.RECORD$ is initialized to NULL.
	'
	'  2. If the LOF() function from stage (1) above returns a
	'     value greater than 0, this means that the file exists and
	'     contains data. The file is then closed and reopened for
	'     input. This file should contain only one record. The
	'     record is made of one string that is a repetition of the
	'     following pattern :-
	'
	'                 FORMAT-NAME             PIC X(6)
	'                 VERSION-NUMBER          PIC 9(2)
	'                 POSITION-IN-TABLE       PIC 9(3)
	'
	'     Following is an example of the contents of DIR.RECORD$
	'     buffer :-
	'
	'           MSM02505007MSC25N10008MSC20N02009MSC21N02010
	'           ^---------^^---------^^---------^^---------^
	'
	'     The above example is explained as follows :-
	'
	'          FORMAT-NAME   VERSION-NUMBER    POSITION-IN-TABLE
	'          ===========   ==============    =================
	'            MSM025           05                 007
	'            MSC25N           10                 008
	'            MSC20N           02                 009
	'            MSC21N           02                 010
	'
	'     POSITION-IN-TABLE is the position of the format's
	'     information in the array FORMATS$(). This number is used
	'     to place the information of the format back to its original
	'     position in the array FORMATS$() at the start of the program.
	'
	'     This is neccessary because each format file can have a
	'     TRIGGERS file and a HELP file associated with it. The naming
	'     convention of the TRIGGERS files and the HELP files is as
	'     follows :-
	'                  TRIG-???.DAT  & HELP-???.DAT
	'
	'     where ??? is the POSITION-IN-TABLE number for the
	'     particular format.
	'
	'     Example :-
	'     ==========
	'
	'        From the above example of DIR.RECORD$ contents the
	'        reader will find that the value of POSITION-IN-TABLE for
	'        format MSC20N is 9. Therefore the name of the TRIGGERS file
	'        for that format, if there was one, is TRIG-9.DAT and the name
	'        of the HELP file, if there was one, is HELP-9.DAT.
	'
	'     The reason for such method of storing the information of
	'     the formats, is to accomplish a fast execution time
	'     during the loading of the stored formats from the external
	'     drive to the RAM drive by performing only one read for the
	'     SCR-MNGR.DIR file.
	'
	' Routine COPY.TO.RAM is called to copy the files from the external
	' drive to the RAM drive. Please refer to the source file
	' SYS-LIB.BAS for documentation for the routine.
	'
	IF EXT.ENABLE$ = "Y" THEN
	   DIR.FILE = FREEFILE
	   OPEN EXT.DRIVE$ + "SCR-MNGR.DIR" FOR RANDOM AS DIR.FILE
	   IF LOF(DIR.FILE) = 0 THEN
	      CLOSE #DIR.FILE
	      DIR.RECORD$ = ""
	   ELSE
	      CLOSE #DIR.FILE
	      OPEN EXT.DRIVE$ + "SCR-MNGR.DIR" FOR INPUT AS #DIR.FILE
	      LINE INPUT #DIR.FILE, DIR.RECORD$
	      CLOSE #DIR.FILE
	      CALL COPY.TO.RAM(ERROR.CODE, FMT.COUNT, DIR.RECORD$, EXT.DRIVE$, RAM.DRIVE$, FORMATS$(), IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, MIN.SPACE#, BGCLR, DISPLAY.MODE$, 0)
	      DIR.RECORD$ = ""
	   END IF
	END IF

INIT2000:
	'
	' Initialize some variables.
	'
	VLI$ = "0020"              ' This variable contains the length
				   ' of the message to be sent to the SERVER.
	'
	FMT$ = "CLIENT"            ' This variable acts a temporary
				   ' buffer to hold the format name of
				   ' the format received from the
				   ' server.
	'
	CLIENT.VER$ = "00"         ' Store the client version number
				   ' of the current format.
	'
	SYS$ = "PC"                ' Store the name of the SERVER
				   ' system such as MT, UB, UF, etc...
				   ' This variable is updated by the
				   ' SERVER in routine SETINPUT.
	'
	NEXT.FUNCTION$ = SPACE$(6) ' This variable is used to tell either
				   ' the CLIENT or the SERVER the next
				   ' function to perform. If the user
				   ' keys in data in the NEXT FUNCTION
				   ' field on the screen, this variable
				   ' will contain that data. In some
				   ' cases, this variable is used to transfer
				   ' system functions from the CLIENT to the
				   ' SERVER and vice versa. A system
				   ' function always starts with the
				   ' character "~". Following is a table
				   ' of the avialable system functions
				   ' and the meaning or action of each one of
				   ' them. Please keep in mind that
				   ' this variable can only be 6 bytes
				   ' long.
	'
	'*================================================================*
	'|                    System Functions Table                      |
	'|----------------------------------------------------------------|
	'|Function|  Meaning  | From | To   |      C o m m e n t s        |
	'|--------|-----------|------|------|-----------------------------|
	'|~RSTRT  |Restart    |CLIENT|SERVER|When a CLIENT first starts,  |
	'|        |           |      |      |it sends this system message |
	'|        |           |      |      |to whatever is on the other  |
	'|        |           |      |      |end. If the SERVER was       |
	'|        |           |      |      |connected and the application|
	'|        |           |      |      |was running, the IDS         |
	'|        |           |      |      |requester will respond to    |
	'|        |           |      |      |this message by sending the  |
	'|        |           |      |      |data of the current screen   |
	'|        |           |      |      |and the system message       |
	'|        |           |      |      |~NOOTO.                      |
	'|--------|-----------|------|------|-----------------------------|
	'|~NOOTO  |Cancel the |SERVER|CLIENT|If the CLIENT starts while   |
	'|        |auto start |      |      |the application is in RUN    |
	'|        |routine.   |      |      |state, the CLIENT will send a|
	'|        |           |      |      |~RSTRT message to the SERVER |
	'|        |           |      |      |which will instruct the      |
	'|        |           |      |      |SERVER to send the data of   |
	'|        |           |      |      |the current screen to the    |
	'|        |           |      |      |CLIENT. Therefore, we do not |
	'|        |           |      |      |want the CLIENT to go through|
	'|        |           |      |      |its auto start sequence. So  |
	'|        |           |      |      |this message is sent by the  |
	'|        |           |      |      |SERVER to disable the auto   |
	'|        |           |      |      |start sequence.              |
	'|--------|-----------|------|------|-----------------------------|
	'|~ERROR  |Display an |SERVER|CLIENT|If a system error or any type|
	'|        |error mess-|      |      |of error on the SERVER occurs|
	'|        |-age.      |      |      |, the SERVER will send this  |
	'|        |           |      |      |system function along with   |
	'|        |           |      |      |the error message text to the|
	'|        |           |      |      |CLIENT. The CLIENT displays  |
	'|        |           |      |      |the error message received   |
	'|        |           |      |      |at line 25.                  |
	'|--------|-----------|------|------|-----------------------------|
	'|~RSCRN  |Receive a  |CLIENT|SERVER|When the user activates the  |
	'|        |screen     |      |      |send screen function by      |
	'|        |image.     |      |      |pressing ALT+S (SEND.SCREEN$)|
	'|        |           |      |      |or whatever function key     |
	'|        |           |      |      |he/she had assigned for this |
	'|        |           |      |      |function, the CLIENT captures|
	'|        |           |      |      |a screen image of the current|
	'|        |           |      |      |displayed screen and forms a |
	'|        |           |      |      |message with this system     |
	'|        |           |      |      |function. When the SERVER    |
	'|        |           |      |      |receives this message, it    |
	'|        |           |      |      |saves the screen image in a  |
	'|        |           |      |      |file for later use.          |
	'|--------|-----------|------|------|-----------------------------|
	'|~PRINT  |Print the  |SERVER|CLIENT|This function is activated   |
	'|        |SERVER'S   |      |      |when the SERVER system wants |
	'|        |message.   |      |      |to print a report on the     |
	'|        |           |      |      |printer attached to the      |
	'|        |           |      |      |CLIENT. When this function is|
	'|        |           |      |      |received, the CLIENT program |
	'|        |           |      |      |looks for the PRN.ENABLE$    |
	'|        |           |      |      |and the PRN.DISABLE$ bytes   |
	'|        |           |      |      |and prints whatever is in    |
	'|        |           |      |      |between.                     |
	'*----------------------------------------------------------------*
	'
	TXO$ = ""        ' This variable is contains the length of the
			 ' message to be sent to the SERVER and the
			 ' message itself. The length of the message is
			 ' 4 bytes long. This variable is used only in
			 ' routine SETOUTPUT.
	'
	START.MESSAGE$ = "0020" + "CLIENT" + "00" + "PC" + "~RSTRT"
			 '
			 ' The START.MESSAGE$ variable is used to send
			 ' to the SERVER the restart message explained
			 ' above. After the restart message is sent,
			 ' this variable gets initialized to 1 space.
			 ' This is because we do not want the restart
			 ' message to be sent again.
	'
	AUTO.START.FLAG = 0      ' This flag is set to 1 when the
				 ' system detects that an
				 ' AUTOEXEC.CS file exists. When this
				 ' falg is set to 1 the system goes
				 ' into the execution of the commands
				 ' found in the AUTOEXEC.CS file.
	'
	FUNC.KEY.FLAG = 0        ' This flag is set to 1 when a
				 ' function key is pressed. And it is
				 ' reset to 0 when the execution of
				 ' the commands assigned to a function
				 ' key stops.
	'
	GOSUB AUTOEXEC.ROUTINE   ' Find out about the AUTOEXEC.CS
				 ' file.
	'
	SCREEN , , 0, 0          ' Set the current screen page to page 0.
	'
	LOCATE 25, 1, 0
	COLOR 15, 0
	PRINT "Press any key to connect to the SERVER or ESC to exit ...";
	COLOR FGCLR, BGCLR
	GOSUB START.COMM
	RETURN

INIT3000:
	'
	' Initialize the COM port by closing it and reopening it.
	'
	CLOSE #1
	OPEN PORT$ FOR RANDOM AS #1 LEN = 4096
	COM(COMM) STOP
	ON COM(COMM) GOSUB GETSERVER
	COM(COMM) STOP
	RETURN

START.COMM:
	'
	' This routine opens the COM port and send the restart message
	' as the first message. It then initializes the restart
	' message START.MESSAGE$ to 1 space. This is done so the
	' SERVER does not go through the restart algorithm when
	' receiving the restart message for the second time.
	'
	ERR.ROUTINE$ = "START.COMM"
	CLOSE #1
	OPEN PORT$ FOR RANDOM AS #1 LEN = 4096

SEND.CHAR:

	ERR.ROUTINE$ = "SEND.CHAR"
	W$ = INKEY$: IF W$ = "" GOTO SEND.CHAR
	'
	' Initialize all the avialable screen pages and blank them
	' when the user decides to exit the program.
	'
	IF W$ = CHR$(27) THEN
	   FOR I = 0 TO C.SCREEN + 1
	      SCREEN , , I, I
	      CLS
	   NEXT I
	   GOTO PROGRAM.END
	END IF
	'
	' Send the restart message.
	'
	PRINT #1, START.MESSAGE$

	RETURN

MAINLINE:
	'
	' This is the main loop. It is divided into 5 major routines as
	' follows :-
	'
	'      1. SETINPUT
	'      2. LOADFORMAT
	'      3. SCREENOUTPUT
	'      4. DATA.ENTRY.ROUTINE
	'      5. SETOUTPUT
	'
	' The above mentioned routines are the main routines that get
	' executed in a loop which is the MAINLINE loop. Following is
	' a brief explanation for each of these routines. The reader
	' can find detailed documentation for each step of the above
	' routines in the routine itself.
	'
	' 1. SETINPUT :- This routine is responsible for receiving
	'    only the data from the SERVER. The data is received as a
	'    message. The message is then analized into the
	'    appropriate memory variables.
	'
	' 2. LOADFORMAT :- This routine executes other routines to
	'    load the formats from the SERVER and decompress them if
	'    they were comppressed. It also updates the FORMATS$ array
	'    in memory. In addition to that it creates the
	'    HELP-???.DAT & TRIG-???.DAT files mentioned above.
	'    LOADFORMAT also displays the base screen of the format
	'    received.
	'
	' 3. SCREENOUTPUT :- The purpose of this routine is to analize
	'    the recieved data and format it according to its PICTURE
	'    description and display it on the screen.
	'
	' 4. DATA.ENTRY.ROUTINE :- This is the routine that handles
	'    the keyboard. It draws the input fields on the screen and
	'    allow the user to enter his/her data. The routine also
	'    does minor validation on the entered data. The validation
	'    is done according to the type of the field.
	'
	'    The reader will notice that routine INIT3000 is called
	'    regularly, this is because it initializes the COM port
	'    after each send and receive. The reason for that is the
	'    COM will have unwanted bytes after sending and receiving,
	'    and the only solution to this problem is to reinitialize
	'    the COM port.

	ERR.ROUTINE$ = "MAINLINE"

	GOT.TRIGGER = 0  ' This flag is set to 1 when a trigger code
			 ' is received from the SERVER.

	GOSUB INIT3000
	GOSUB SETINPUT

	' When the NEXT.FUNCTION$ is set to "~PRINT", we want to go
	' back to the beggining and execute routine SETINPUT, because
	' we should recieve a message from the server after the
	' printing is done.

	IF NEXT.FUNCTION$ = "~PRINT" THEN
	   GOTO MAINLINE
	END IF

	IF FMT$ = "FMTERR" THEN
	   BEEP
	   SCREEN , , 0, 0
	   CLS
	   PRINT "FORMAT ERROR ... CANNOT RECOVER"
	   GOTO PROGRAM.END
	END IF

	' When DISPLAY.MODE$ equals "B" this means that the user wants
	' the display mode to be BLOCK mode. The way to do this is we
	' make the active screen page differet than the displayed one.
	'
	' The following logic is done to accomplish the BLOCK mode
	' display:
	'
	'   1. Copy the current screen page to another screen page
	'      using the PCOPY statement.
	'
	'   2. Make the current screen page different than the active
	'      one, so all the writes to the screen will be done in the
	'      background.
	'
	'   3. When the writes to the screen are done, make the
	'      backgroud screen page the active one and that will give
	'      the user the BLOCK mode effect.

	IF DISPLAY.MODE$ = "B" THEN
	   PCOPY C.SCREEN, C.SCREEN + 1
	   SCREEN , , C.SCREEN + 1, C.SCREEN
	ELSE
	   SCREEN , , C.SCREEN, C.SCREEN
	END IF

	GOSUB INIT3000
	GOSUB LOADFORMAT
	FORMAT.LOAD.CONTROL = 0

	IF ERROR.CODE = 1 THEN
	   ERROR.CODE = 0
	   GOTO MAINLINE
	END IF

	IF EXT.ENABLE$ = "Y" THEN
	   CALL COPY.TO.EXT(EXT.DRIVE$, R.FREQ, 1, FMT.COUNT, FORMATS$(), RAM.DRIVE$, ERROR.CODE, IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, EXT.MIN.SP#, BGCLR, DISPLAY.MODE$, C.SCREEN)
	END IF
	'
	' Check if there are any trigger code to map the trigger
	' message into the output message.
	'
	FOR II = 211 TO MAX.TRIGS.AND.DUMMY
	   IF MOOFF(II) > 0 AND MOLEN(II) > 0 THEN
	      TRIGGER.CODE$ = MID$(MSO$, MOOFF(II), MOLEN(II))
	      IF VAL(TRIGGER.CODE$) > 0 THEN
		 GOT.TRIGGER = 1
		 CALL GET.TRIGGER.MESSAGE(RAM.DRIVE$, TRIGGER.MESSAGE$, FMT.INDEX, TRIGGER.CODE$)
		 IF LEN(MSO$) < MOOFF(II - 10) THEN
		    MSO$ = MSO$ + SPACE$(MOOFF(II - 10) - LEN(MSO$) + MOLEN(II - 10))
		 END IF
		 IF RTRIM$(TRIGGER.MESSAGE$) = "" THEN
		    TRIGGER.MESSAGE$ = MID$(MSO$, MOOFF(II - 10), MOLEN(II - 10))
		 END IF
		 IF LEN(TRIGGER.MESSAGE$) < MOLEN(II - 10) THEN
		    TRIGGER.MESSAGE$ = TRIGGER.MESSAGE$ + SPACE$(MOLEN(II - 10) - LEN(TRIGGER.MESSAGE$))
		 END IF
		 MID$(MSO$, MOOFF(II - 10), MOLEN(II - 10)) = TRIGGER.MESSAGE$
	      END IF
	   END IF
	NEXT II

	GOSUB SCREENOUTPUT
	
	IF DISPLAY.MODE$ = "B" THEN
	   PCOPY C.SCREEN + 1, C.SCREEN
	   SCREEN , , C.SCREEN, C.SCREEN
	END IF

	CALL MEMORY.CHECK

	AVAILABLE.SPACE# = 0
	CALL DISK.SPACE(IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, AVAILABLE.SPACE#, LEFT$(RAM.DRIVE$, 2))
	IF AVAILABLE.SPACE# <= MIN.SPACE# THEN
	   COLOR 15, BGCLR
	   LOCATE 25, 1, 0: PRINT "Deleting old formats ... Please wait!";
	   GOSUB DELETE.FORMAT
	   LOCATE 25, 1, 0: PRINT SPACE$(80);
	END IF

	GOSUB DATA.ENTRY.ROUTINE

	IF LEN(W1$) = 0 AND AUTO.START.FLAG = 0 AND COLORS.FLAG = 1 THEN
	   GOSUB RESET.COLORS
	END IF

	IF DISPLAY.MODE$ = "B" THEN
	   PCOPY C.SCREEN, C.SCREEN + 1
	   SCREEN , , C.SCREEN + 1, C.SCREEN
	ELSE
	   SCREEN , , C.SCREEN, C.SCREEN
	END IF

	GOSUB INIT3000
	GOSUB SETOUTPUT

	IF DISPLAY.MODE$ = "B" THEN
	   PCOPY C.SCREEN + 1, C.SCREEN
	   SCREEN , , C.SCREEN, C.SCREEN
	END IF

	H.SCREEN = H.SCREEN + 1
	IF H.SCREEN > C.SCREEN - 2 THEN
	   H.SCREEN = C.SCREEN - 2
	   M.SCREEN = M.SCREEN + 1
	   IF M.SCREEN > C.SCREEN - 2 THEN
	      M.SCREEN = 0
	   END IF
	   PCOPY C.SCREEN, M.SCREEN
	ELSE
	   PCOPY C.SCREEN, H.SCREEN
	END IF

	GOTO MAINLINE
	RETURN

SETINPUT:

	ERR.ROUTINE$ = "SETINPUT"
	COLOR 15, BGCLR
	LOCATE 25, 1, 0: PRINT SPACE$(80);
	LOCATE 25, 59, 0: PRINT "Receiving Data";
	TXI$ = ""

SETINP0500:

	GOSUB SET.SECONDS
	COM(COMM) ON

SETINP1000:

	IF LEN(TXI$) < 4 THEN
	   GOSUB CHECK.TIME
	   IF TIME.OUT.FLAG = 1 THEN
	      GOTO GET.MDX.MSG
	   END IF
	   GOTO SETINP1000
	END IF
	TXIL = VAL(LEFT$(TXI$, 4))
	IF TXIL = 0 AND LEFT$(TXI$, 4) <> "0000" THEN

GET.MDX.MSG:

	   IF NOT EOF(1) OR LOC(1) > 0 THEN
	      GOTO GET.MDX.MSG
	   END IF

	   SCREEN , , 0, 0
	   CLS
	   COLOR 15, 0
	   LOCATE 25, 1, 0
	   PRINT "The SERVER is disconnected ... ESC to exit ... Any key to try agian !!!";
	   SLEEP 3
	   GOSUB SEND.CHAR
	   GOTO MAINLINE
	END IF

SETINP2000:

	IF NOT EOF(1) OR LEN(TXI$) < TXIL THEN
	   GOTO SETINP2000
	END IF

	COM(COMM) STOP

SETINP2100:

	TXI$ = LEFT$(TXI$, TXIL)
	FMT$ = MID$(TXI$, 5, 6)
	SERVER.VER$ = MID$(TXI$, 11, 2)
	SYS$ = MID$(TXI$, 13, 2)
	NEXT.FUNCTION$ = MID$(TXI$, 15, 6)

	SELECT CASE RTRIM$(NEXT.FUNCTION$)
	   CASE "~ERROR"
	      NEXT.FUNCTION$ = SPACE$(6)
	      COLOR FGCLR, BGCLR
	      LOCATE 25, 1, 0: PRINT SPACE$(80);
	      LOCATE 25, 1, 0
	      PRINT RTRIM$(MID$(TXI$, 21, LEN(TXI$) - 20));
	      RETURN
	   CASE "~NOOTO"
	      AUTO.START.FLAG = 0
	      FUNC.KEY.FLAG = 0
	      GOSUB RESET.COLORS
	   CASE "~PRINT"
	      TXP$ = TXI$
	      PRN.START = INSTR(TXP$, PRN.ENABLE$)
	      PRN.END = INSTR(TXP$, PRN.DISABLE$)
	      PRN.LEN = PRN.END - PRN.START
	      IF PRN.LEN > 0 THEN
		 TXP$ = MID$(TXP$, PRN.START + LEN(PRN.ENABLE$), PRN.LEN)
		 OPEN PRN.PORT$ FOR OUTPUT AS #10
		 PRINT #10, TXP$;
		 CLOSE #10
		 TXP$ = ""
	      END IF
	      RETURN
	   CASE "MORE", "NOMORE"
	      MSO$ = ""
	      NEXT.FUNCTION$ = SPACE$(6)
	      FORMAT.LOAD.CONTROL = FIRST.PART.LOADED
	      RETURN
	   CASE "~LOGON"
	      MSO$ = ""
	      IF LEN(TXI$) > 20 THEN
		 MSO$ = MID$(TXI$, 21, LEN(TXI$) - 20)
	      END IF
	      NEXT.FUNCTION$ = SPACE$(6)
	      FORMAT.LOAD.CONTROL = CHECK.FORMAT
	      GOSUB LOADFORMAT
	      IF FORMAT.FOUND <> 0 THEN
		 SERVER.VER$ = CLIENT.VER$
	      END IF
	      IF FORMAT.FOUND = 0 THEN
		 FORMAT.LOAD.CONTROL = COMPLETE.PROCESS
		 RETURN
	      END IF
	      FORMAT.LOAD.CONTROL = FIRST.PART.LOADED
	      RETURN
	END SELECT

	MSO$ = ""
	IF LEN(TXI$) > 20 THEN
	   MSO$ = MID$(TXI$, 21, LEN(TXI$) - 20)
	END IF

	COLOR 15, BGCLR
	LOCATE 25, 59, 0: PRINT SPC(20);
	RETURN

GETSERVER:

	ERR.ROUTINE$ = "GETSERVER"

GET.SERVER.LOOP:

	IF LOC(1) <> 0 THEN
	   TXI$ = TXI$ + INPUT$(LOC(1), #1)
	   GOTO GET.SERVER.LOOP
	END IF
	RETURN

LOADFORMAT:

	ERR.ROUTINE$ = "LOADFORMAT"

	IF FORMAT.LOAD.CONTROL <> CHECK.FORMAT THEN
	   CLEAR.SCREEN.FLAG = 0
	   IF CURRENT.FMT$ = FMT$ AND CLIENT.VER$ = SERVER.VER$ THEN
	      IF AUTO.START.FLAG = 1 THEN
		 IF LEN(W1$) = 0 THEN
		    AUTO.START.FLAG = 0
		    W1$ = END.KEY$ + "LOGOFF" + ACCEPT$
		    FUNC.KEY.FLAG = 1
		 ELSE
		    COLOR 15, BGCLR
		    LOCATE 25, 1, 0
		    PRINT "Repeated format or INCORRECT PASSWORD !!!";
		    SLEEP 3
		 END IF
	      END IF
	      IF FORMAT.LOAD.CONTROL = COMPLETE.PROCESS THEN
		 PRINT #1, "0000"; "FMT-OK"; SERVER.VER$; SYS$
		 RETURN
	      END IF
	   END IF
	   ERASE SISFN, MIOFF, MILEN, MIPIC$, CT$, MOOFF, MOLEN, SIROW, SICOL
	   ERASE SOROW, SOCOL
	   REM $DYNAMIC
	   REDIM SISFN(MAX.FIELDS), MILEN(MAX.FIELDS), MIOFF(MAX.FIELDS)
	   REDIM MIPIC$(MAX.FIELDS), CT$(MAX.FIELDS)
	   REDIM MOOFF(MAX.TRIGS.AND.DUMMY), MOLEN(MAX.TRIGS.AND.DUMMY)
	   REDIM SIROW(MAX.FIELDS), SICOL(MAX.FIELDS)
	   REDIM SOROW(MAX.FIELDS), SOCOL(MAX.FIELDS)
	END IF

	IF FORMAT.LOAD.CONTROL = FIRST.PART.LOADED THEN
	   GOTO SECTION.1
	END IF

	FORMAT.FOUND = 0
	FOR K = 1 TO FMT.COUNT
	   IF FMT$ = LEFT$(FORMATS$(K), 6) THEN
	      FORMAT.FOUND = 1
	      FMT.INDEX = K
	      FORMAT.USED.COUNTER = VAL(MID$(FORMATS$(K), 9, 2)) + 1
	      FUC$ = LTRIM$(STR$(FORMAT.USED.COUNTER))
	      FUC$ = STRING$(2 - LEN(FUC$), "0") + FUC$
	      MID$(FORMATS$(K), 9, 2) = FUC$
	      CLIENT.VER$ = MID$(FORMATS$(K), 7, 2)
	   END IF
	NEXT K

	IF (FORMAT.LOAD.CONTROL = CHECK.FORMAT) AND (FORMAT.FOUND = 1) THEN
	   RETURN
	END IF

SECTION.1:

	IF FORMAT.FOUND = 0 THEN
	   IF FORMAT.LOAD.CONTROL <> FIRST.PART.LOADED THEN
	      IF EXT.ENABLE$ = "Y" THEN
		 OPEN EXT.DRIVE$ + "SCR-MNGR.DIR" FOR RANDOM AS #2
		 IF LOF(2) = 0 THEN
		    CLOSE #2
		    DIR.RECORD$ = ""
		 ELSE
		    CLOSE #2
		    OPEN EXT.DRIVE$ + "SCR-MNGR.DIR" FOR INPUT AS #2
		    LINE INPUT #2, DIR.RECORD$
		    CLOSE #2
		 END IF
		 X = INSTR(DIR.RECORD$, FMT$)
		 IF X > 0 THEN
		    OLD.FMT$ = MID$(DIR.RECORD$, X, 11)
		    IF LEFT$(OLD.FMT$, 8) = FMT$ + SERVER.VER$ THEN
		       FMT.INDEX = VAL(RIGHT$(OLD.FMT$, 3))
		       FORMATS$(FMT.INDEX) = LEFT$(OLD.FMT$, 8) + "01Y"
		       CALL COPY.TO.RAM(ERROR.CODE, W.DUMMY, OLD.FMT$, EXT.DRIVE$, RAM.DRIVE$, FORMATS$(), IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, MIN.SPACE#, BGCLR, DISPLAY.MODE$, C.SCREEN)
		       OPEN EXT.DRIVE$ + "SCR-MNGR.DIR" FOR OUTPUT AS #2
		       PRINT #2, DIR.RECORD$;
		       CLOSE #2
		       CLIENT.VER$ = SERVER.VER$
		       IF FORMAT.LOAD.CONTROL = CHECK.FORMAT THEN
			  FORMAT.FOUND = 1
			  RETURN
		       END IF
		       GOTO DISPLAY.BASE
		    END IF
		 END IF
	      END IF
	   END IF
	   
	   IF FORMAT.LOAD.CONTROL = CHECK.FORMAT THEN
	      RETURN
	   END IF

	   IF AUTO.START.FLAG = 1 THEN
	      IF DISPLAY.MODE$ = "B" THEN
		 SCREEN , , C.SCREEN, C.SCREEN
	      END IF
	      CLS
	      COLOR 15, BGCLR
	      LOCATE 25, 1, 0
	      PRINT "Loading format "; FMT$; " please wait !!!";
	   ELSE
	      IF DISPLAY.MODE$ = "B" THEN
		 SCREEN , , C.SCREEN, C.SCREEN
	      END IF
	      COLOR 15, BGCLR
	      LOCATE 25, 1, 0
	      PRINT "Loading format "; FMT$; " from server ... please wait !!!";
	   END IF

	   GOSUB GET.SERVER.FORMAT

	   IF DISPLAY.MODE$ = "B" THEN
	      SCREEN , , C.SCREEN + 1, C.SCREEN
	   END IF

	   COLOR 15, BGCLR
	   LOCATE 25, 1, 0: PRINT SPACE$(80);
	   IF ERROR.CODE = 1 THEN
	      RETURN
	   END IF
	ELSE
	   IF CLIENT.VER$ <> SERVER.VER$ THEN
	      IF DISPLAY.MODE$ = "B" THEN
		 SCREEN , , C.SCREEN, C.SCREEN
	      END IF
	      MID$(FORMATS$(FMT.INDEX), 11, 1) = "N"
	      KILL RAM.DRIVE$ + FMT$ + "." + CLIENT.VER$
	      COLOR 15, BGCLR
	      LOCATE 25, 1, 0
	      PRINT "Loading format "; FMT$; " from server ... please wait !!!";
	      IF DISPLAY.MODE$ = "B" THEN
		 SCREEN , , C.SCREEN + 1, C.SCREEN
	      END IF
	      GOSUB GET.SERVER.FORMAT
	      IF ERROR.CODE = 1 THEN
		 RETURN
	      END IF
	      GOTO DISPLAY.BASE
	   END IF
	   '
	   ' reply that the format is found by sending 0000
	   '
	   PRINT #1, "0000"; "FMT-OK"; SERVER.VER$; SYS$
	   COLOR 15, BGCLR: LOCATE 25, 1, 0: PRINT SPACE$(80);
	END IF

DISPLAY.BASE:

	IF AUTO.START.FLAG = 1 THEN
	   IF LEN(W1$) = 0 THEN
	      GOSUB RESET.COLORS
	      AUTO.START.FLAG = 0
	   END IF
	END IF

	IF AUTO.START.FLAG = 0 THEN
	   COLOR FGCLR, BGCLR
	END IF

	OPEN RAM.DRIVE$ + FMT$ + "." + CLIENT.VER$ FOR INPUT AS #3

	SCX = 1: SCROW(1) = 99
	SIX = 1: SIROW(1) = 99
	SOX = 1: SOROW(1) = 99
	SUSFN = 1
	MIOFFSET = 1
	MOOFFSET = 1

LFMT1000:

	IF EOF(3) THEN
	   GOTO LFMT9000
	END IF
	INPUT #3, FT$
	IF ASC(FT$) < 32 THEN
	   CLOSE #3
	   CALL DECOMPRESS.FORMAT.FILE(RAM.DRIVE$, FMT$, TRIGGER.FILE.OPEN, HELP.FILE.OPEN, HELP.FILE.NUMBER$, TRIGGER.FILE.NUMBER$, FMT.COUNT, MAX.FIELDS, CLIENT.VER$)
	   GOTO DISPLAY.BASE
	END IF

	IF CLEAR.SCREEN.FLAG = 0 THEN
	   CLS
	   CLEAR.SCREEN.FLAG = 1
	END IF

	SELECT CASE FT$
	   CASE "SC"
	      GOTO LFMTSC
	   CASE "SI"
	      GOTO LFMTSI
	   CASE "SO"
	      GOTO LFMTSO
	   CASE "MI"
	      GOTO LFMTMI
	   CASE "MO"
	      GOTO LFMTMO
	   CASE "CU"
	      GOTO LFMTCU
	   CASE "HI"
	      INPUT #3, FT$
	      GOTO LFMT1000
	   CASE "TG"
	      INPUT #3, FT$
	      INPUT #3, FT$
	      GOTO LFMT1000
	   CASE ELSE
	      PRINT "Unknown field-type "; FT$
	      GOTO PROGRAM.END
	END SELECT

LFMTSC:

	INPUT #3, SCROW(SCX), SCCOL(SCX), SCATT$(SCX), SCTXT$(SCX)
	IF SCROW(SCX) <> 1 GOTO LFMTSC1000
	IF SCCOL(SCX) > 50 GOTO LFMTSC1000
	SCCOL(SCX) = SCCOL(SCX) + 3

LFMTSC1000:

	LOCATE SCROW(SCX), SCCOL(SCX), 0
	CALL SET.COLORS(SCATT$(SCX), BGCLR, FGCLR, UNPRO, BLINK, DIMC)
	PRINT SCTXT$(SCX);
	SCX = SCX + 1
	SCROW(SCX) = 99
	GOTO LFMT1000

LFMTSI:

	INPUT #3, SIROW(SIX), SICOL(SIX), SILEN(SIX), SIPIC$(SIX), SIATT$(SIX), SISFN(SIX)
	X = INSTR(SIPIC$(SIX), "#")
	Y = INSTR(SIPIC$(SIX), "Z")
	IF X > 0 OR Y > 0 THEN
	   TYP(SIX) = 1
	ELSE
	   X = INSTR(SIATT$(SIX), CHR$(17))
	   IF X > 0 THEN
	      TYP(SIX) = 3 'input is converted to upper case
	   ELSE
	      TYP(SIX) = 2
	   END IF
	END IF

	SIX = SIX + 1
	SIROW(SIX) = 99
	GOTO LFMT1000

LFMTSO:

	INPUT #3, SOROW(SOX), SOCOL(SOX), SOLEN(SOX), SOPIC$(SOX), SOATT$(SOX), SOSFN(SOX)
	PIC$ = SOPIC$(SOX)
	CALL ANALIZE.PIC(PIC$)
	SOPIC$(SOX) = PIC$
	IF SOROW(SOX) <> 1 GOTO LFMTSO1000
	IF SOCOL(SOX) > 50 GOTO LFMTSO1000
	SOCOL(SOX) = SOCOL(SOX) + 3

LFMTSO1000:

	SOX = SOX + 1
	SOROW(SOX) = 99
	GOTO LFMT1000

LFMTMI:

	INPUT #3, MIMFN, PICLEN, PIC$
	IF MIMFN > 400 GOTO LFMTMI1000
	MIOFF(MIMFN) = MIOFFSET
	MIPIC$(MIMFN) = PIC$
	MILEN(MIMFN) = PICLEN

LFMTMI1000:

	MIOFFSET = MIOFFSET + PICLEN
	MAXMSI = MIOFFSET - 1
	GOTO LFMT1000

LFMTMO:

	INPUT #3, MOFN, PICLEN
	IF MOFN > 400 THEN
	   GOTO LFMTMO1000
	END IF
	MOOFF(MOFN) = MOOFFSET
	MOLEN(MOFN) = PICLEN

LFMTMO1000:

	MOOFFSET = MOOFFSET + PICLEN
	GOTO LFMT1000

LFMTCU:

	INPUT #3, CUSFN
	I.CU = 1

LFMTCU1000:

	IF SIROW(I.CU) = 99 GOTO LFMTCU2000
	IF CUSFN = SISFN(I.CU) GOTO LFMTCU3000
	I.CU = I.CU + 1
	GOTO LFMTCU1000

LFMTCU2000:

	I.CU = 1

LFMTCU3000:

	CUROW = SIROW(I.CU)
	CUCOL = SICOL(I.CU)
	GOTO LFMT1000

LFMT9000:

	CLOSE #3
	CURRENT.FMT$ = FMT$
	RETURN

SCREENOUTPUT:

	ERR.ROUTINE$ = "SCREENOUTPUT"
	ERASE CT$
	REM $DYNAMIC
	REDIM CT$(MAX.FIELDS)

	OMAP$ = SPACE$(80)
	K1 = 1
	SOX = 1

SCROUT1000:

	IF SOROW(SOX) = 99 THEN
	   RETURN
	END IF

	IF SOSFN(SOX) > 400 THEN
	   GOTO SCROUT2000
	END IF

	IF MOOFF(SOSFN(SOX)) + MOLEN(SOSFN(SOX)) > MOOFF(SOSFN(SOX)) THEN
	   IF MOOFF(SOSFN(SOX)) + MOLEN(SOSFN(SOX)) - 1 <= LEN(MSO$) THEN
	      IF SOSFN(SOX) > MAX.FIELDS THEN
		 IF GOT.TRIGGER <> 0 THEN
		    GOTO SCROUT1100
		 END IF
		 TXT$ = SPACE$(MOLEN(SOSFN(SOX)))
		 GOTO SCROUT1200
	      END IF
	      GOTO SCROUT1100
	   END IF
	END IF
	TXT$ = SPACE$(MOLEN(SOSFN(SOX)))
	GOTO SCROUT1200

SCROUT1100:

	TXT$ = MID$(MSO$, MOOFF(SOSFN(SOX)), MOLEN(SOSFN(SOX)))
	X = INSTR(SOATT$(SOX), CHR$(18))
	Y = INSTR(SOATT$(SOX), CHR$(19))
	IF X > 0 OR Y > 0 THEN
	   TXT$ = UCASE$(TXT$)
	END IF

SCROUT1200:

	CALL SET.COLORS(SOATT$(SOX), BGCLR, FGCLR, UNPRO, BLINK, DIMC)
	LOCATE SOROW(SOX), SOCOL(SOX), 0
	REM 'CONVERT MS TO SC !!
	'
	' Analize the PIC string to determine the format of the
	' field to be printed.
	'
	X = INSTR(SOPIC$(SOX), "#")
	Y = INSTR(SOPIC$(SOX), "~")
	IF Y > 0 THEN
	   NUMERIC.EDITED.CODE = 1
	ELSE
	   NUMERIC.EDITED.CODE = 0
	END IF
	IF X > 0 THEN
	   '
	   ' The field to be printed is a numeric field.
	   '
	   Z1 = INSTR(SOPIC$(SOX), ".")
	   Z2 = INSTR(SOPIC$(SOX), "-")
	   '
	   ' Determine the value of the field and print it according
	   ' to the format.
	   '
	   ' If the format includes a decimal point or a minus sign
	   ' we have to add a one space for each. This is only the
	   ' case when the field has a vlaue of 0 or sapces.
	   '
	   IF VAL(TXT$) = 0 OR TXT$ = SPACE$(LEN(TXT$)) THEN
	      TXT$ = STRING$(SOLEN(SOX), " ")
	      PIC.CODE = 2
	      CALL MOVE.TO.CT(K1, FEELDS, MAX.FIELDS, SISFN(), PRINT.FLAG, SOSFN(), SIROW(), SOROW(), SICOL(), SOCOL(), PIC.CODE, TXT$, SOLEN(), CT$(), TYP(), SOX)
	      IF PRINT.FLAG = 1 THEN
		 PRINT TXT$;
	      END IF
	      GOTO DONE.PRINTING
	   ELSE
	      '
	      ' Find out if there is a negative number in the string
	      ' of numbers to be printed. The negative number is
	      ' indecated by the higher order bit. If the bit is 0
	      ' the number is positive. If it is 1 then the number is
	      ' negative. Normaly there will be only one byte that
	      ' could have that bit on or off. Therefore we have to
	      ' check all the bytes in the string.
	      '
	      ' If the byte was found then we XOR it with 128 which
	      ' will turn the higher order bit off, then we multiply
	      ' the whole number by -1 to make it negative.
	      '
	      FOR Z = 1 TO LEN(TXT$)
		Z$ = MID$(TXT$, Z, 1)
		IF Z$ < CHR$(48) OR Z$ > CHR$(57) THEN
		   Z$ = CHR$(ASC(Z$) XOR 128)
		   MID$(TXT$, Z, 1) = Z$
		   TXT$ = LTRIM$(STR$(VAL(TXT$) * -1))
		   Z = 99
		END IF
	      NEXT Z
	      '
	      ' If the field has a decimal point, determine the number of
	      ' decimal places then multiply the number of decimal places
	      ' by hundreds of that number.
	      '
	      IF Z1 > 0 THEN
		 DIVIDE.BY = 1
		 FOR I = Z1 + 1 TO LEN(SOPIC$(SOX))
		    IF MID$(SOPIC$(SOX), I, 1) = "#" THEN
		       DIVIDE.BY = DIVIDE.BY * 10
		    END IF
		 NEXT I
		 TXT$ = LTRIM$(STR$(VAL(TXT$) / DIVIDE.BY))
	      END IF
	      FORMAT.STRING$ = RIGHT$(SOPIC$(SOX), LEN(SOPIC$(SOX)) - 1)
	      PIC.CODE = 2
	      CALL MOVE.TO.CT(K1, FEELDS, MAX.FIELDS, SISFN(), PRINT.FLAG, SOSFN(), SIROW(), SOROW(), SICOL(), SOCOL(), PIC.CODE, TXT$, SOLEN(), CT$(), TYP(), SOX)
	      IF PRINT.FLAG = 1 THEN
		 PRINT USING FORMAT.STRING$; VAL(TXT$);
	      END IF
	      GOTO DONE.PRINTING
	   END IF
	END IF
	'
	' If the field is a date field but not edited.
	'
	IF SOPIC$(SOX) = "99/99/99" THEN
	   IF VAL(TXT$) = 0 THEN
	      PIC.CODE = 2
	      CALL MOVE.TO.CT(K1, FEELDS, MAX.FIELDS, SISFN(), PRINT.FLAG, SOSFN(), SIROW(), SOROW(), SICOL(), SOCOL(), PIC.CODE, TXT$, SOLEN(), CT$(), TYP(), SOX)
	      IF PRINT.FLAG = 1 THEN
		 TXT$ = "00/00/00"
		 PRINT TXT$;
	      END IF
	      GOTO DONE.PRINTING
	   ELSE
	      PIC.CODE = 2
	      CALL MOVE.TO.CT(K1, FEELDS, MAX.FIELDS, SISFN(), PRINT.FLAG, SOSFN(), SIROW(), SOROW(), SICOL(), SOCOL(), PIC.CODE, TXT$, SOLEN(), CT$(), TYP(), SOX)
	      IF PRINT.FLAG = 1 THEN
		 PRINT LEFT$(TXT$, 2); "/"; MID$(TXT$, 3, 2); "/"; RIGHT$(TXT$, 2);
	      END IF
	      GOTO DONE.PRINTING
	   END IF
	END IF
	'
	' If the field is an edited date field.
	'
	IF SOPIC$(SOX) = "ZZ/ZZ/ZZ" THEN
	   IF VAL(TXT$) = 0 THEN
	      PIC.CODE = 2
	      CALL MOVE.TO.CT(K1, FEELDS, MAX.FIELDS, SISFN(), PRINT.FLAG, SOSFN(), SIROW(), SOROW(), SICOL(), SOCOL(), PIC.CODE, TXT$, SOLEN(), CT$(), TYP(), SOX)
	      IF PRINT.FLAG = 1 THEN
		 TXT$ = SPACE$(LEN(TXT$)) + "  "
		 PRINT TXT$;
	      END IF
	      GOTO DONE.PRINTING
	   ELSE
	      PIC.CODE = 2
	      CALL MOVE.TO.CT(K1, FEELDS, MAX.FIELDS, SISFN(), PRINT.FLAG, SOSFN(), SIROW(), SOROW(), SICOL(), SOCOL(), PIC.CODE, TXT$, SOLEN(), CT$(), TYP(), SOX)
	      IF PRINT.FLAG = 1 THEN
		 PRINT USING "##"; VAL(LEFT$(TXT$, 2));
		 PRINT "/";
		 PRINT USING "##"; VAL(MID$(TXT$, 3, 2));
		 PRINT "/";
		 PRINT USING "##"; VAL(RIGHT$(TXT$, 2));
	      END IF
	      GOTO DONE.PRINTING
	   END IF
	END IF
	'
	' If the field's picture is "99/99"
	'
	IF SOPIC$(SOX) = "99/99" THEN
	   IF VAL(TXT$) = 0 THEN
	      PIC.CODE = 2
	      CALL MOVE.TO.CT(K1, FEELDS, MAX.FIELDS, SISFN(), PRINT.FLAG, SOSFN(), SIROW(), SOROW(), SICOL(), SOCOL(), PIC.CODE, TXT$, SOLEN(), CT$(), TYP(), SOX)
	      IF PRINT.FLAG = 1 THEN
		 TXT$ = SPACE$(LEN(TXT$)) + " "
		 PIC$ = STRING$(LEN(TXT$), "#")
		 PRINT USING PIC$; VAL(TXT$);
	      END IF
	      GOTO DONE.PRINTING
	   ELSE
	      PIC.CODE = 2
	      CALL MOVE.TO.CT(K1, FEELDS, MAX.FIELDS, SISFN(), PRINT.FLAG, SOSFN(), SIROW(), SOROW(), SICOL(), SOCOL(), PIC.CODE, TXT$, SOLEN(), CT$(), TYP(), SOX)
	      IF PRINT.FLAG = 1 THEN
		 PRINT LEFT$(TXT$, 2); "/"; RIGHT$(TXT$, 2);
	      END IF
	      GOTO DONE.PRINTING
	   END IF
	END IF
	PIC.CODE = 1
	CALL MOVE.TO.CT(K1, FEELDS, MAX.FIELDS, SISFN(), PRINT.FLAG, SOSFN(), SIROW(), SOROW(), SICOL(), SOCOL(), PIC.CODE, TXT$, SOLEN(), CT$(), TYP(), SOX)
	IF PRINT.FLAG = 1 THEN
	   TXT$ = LEFT$(TXT$, SOLEN(SOX))
	   PRINT TXT$;
	END IF

DONE.PRINTING:

	IF SOROW(SOX) = 1 THEN
	   MID$(OMAP$, SOCOL(SOX), SOLEN(SOX)) = TXT$
	END IF

SCROUT2000:

	SOX = SOX + 1
	GOTO SCROUT1000
	RETURN

SETOUTPUT:

	ERR.ROUTINE$ = "SETOUTPUT"
	IF DISPLAY.MODE$ = "B" THEN
	   SCREEN , , C.SCREEN, C.SCREEN
	END IF
	COLOR 15, BGCLR
	LOCATE 25, 59, 0: PRINT "Sending Data";
	IF DISPLAY.MODE$ = "B" THEN
	   SCREEN , , C.SCREEN + 1, C.SCREEN
	END IF
	MSI$ = SPACE$(MAXMSI)
	IF SEND.SCREEN.FLAG = 1 THEN
	   MSI$ = ""
	   FOR I = 1 TO 24
	      MSI$ = MSI$ + LINES$(I)
	   NEXT I
	   GOTO SETOUT3200
	ELSE
	   CALL INITIALIZE.MESSAGE.OUT(MAX.FIELDS, MILEN(), MIPIC$(), MSI$, MIOFF())
	END IF
	SIX = 1

SETOUT1000:

	IF SIROW(SIX) = 99 GOTO SETOUT3000
	IF SISFN(SIX) > 400 GOTO SETOUT2000
	TXT$ = CT$(SIX)

	IF TYP(SIX) = 2 OR TYP(SIX) = 3 THEN
	   IF LEN(TXT$) = 0 THEN
	      TXT$ = STRING$(MOLEN(SISFN(SIX)), " ")
	   END IF
	END IF

	IF TYP(SIX) = 1 THEN
	   Z1 = INSTR(CT$(SIX), ".")
	   Z2 = INSTR(CT$(SIX), "-")
	   IF Z1 > 0 THEN
	      TXT$ = LEFT$(CT$(SIX), Z1 - 1) + RIGHT$(CT$(SIX), LEN(CT$(SIX)) - Z1)
	      X1 = X1 - 1
	   END IF
	   IF Z2 > 0 THEN
	      TXT$ = LEFT$(CT$(SIX), Z2 - 1) + RIGHT$(CT$(SIX), LEN(CT$(SIX)) - Z2)
	      X1 = X1 - 1
	   END IF
	   IF VAL(TXT$) = 0 THEN
	      TXT$ = STRING$(MOLEN(SISFN(SIX)), "0")
	   END IF
	END IF
	MID$(MSI$, MIOFF(SISFN(SIX)), SILEN(SIX)) = TXT$

SETOUT2000:

	SIX = SIX + 1
	GOTO SETOUT1000

SETOUT3000:

	FOR J = MAXMSI TO 0 STEP -1
	   IF J = 0 GOTO SETOUT3100
	   IF MID$(MSI$, J, 1) <> " " GOTO SETOUT3100
	NEXT J

SETOUT3100:

	IF J > 0 GOTO SETOUT3200
	TXO$ = "0002"
	GOTO SETOUT4000

SETOUT3200:

	IF SEND.SCREEN.FLAG = 1 THEN
	   J = 1920
	   TXO$ = MSI$
	   GOTO SETOUT4000
	END IF
	TXO$ = LTRIM$(STR$(J))
	TXO$ = LEFT$("0000", 4 - LEN(TXO$)) + TXO$
	TXO$ = TXO$ + MID$(MSI$, 5, J - 4)

SETOUT4000:

	VLI$ = LTRIM$(STR$(LEN(TXO$) + 20))
	VLI$ = LEFT$("0000", 4 - LEN(VLI$)) + VLI$
	IF SEND.SCREEN.FLAG = 1 THEN
	   NEXT.FUNCTION$ = "~RSCRN"
	ELSE
	   NEXT.FUNCTION$ = MID$(OMAP$, 74, 6)
	END IF
	'IF SIROW(1) = 1 AND SICOL(1) = 74 THEN
	   IF SEND.SCREEN.FLAG = 0 THEN
	      NEXT.FUNCTION$ = CT$(1) + SPACE$(6 - LEN(CT$(1)))
	      IF FMT$ = "TX0002" THEN
		 NEXT.FUNCTION$ = CT$(3) + SPACE$(6 - LEN(CT$(3)))
	      END IF
	   END IF
	'END IF

SETOUT5100:

	IF NEXT.FUNCTION$ <> SPACE$(6) THEN
	   FMT$ = NEXT.FUNCTION$
	END IF

	FORMAT.LOAD.CONTROL = CHECK.FORMAT
	GOSUB LOADFORMAT
	FORMAT.LOAD.CONTROL = COMPLETE.PROCESS

	IF FORMAT.FOUND <> 0 THEN
	   SERVER.VER$ = CLIENT.VER$
	   TEMP.VER$ = SERVER.VER$
	ELSE
	   VLI$ = "9999"
	   TEMP.VER$ = "ZZ"
	END IF

	PRINT #1, VLI$; FMT$; TEMP.VER$; SYS$; NEXT.FUNCTION$; TXO$
	SEND.SCREEN.FLAG = 0

SETOUT6000:

	COLOR FGCLR, BGCLR
	LOCATE 25, 59, 0: PRINT SPC(20);
	RETURN

LOADCONFIG:

	ERR.ROUTINE$ = "LOADCONFIG"
	BGCLR = BLACK
	FGCLR = H.WHITE
	UNPRO = H.BROWN
	BLINK = H.MAGENTA + 8
	DIMC = WHITE
	DISPLAY.MODE$ = "B"
	DISK.TYPE$ = "R"
	PORT$ = "COM1:19200,E," + COMM.PARAM$
	TIME.OUT = 60
	COMM = 1
	'
	OPEN "SCR-MNGR.INI" FOR INPUT AS #1
	DO UNTIL EOF(1)
	   LINE INPUT #1, A$
	   IF LEFT$(A$, 1) = "*" THEN
	      GOTO GET.NEXT.CONFIG.RECORD
	   END IF
	   SP = INSTR(A$, " ")
	   EQ = INSTR(A$, "=")
	   LA$ = LTRIM$(LEFT$(A$, SP - 1))
	   RA$ = LTRIM$(RIGHT$(A$, LEN(A$) - EQ))

	   SELECT CASE LA$
	      CASE "BACKGROUND"
		 COLOR.CODE = 0
		 GOSUB ASSIGN.COLORS
		 BGCLR = COLOR.CODE
	      CASE "NORMAL"
		 COLOR.CODE = 0
		 GOSUB ASSIGN.COLORS
		 FGCLR = COLOR.CODE
	      CASE "UNPROTECTED"
		 COLOR.CODE = 0
		 GOSUB ASSIGN.COLORS
		 UNPRO = COLOR.CODE
	      CASE "BLINK"
		 COLOR.CODE = 0
		 GOSUB ASSIGN.COLORS
		 BLINK = COLOR.CODE + 16
	      CASE "DIM"
		 COLOR.CODE = 0
		 GOSUB ASSIGN.COLORS
		 DIMC = COLOR.CODE
	      CASE "DISP.MODE"
		 DISPLAY.MODE$ = LEFT$(RA$, 1)
	      CASE "PORT"
		 PORT$ = RA$ + COMM.PARAM$
		 COMM = VAL(MID$(PORT$, 4, 1))
	      CASE "TIME.OUT"
		 TIME.OUT = VAL(RA$)
	      CASE "PRN.PORT"
		 PRN.PORT$ = RA$
	      CASE "PRN.ENABLE"
		 PRN.ENABLE$ = HEX.TO.CHAR$(RA$)
	      CASE "PRN.DISABLE"
		 PRN.DISABLE$ = HEX.TO.CHAR$(RA$)
	      CASE "RAM.DRIVE"
		 RAM.DRIVE$ = RA$
	      CASE "MIN.SPACE"
		 MIN.SPACE# = VAL(RA$) * 1024
	      CASE "EXT.DRIVE"
		 EXT.DRIVE$ = RA$
	      CASE "EXT.MIN.SP"
		 EXT.MIN.SP# = VAL(RA$) * 1024
	      CASE "REFRESH"
		 R.FREQ = VAL(RA$)
	      CASE "EXT.ENABLE"
		 EXT.ENABLE$ = RA$
	      CASE "F1"
		 GOSUB DEFINE.KEY
		 SWAP F1.KEY$, KEY.STRING$
	      CASE "F2"
		 GOSUB DEFINE.KEY
		 SWAP F2.KEY$, KEY.STRING$
	      CASE "F3"
		 GOSUB DEFINE.KEY
		 SWAP F3.KEY$, KEY.STRING$
	      CASE "F4"
		 GOSUB DEFINE.KEY
		 SWAP F4.KEY$, KEY.STRING$
	      CASE "F5"
		 GOSUB DEFINE.KEY
		 SWAP F5.KEY$, KEY.STRING$
	      CASE "F6"
		 GOSUB DEFINE.KEY
		 SWAP F6.KEY$, KEY.STRING$
	      CASE "F7"
		 GOSUB DEFINE.KEY
		 SWAP F7.KEY$, KEY.STRING$
	      CASE "F8"
		 GOSUB DEFINE.KEY
		 SWAP F8.KEY$, KEY.STRING$
	      CASE "F9"
		 GOSUB DEFINE.KEY
		 SWAP F9.KEY$, KEY.STRING$
	      CASE "F10"
		 GOSUB DEFINE.KEY
		 SWAP F10.KEY$, KEY.STRING$
	      CASE "F11"
		 GOSUB DEFINE.KEY
		 SWAP F11.KEY$, KEY.STRING$
	      CASE "F12"
		 GOSUB DEFINE.KEY
		 SWAP F12.KEY$, KEY.STRING$
	      CASE "#F1"
		 GOSUB DEFINE.KEY
		 SWAP SF1.KEY$, KEY.STRING$
	      CASE "#F2"
		 GOSUB DEFINE.KEY
		 SWAP SF2.KEY$, KEY.STRING$
	      CASE "#F3"
		 GOSUB DEFINE.KEY
		 SWAP SF3.KEY$, KEY.STRING$
	      CASE "#F4"
		 GOSUB DEFINE.KEY
		 SWAP SF4.KEY$, KEY.STRING$
	      CASE "#F5"
		 GOSUB DEFINE.KEY
		 SWAP SF5.KEY$, KEY.STRING$
	      CASE "#F6"
		 GOSUB DEFINE.KEY
		 SWAP SF6.KEY$, KEY.STRING$
	      CASE "#F7"
		 GOSUB DEFINE.KEY
		 SWAP SF7.KEY$, KEY.STRING$
	      CASE "#F8"
		 GOSUB DEFINE.KEY
		 SWAP SF8.KEY$, KEY.STRING$
	      CASE "#F9"
		 GOSUB DEFINE.KEY
		 SWAP SF9.KEY$, KEY.STRING$
	      CASE "#F10"
		 GOSUB DEFINE.KEY
		 SWAP SF10.KEY$, KEY.STRING$
	      CASE "#F11"
		 GOSUB DEFINE.KEY
		 SWAP SF11.KEY$, KEY.STRING$
	      CASE "#F12"
		 GOSUB DEFINE.KEY
		 SWAP SF12.KEY$, KEY.STRING$
	      CASE "^F1"
		 GOSUB DEFINE.KEY
		 SWAP CF1.KEY$, KEY.STRING$
	      CASE "^F2"
		 GOSUB DEFINE.KEY
		 SWAP CF2.KEY$, KEY.STRING$
	      CASE "^F3"
		 GOSUB DEFINE.KEY
		 SWAP CF3.KEY$, KEY.STRING$
	      CASE "^F4"
		 GOSUB DEFINE.KEY
		 SWAP CF4.KEY$, KEY.STRING$
	      CASE "^F5"
		 GOSUB DEFINE.KEY
		 SWAP CF5.KEY$, KEY.STRING$
	      CASE "^F6"
		 GOSUB DEFINE.KEY
		 SWAP CF6.KEY$, KEY.STRING$
	      CASE "^F7"
		 GOSUB DEFINE.KEY
		 SWAP CF7.KEY$, KEY.STRING$
	      CASE "^F8"
		 GOSUB DEFINE.KEY
		 SWAP CF8.KEY$, KEY.STRING$
	      CASE "^F9"
		 GOSUB DEFINE.KEY
		 SWAP CF9.KEY$, KEY.STRING$
	      CASE "^F10"
		 GOSUB DEFINE.KEY
		 SWAP CF10.KEY$, KEY.STRING$
	      CASE "^F11"
		 GOSUB DEFINE.KEY
		 SWAP CF11.KEY$, KEY.STRING$
	      CASE "^F12"
		 GOSUB DEFINE.KEY
		 SWAP CF12.KEY$, KEY.STRING$
	      CASE "@F1"
		 GOSUB DEFINE.KEY
		 SWAP AF1.KEY$, KEY.STRING$
	      CASE "@F2"
		 GOSUB DEFINE.KEY
		 SWAP AF2.KEY$, KEY.STRING$
	      CASE "@F3"
		 GOSUB DEFINE.KEY
		 SWAP AF3.KEY$, KEY.STRING$
	      CASE "@F4"
		 GOSUB DEFINE.KEY
		 SWAP AF4.KEY$, KEY.STRING$
	      CASE "@F5"
		 GOSUB DEFINE.KEY
		 SWAP AF5.KEY$, KEY.STRING$
	      CASE "@F6"
		 GOSUB DEFINE.KEY
		 SWAP AF6.KEY$, KEY.STRING$
	      CASE "@F7"
		 GOSUB DEFINE.KEY
		 SWAP AF7.KEY$, KEY.STRING$
	      CASE "@F8"
		 GOSUB DEFINE.KEY
		 SWAP AF8.KEY$, KEY.STRING$
	      CASE "@F9"
		 GOSUB DEFINE.KEY
		 SWAP AF9.KEY$, KEY.STRING$
	      CASE "@F10"
		 GOSUB DEFINE.KEY
		 SWAP AF10.KEY$, KEY.STRING$
	      CASE "@F11"
		 GOSUB DEFINE.KEY
		 SWAP AF11.KEY$, KEY.STRING$
	      CASE "@F12"
		 GOSUB DEFINE.KEY
		 SWAP AF12.KEY$, KEY.STRING$
	      CASE ELSE
		 PRINT "Unknown CONFIGURATION Parameter "; LA$
		 GOTO PROGRAM.END
	   END SELECT

GET.NEXT.CONFIG.RECORD:

	LOOP
	CLOSE #1

	OLD.BGCLR = BGCLR
	OLD.FGCLR = FGCLR
	OLD.UNPRO = UNPRO
	OLD.BLINK = BLINK
	OLD.DIMC = DIMC
	COLORS.FLAG = 1

	RETURN

DEFINE.KEY:

	KEY.STRING$ = ""
	FOR I = 1 TO LEN(RA$)
	   IF MID$(RA$, I, 1) = "+" THEN
	      GOTO NEXT.LETTER
	   END IF
	   IF MID$(RA$, I, 1) = "[" THEN
	      X$ = ""
	      I = I + 1
	      DO UNTIL MID$(RA$, I, 1) = "]"
		 X$ = X$ + MID$(RA$, I, 1)
		 I = I + 1
	      LOOP
	      SELECT CASE X$
		 CASE "ENTER"
		    KEY.STRING$ = KEY.STRING$ + ACCEPT$
		 CASE "HOME"
		    KEY.STRING$ = KEY.STRING$ + HOME$
		 CASE "END"
		    KEY.STRING$ = KEY.STRING$ + END.KEY$
		 CASE "SEND SCREEN"
		    KEY.STRING$ = KEY.STRING$ + SEND.SCREEN$
		 CASE "HELP"
		    KEY.STRING$ = KEY.STRING$ + HELP$
		 CASE "TAB"
		    KEY.STRING$ = KEY.STRING$ + TAB$
		 CASE "BACK TAB"
		    KEY.STRING$ = KEY.STRING$ + BACK.TAB$
		 CASE "RECALL INPUT"
		    KEY.STRING$ = KEY.STRING$ + RECALL.INPUT$
		 CASE "DOS"
		    KEY.STRING$ = KEY.STRING$ + EXIT.TO.DOS$
		 CASE "SHOW KEYS"
		    KEY.STRING$ = KEY.STRING$ + SHOW.KEYS$
		 CASE "CALCULATOR"
		    KEY.STRING$ = KEY.STRING$ + CALCULATOR$
		 CASE "CALCULATOR W/TRANSFER"
		    KEY.STRING$ = KEY.STRING$ + CALCULATOR.T$
		 CASE "INITIALIZE FIELDS"
		    KEY.STRING$ = KEY.STRING$ + INIT.FIELDS$
	      END SELECT
	      GOTO NEXT.LETTER
	   END IF
	   KEY.STRING$ = KEY.STRING$ + MID$(RA$, I, 1)

NEXT.LETTER:

	NEXT I
	RETURN

SET.SECONDS:

	OLD.SECONDS = VAL(RIGHT$(TIME$, 2))
	SECONDS.COUNT = 0
	TIME.OUT.FLAG = 0
	RETURN

CHECK.TIME:

	NEW.SECONDS = VAL(RIGHT$(TIME$, 2))
	IF NEW.SECONDS <> OLD.SECONDS THEN
	   OLD.SECONDS = NEW.SECONDS
	   SECONDS.COUNT = SECONDS.COUNT + 1
	   LOCATE 25, 75, 0
	   PRINT SECONDS.COUNT;
	END IF
	IF SECONDS.COUNT > TIME.OUT THEN
	   TIME.OUT.FLAG = 1
	END IF

	RETURN

GET.SERVER.FORMAT:

	ERR.ROUTINE$ = "GET.SERVER.FORMAT"

	OPEN RAM.DRIVE$ + FMT$ + "." + SERVER.VER$ FOR OUTPUT AS #5
	GOSUB SET.SECONDS
	'
	' reply that the format is not found by sending 9999
	'
	IF FORMAT.LOAD.CONTROL <> FIRST.PART.LOADED THEN
	   PRINT #1, "9999"; FMT$; "ZZ"; SYS$; NEXT.FUNCTION$; TXO$
	ELSE
	   GOTO FIRST.PART.1
	END IF

GET.FMT.START:

	COM(COMM) ON
	TXI$ = ""

GET.FMT.LOOP:

	IF LEN(TXI$) < 4 THEN
	   GOSUB CHECK.TIME
	   IF TIME.OUT.FLAG = 1 THEN
	      ERROR.CODE = 1
	      CLOSE #5, #6
	      RETURN
	   END IF
	   GOTO GET.FMT.LOOP
	END IF
	TXIL = VAL(LEFT$(TXI$, 4))

GET.FMT.LINE:

	IF NOT EOF(1) OR LEN(TXI$) < TXIL THEN
	   GOTO GET.FMT.LINE
	END IF

	COM(COMM) STOP

FIRST.PART.1:

	TXI$ = LEFT$(TXI$, TXIL)
	IF MID$(TXI$, 15, 6) = "~ERROR" THEN
	   COLOR FGCLR, BGCLR
	   LOCATE 25, 1, 0: PRINT SPACE$(80);
	   LOCATE 25, 1, 0
	   PRINT RTRIM$(MID$(TXI$, 21, LEN(TXI$) - 20));
	   ERROR.CODE = 1
	   LOCATE 25, 55, 0: PRINT "Press any key to continue";

ANY.KEY.LOOP:

	   IF INKEY$ = "" GOTO ANY.KEY.LOOP

	   PRINT #1, SPACE$(20)
	   CLOSE #5, #6
	   RETURN
	END IF
	'
	' go and store whatever was received in the format file
	'
	GOSUB WRITE.FMT.TO.FILE
	'
	' if there are no more FMT description ... close the
	' the data file and update the table in memory and
	' send a message to the requester because it is now waiting
	' to continue its own processing. Then return.
	'
	IF MID$(TXI$, 15, 6) = "NOMORE" THEN
	   CLOSE #5, #6
	   IF FORMAT.FOUND = 1 THEN
	      MID$(FORMATS$(FMT.INDEX), 1, 8) = MID$(TXI$, 5, 8)
	   ELSE
	      FMT.COUNT = FMT.COUNT + 1
	      FMT.INDEX = FMT.COUNT
	      FORMATS$(FMT.COUNT) = SPACE$(11)
	      MID$(FORMATS$(FMT.COUNT), 1, 8) = MID$(TXI$, 5, 8)
	      MID$(FORMATS$(FMT.COUNT), 9, 2) = "01"
	      MID$(FORMATS$(FMT.COUNT), 11, 1) = "N"
	   END IF
	   CLIENT.VER$ = SERVER.VER$
	   PRINT #1, "0000"; "FMT-OK"; SERVER.VER$; SYS$
	   RETURN
	END IF
	'
	' if there are more FMT description ... send a message to the
	' requester in order for it to continue sending the rest of
	' the FMT description. Go back to receive the rest of the data.
	'
	PRINT #1, "0000"; FMT$; SERVER.VER$; SYS$
	GOTO GET.FMT.START

WRITE.FMT.TO.FILE:

	ERR.ROUTINE$ = "WRITE.FMT.TO.FILE"

	IF TXIL > 20 AND LEN(TXI$) >= 21 THEN
	   FMT.DESC$ = MID$(TXI$, 21, TXIL - 20)
	ELSE
	   BEEP
	   SCREEN , , 0, 0
	   CLS
	   PRINT "TRANSMISSION ERROR ... PLEASE RESTART"
	   GOTO PROGRAM.END
	END IF

WRITE.FMT.LOOP:

	J = INSTR(FMT.DESC$, "~~~")
	IF J > 0 THEN
	   R$ = MID$(FMT.DESC$, 1, J - 1)
	   FMT.DESC$ = MID$(FMT.DESC$, J + 3, LEN(FMT.DESC$) - J + 3)
	   IF LEFT$(R$, 3) <> "HI," AND LEFT$(R$, 3) <> "TG," THEN
	      PRINT #5, R$
	   END IF
	   IF LEFT$(R$, 3) = "SI," THEN
	      FOR Q1 = LEN(R$) TO 1 STEP -1
		 IF MID$(R$, Q1, 1) = "," THEN
		    LAST.FIELD.NUM = VAL(MID$(R$, Q1 + 1, LEN(R$) - Q1))
		    Q1 = 1
		 END IF
	      NEXT Q1
	   END IF
	   IF LEFT$(R$, 3) = "HI," THEN
	      CALL CREATE.HELP.FILE(RAM.DRIVE$, FMT.COUNT, HELP.FILE.NUMBER$, HELP.FILE.OPEN, FMT.DESC$, LAST.FIELD.NUM, R$)
	   END IF
	   IF LEFT$(R$, 3) = "TG," THEN
	      CALL CREATE.TRIGGER.FILE(RAM.DRIVE$, FMT.COUNT, TRIGGER.FILE.NUMBER$, TRIGGER.FILE.OPEN, FMT.DESC$, R$)
	   END IF
	   GOTO WRITE.FMT.LOOP
	END IF
	RETURN

DATA.ENTRY.ROUTINE:

	ERR.ROUTINE$ = "DATA.ENTRY.ROUTINE"

	FEELDS = SIX - 1

DATA.ENTRY.ROUTINE.START:

	FOR CURRENT.FIELD = 1 TO FEELDS
	   LOCATE SIROW(CURRENT.FIELD), SICOL(CURRENT.FIELD), 0
	   '
	   ' add the UNPROTECTED attribute to the input fields.
	   '
	   IF INSTR(SIATT$(CURRENT.FIELD), CHR$(2)) = 0 THEN
	      SIATT$(CURRENT.FIELD) = SIATT$(CURRENT.FIELD) + CHR$(2)
	   END IF
	   COLOR UNPRO, BGCLR
	   PRINT CT$(CURRENT.FIELD); STRING$(SILEN(CURRENT.FIELD) - LEN(CT$(CURRENT.FIELD)), F$);
	NEXT CURRENT.FIELD

	IF I.CU = 0 THEN
	   I.CU = 1
	END IF

	FOR CURRENT.FIELD = I.CU TO FEELDS

	   CALL SET.COLORS(SIATT$(CURRENT.FIELD), BGCLR, FGCLR, UNPRO, BLINK, DIMC)
	   INS.FLAG = 0

JUMP1:

	   IF C1 = 1 THEN GOTO JUMP2
	   LOCATE SIROW(CURRENT.FIELD), SICOL(CURRENT.FIELD), 0
	   IF INSTR(SIATT$(CURRENT.FIELD), CHR$(1)) > 0 THEN
	      COLOR UNPRO, BGCLR
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

	      IF FUNC.KEY.FLAG <> 1 THEN
		 IF SPECIAL.CHAR.FLAG = 0 THEN
		    IN.ARRAY%(ax) = &H1100
		    CALL INT86OLD(&H16, IN.ARRAY%(), OUT.ARRAY%())
		    FLGS% = OUT.ARRAY%(FL)
		    CALL DECIMAL.TO.BINARY(FLGS%, FLGS$)
		 END IF
		 IF MID$(FLGS$, 9, 1) = "0" THEN
		    IN.ARRAY%(ax) = &H700
		    CALL INT86OLD(&H21, IN.ARRAY%(), OUT.ARRAY%())
		    W$ = RIGHT$(HEX$(OUT.ARRAY%(ax)), 2)
		    W$ = CHR$(HEX.TO.DECIMAL(W$))
		 ELSE
		    GOTO GET.CHARACTER
		 END IF
	      ELSE
		 IF LEN(W1$) = 0 THEN
		    W$ = W1$
		    FUNC.KEY.FLAG = 0
		    IF AUTO.START.FLAG = 1 THEN
		       GOSUB RESET.COLORS
		    END IF
		 ELSE
		    W$ = LEFT$(W1$, 1)
		    IF W$ = CHR$(27) THEN
		       W$ = CHR$(0)
		    END IF
		    W1$ = RIGHT$(W1$, LEN(W1$) - 1)
		 END IF
	      END IF

	      IF W$ = CHR$(0) THEN
		 SPECIAL.CHAR.FLAG = 1
		 MID$(FLGS$, 9, 1) = "0"
		 GOTO GET.CHARACTER
	      END IF

	      IF SPECIAL.CHAR.FLAG = 1 THEN
		 SPECIAL.CHAR.FLAG = 0
		 W$ = CHR$(0) + W$
	      ELSE
		 SPECIAL.CHAR.FLAG = 0
	      END IF

	      IF LEN(W$) = 2 THEN
		 Z$ = RIGHT$(W$, 1)
		 SELECT CASE Z$
		    CASE CHR$(73)
		       CALL PROCESS.PGUP.PGDN.KEY(H.SCREEN, W$, C.SCREEN)
		       GOTO DATA.ENTRY.ROUTINE.START
		    CASE CHR$(81)
		       CALL PROCESS.PGUP.PGDN.KEY(H.SCREEN, W$, C.SCREEN)
		       GOTO DATA.ENTRY.ROUTINE.START
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

	      IF W$ = CHR$(8) THEN GOTO PROCESS.BACK.SPACE

	      CALL VALIDATE.INPUT(ALLOWED.KEYS$, W$, ERROR.CODE, BACK.TAB$, ASCII$, TYP(), ACCEPT$, HELP$, HOME$, END.KEY$, CURRENT.FIELD)

	      IF ERROR.CODE = 1 THEN
		 ERROR.CODE = 0
		 LOCATE 25, 1, 0
		 PRINT "Invalid character for the field type ...";
		 BEEP
		 GOTO GET.CHARACTER
	      END IF

	      COLOR FGCLR, BGCLR
	      LOCATE 25, 1, 0: PRINT SPACE$(80);
	      CALL SET.COLORS(SIATT$(CURRENT.FIELD), BGCLR, FGCLR, UNPRO, BLINK, DIMC)
	      '
	      ' Test for function key.
	      '
	      IF FUNC.KEY.FLAG <> 1 THEN
		 SELECT CASE W$
		    CASE IS = F1$
		       W1$ = F1.KEY$
		    CASE IS = F2$
		       W1$ = F2.KEY$
		    CASE IS = F3$
		       W1$ = F3.KEY$
		    CASE IS = F4$
		       W1$ = F4.KEY$
		    CASE IS = F5$
		       W1$ = F5.KEY$
		    CASE IS = F6$
		       W1$ = F6.KEY$
		    CASE IS = F7$
		       W1$ = F7.KEY$
		    CASE IS = F8$
		       W1$ = F8.KEY$
		    CASE IS = F9$
		       W1$ = F9.KEY$
		    CASE IS = F10$
		       W1$ = F10.KEY$
		    CASE IS = F11$
		       W1$ = F11.KEY$
		    CASE IS = F12$
		       W1$ = F12.KEY$
		    '
		    ' Shift function keys
		    '
		    CASE IS = SF1$
		       W1$ = SF1.KEY$
		    CASE IS = SF2$
		       W1$ = SF2.KEY$
		    CASE IS = SF3$
		       W1$ = SF3.KEY$
		    CASE IS = SF4$
		       W1$ = SF4.KEY$
		    CASE IS = SF5$
		       W1$ = SF5.KEY$
		    CASE IS = SF6$
		       W1$ = SF6.KEY$
		    CASE IS = SF7$
		       W1$ = SF7.KEY$
		    CASE IS = SF8$
		       W1$ = SF8.KEY$
		    CASE IS = SF9$
		       W1$ = SF9.KEY$
		    CASE IS = SF10$
		       W1$ = SF10.KEY$
		    CASE IS = SF11$
		       W1$ = SF11.KEY$
		    CASE IS = SF12$
		       W1$ = SF12.KEY$
		    '
		    ' CTRL function keys
		    '
		    CASE IS = CF1$
		       W1$ = CF1.KEY$
		    CASE IS = CF2$
		       W1$ = CF2.KEY$
		    CASE IS = CF3$
		       W1$ = CF3.KEY$
		    CASE IS = CF4$
		       W1$ = CF4.KEY$
		    CASE IS = CF5$
		       W1$ = CF5.KEY$
		    CASE IS = CF6$
		       W1$ = CF6.KEY$
		    CASE IS = CF7$
		       W1$ = CF7.KEY$
		    CASE IS = CF8$
		       W1$ = CF8.KEY$
		    CASE IS = CF9$
		       W1$ = CF9.KEY$
		    CASE IS = CF10$
		       W1$ = CF10.KEY$
		    CASE IS = CF11$
		       W1$ = CF11.KEY$
		    CASE IS = CF12$
		       W1$ = CF12.KEY$
		    '
		    ' ALT function keys
		    '
		    CASE IS = AF1$
		       W1$ = AF1.KEY$
		    CASE IS = AF2$
		       W1$ = AF2.KEY$
		    CASE IS = AF3$
		       W1$ = AF3.KEY$
		    CASE IS = AF4$
		       W1$ = AF4.KEY$
		    CASE IS = AF5$
		       W1$ = AF5.KEY$
		    CASE IS = AF6$
		       W1$ = AF6.KEY$
		    CASE IS = AF7$
		       W1$ = AF7.KEY$
		    CASE IS = AF8$
		       W1$ = AF8.KEY$
		    CASE IS = AF9$
		       W1$ = AF9.KEY$
		    CASE IS = AF10$
		       W1$ = AF10.KEY$
		    CASE IS = AF11$
		       W1$ = AF11.KEY$
		    CASE IS = AF12$
		       W1$ = AF12.KEY$
		    CASE ELSE
		       FUNC.KEY.FLAG = 0
		       GOTO CONTINUE.CHECK
		 END SELECT
		 FUNC.KEY.FLAG = 1
		 GOTO GET.CHARACTER
	      END IF

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
		    ERROR.CODE = 0
		    CALL VALIDATE.FIELDS.PIC(FEELDS, CT$(), TYP(), ERROR.CODE, CURRENT.FIELD, SIPIC$())
		    IF ERROR.CODE = 1 THEN
		       ERROR.CODE = 0
		       GOTO JUMP2
		    END IF
		    IF CT$(1) = "" THEN
		       FOR J = 1 TO FEELDS
			   R.SILEN(J) = SILEN(J)
			   R.SIPIC$(J) = SIPIC$(J)
			   R.CT$(J) = CT$(J)
			   R.TYP(J) = TYP(J)
		       NEXT J
		    END IF
		    RETURN
		 CASE HELP$
		    GOSUB DISPLAY.HELP.WINDOW
		    POZ = POZ - 1
		    GOTO NEXT.POSITION
		 CASE EXIT.TO.DOS$
		    CALL DOS(C.SCREEN)
		    POZ = POZ - 1
		    GOTO NEXT.POSITION
		 CASE COLORS$, COMMS$, DISK$, FKEYS$, SFKEYS$, CFKEYS$, AFKEYS$, AUTO$, PASSWORD$, CALC.CONFIG$
		    CALL CONFIG(W$, C.SCREEN, COLORS$, COMMS$, DISK$, FKEYS$, SFKEYS$, CFKEYS$, AFKEYS$, AUTO$, PASSWORD$, CALC.CONFIG$, BGCLR)
		    SELECT CASE W$
		       CASE COLORS$, COMMS$, DISK$, FKEYS$, SFKEYS$, CFKEYS$, AFKEYS$
			  CLOSE
			  GOSUB LOADCONFIG
		       CASE CALC.CONFIG$
			  OUT.BUFFER$ = "NEW"
		    END SELECT
		    POZ = POZ - 1
		    GOTO NEXT.POSITION
		 CASE SHOW.KEYS$
		    CALL DISPLAY.FUNCTION.KEYS(C.SCREEN)
		    POZ = POZ - 1
		    GOTO NEXT.POSITION
		 CASE CALCULATOR$, CALCULATOR.T$
		    IF W$ = CALCULATOR.T$ THEN
		       IF TYP(CURRENT.FIELD) = 1 THEN
			  OUT.BUFFER$ = CT$(CURRENT.FIELD)
		       END IF
		    ELSE
		       OUT.BUFFER$ = ""
		    END IF
		    CALL CALC(OUT.BUFFER$, C.SCREEN, BGCLR, SWidth%, VideoAddr%, PrL%, PrR%, PrLr%, PrM%, PrLM%, PrMR%, PrAll%, PrNone%, ButtonLeft%, ButtonRight%, ButtonMiddle%, True%, False%, COLORED)
		    SCREEN , , C.SCREEN, C.SCREEN
		    COLOR UNPRO, BGCLR
		    IF OUT.BUFFER$ <> "" THEN
		       CT$(CURRENT.FIELD) = LEFT$(OUT.BUFFER$, SILEN(CURRENT.FIELD))
		       LOCATE SIROW(CURRENT.FIELD), SICOL(CURRENT.FIELD)
		       PRINT CT$(CURRENT.FIELD);
		    END IF
		    POZ = POZ - 1
		    GOTO NEXT.POSITION
		 CASE SEND.SCREEN$
		    SEND.SCREEN.FLAG = 1
		    CALL SEND.SCREEN.ROUTINE(LINES$())
		    EXIT.FLAG = 2
		    RETURN
		 CASE RECALL.INPUT$
		    FOR J = 1 TO FEELDS
		       IF R.SILEN(J) = SILEN(J) THEN
			  IF R.SIPIC$(J) = SIPIC$(J) THEN
			     IF R.TYP(J) = TYP(J) THEN
				CT$(J) = R.CT$(J)
			     END IF
			  END IF
		       END IF
		    NEXT J
		    GOTO DATA.ENTRY.ROUTINE.START
		 CASE INIT.FIELDS$
		    ERASE CT$
		    REM $DYNAMIC
		    REDIM CT$(MAX.FIELDS)
		    GOTO DATA.ENTRY.ROUTINE
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
			  IF INSTR(SIATT$(CURRENT.FIELD), CHR$(1)) > 0 THEN
			     COLOR UNPRO, BGCLR
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
		    IF INSTR(SIATT$(CURRENT.FIELD), CHR$(1)) > 0 THEN
		       COLOR UNPRO, BGCLR
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
		    IF INSTR(SIATT$(CURRENT.FIELD), CHR$(1)) > 0 THEN
		       COLOR UNPRO, BGCLR
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
		    IF INSTR(SIATT$(CURRENT.FIELD), CHR$(1)) > 0 THEN
		       COLOR UNPRO, BGCLR
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
		 IF INSTR(SIATT$(CURRENT.FIELD), CHR$(1)) > 0 THEN
		    COLOR UNPRO, BGCLR
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
	      IF INSTR(SIATT$(CURRENT.FIELD), CHR$(1)) > 0 THEN
		 COLOR UNPRO, BGCLR
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
		 IF INSTR(SIATT$(CURRENT.FIELD), CHR$(1)) > 0 THEN
		    COLOR UNPRO, BGCLR
		    PRINT STRING$(LEN(XX$), "*"); F$; STRING$(I1, F$); STRING$(LEN(LTRIM$(YY$)), "*");
		    COLOR BGCLR, BGCLR
		 ELSE
		    PRINT XX$; F$; STRING$(I1, F$); LTRIM$(YY$);
		 END IF
		 BS.KEY = 1
	      END IF
	      Y = Y - 1
	      LOCATE X, Y, 1, 0, 31
	      IF INSTR(SIATT$(CURRENT.FIELD), CHR$(1)) > 0 THEN
		 COLOR UNPRO, BGCLR
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

	ERR.ROUTINE$ = "ANALIZE.RIGHT.ARROW"

	CALL VALIDATE.INPUT(ALLOWED.KEYS$, W$, ERROR.CODE, BACK.TAB$, ASCII$, TYP(), ACCEPT$, HELP$, HOME$, END.KEY$, CURRENT.FIELD)

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
	   IF INSTR(SIATT$(CURRENT.FIELD), CHR$(1)) > 0 THEN
	      COLOR UNPRO, BGCLR
	      PRINT "*";
	      COLOR BGCLR, BGCLR
	   ELSE
	      PRINT W$;
	   END IF
	   RETURN
	END IF

	X$ = STRING$(POZ - (LEN(CT$(CURRENT.FIELD)) + 1), " ")
	CT$(CURRENT.FIELD) = CT$(CURRENT.FIELD) + X$ + W$
	IF INSTR(SIATT$(CURRENT.FIELD), CHR$(1)) > 0 THEN
	   COLOR UNPRO, BGCLR
	   PRINT "*";
	   COLOR BGCLR, BGCLR
	ELSE
	   PRINT W$;
	END IF
	RP.KEY = 0
	RETURN

DISPLAY.HELP.WINDOW:

	CALL DISPLAY.WINDOW(NX, NY, C.SCREEN)
	COLOR 15, 4
	LOCATE 8, 19, 0: PRINT "               Help Window"
	LOCATE 16, 19, 0: PRINT "          Press Any Key To Exit"

	GOSUB READ.HELP.FILE
	CALL HELP.DISPLAY(H1$, H2$, H3$, H4$, H5$)

	DO
	   AA$ = INPUT$(1)
	LOOP WHILE AA$ = ""

	SCREEN , , C.SCREEN, C.SCREEN

	LOCATE NX, NY, 1
	RETURN

READ.HELP.FILE:

	ERR.ROUTINE$ = "READ.HELP.FILE"
	'
	' read help file and display help messages.
	'
	'
	H1$ = ""
	H2$ = ""
	H3$ = ""
	H4$ = ""
	H5$ = ""
	'
	ERROR.CODE = 0
	COLOR 11, 4
	HELP.FILE.NUMBER$ = LTRIM$(STR$(FMT.INDEX))
	HELP.FILE.NAME$ = RAM.DRIVE$ + "HELP-" + HELP.FILE.NUMBER$ + ".DAT"
	OPEN HELP.FILE.NAME$ FOR INPUT AS #6
	IF ERROR.CODE = 53 THEN
	   LOCATE 12, 22, 0
	   PRINT "No help is available for this field"
	   ERROR.CODE = 0
	   CLOSE #6
	   RETURN
	END IF

	R1$ = LTRIM$(STR$(SISFN(CURRENT.FIELD)))

	IF VAL(R1$) > MAX.FIELDS THEN
	   LOCATE 12, 22, 0
	   PRINT "No help is available for this field"
	   CLOSE #6
	   RETURN
	END IF

	DO UNTIL EOF(6)
	   LINE INPUT #6, A$
	   IF LEFT$(A$, 1) = "?" THEN
	      A$ = RIGHT$(A$, LEN(A$) - 1)
	      IF VAL(A$) <> VAL(R1$) THEN
		 GOTO ANOTHER.HELP.RECORD
	      ELSE
		 LINE INPUT #6, H1$
		 LINE INPUT #6, H2$
		 LINE INPUT #6, H3$
		 LINE INPUT #6, H4$
		 LINE INPUT #6, H5$
		 RETURN
	      END IF
	   END IF

ANOTHER.HELP.RECORD:

	LOOP
	RETURN

AUTOEXEC.ROUTINE:

	ERR.ROUTINE$ = "AUTOEXEC.ROUTINE"

	ERROR.CODE = 0
	AUTO.FILE.NAME$ = "AUTOEXEC.CS"
	OPEN AUTO.FILE.NAME$ FOR INPUT AS #6
	IF ERROR.CODE = 53 THEN
	   AUTO.START.FLAG = 0
	   FUNC.KEY.FLAG = 0
	   RETURN
	END IF

	BGCLR = 0
	FGCLR = 0
	UNPRO = 0
	BLINK = 0
	DIMC = 0

	AUTO.START.FLAG = 1
	FUNC.KEY.FLAG = 1
	K = 0
	DO UNTIL EOF(6)
	   LINE INPUT #6, RA$
	   K = K + 1
	   IF K = 2 OR K = 3 THEN
	      FOR J = 1 TO LEN(RA$)
		 W1$ = W1$ + CHR$(ASC(MID$(RA$, J, 1)) XOR 15)
	      NEXT J
	      GOTO NEXT.AUTO.RECORD
	   END IF

	   GOSUB DEFINE.KEY
	   W1$ = W1$ + KEY.STRING$

NEXT.AUTO.RECORD:

	LOOP
	CLOSE #6
	RETURN

ASSIGN.COLORS:

	SELECT CASE RA$
	   CASE "BLACK"
	      COLOR.CODE = 0
	   CASE "BLUE"
	      COLOR.CODE = 1
	   CASE "GREEN"
	      COLOR.CODE = 2
	   CASE "CYAN"
	      COLOR.CODE = 3
	   CASE "RED"
	      COLOR.CODE = 4
	   CASE "MAGENTA"
	      COLOR.CODE = 5
	   CASE "BROWN"
	      COLOR.CODE = 6
	   CASE "WHITE"
	      COLOR.CODE = 7
	   CASE "H.BLACK"
	      COLOR.CODE = 8
	   CASE "H.BLUE"
	      COLOR.CODE = 9
	   CASE "H.GREEN"
	      COLOR.CODE = 10
	   CASE "H.CYAN"
	      COLOR.CODE = 11
	   CASE "H.RED"
	      COLOR.CODE = 12
	   CASE "H.MAGENTA"
	      COLOR.CODE = 13
	   CASE "H.BROWN"
	      COLOR.CODE = 14
	   CASE "H.WHITE"
	      COLOR.CODE = 15
	   CASE ELSE
	      CLS
	      PRINT "Undefined color code :"; RA$
	      GOTO PROGRAM.END
	END SELECT
	RETURN

RESET.COLORS:

	IF COLORS.FLAG = 1 THEN
	   BGCLR = OLD.BGCLR
	   FGCLR = OLD.FGCLR
	   UNPRO = OLD.UNPRO
	   BLINK = OLD.BLINK
	   DIMC = OLD.DIMC
	   COLORS.FLAG = 0
	END IF

	RETURN

DELETE.FORMAT:

	ERR.ROUTINE$ = "DELETE.FORMAT"

	IF FMT.COUNT <= 1 THEN
	   RETURN
	END IF

	FMT.RANGE = INT(FMT.COUNT / 2)

	LEAST.USED = 0
	FORMAT.TO.DELETE = 0

	FOR I = FMT.RANGE TO 1 STEP -1
	   COUNT.USED = VAL(MID$(FORMATS$(I), 9, 2))
	   IF LEAST.USED = 0 THEN
	      LEAST.USED = COUNT.USED
	   END IF
	   IF COUNT.USED <= LEAST.USED THEN
	      LEAST.USED = COUNT.USED
	      FORMAT.TO.DELETE = I
	   END IF
	NEXT I

	IF EXT.ENABLE$ = "Y" THEN
	   CALL COPY.TO.EXT(EXT.DRIVE$, 1, FORMAT.TO.DELETE, FORMAT.TO.DELETE, FORMATS$(), RAM.DRIVE$, ERROR.CODE, IN.ARRAY%(), OUT.ARRAY%(), ax, bx, cx, dx, EXT.MIN.SP#, BGCLR, DISPLAY.MODE$, C.SCREEN)
	END IF

	FORMAT.NAME$ = RTRIM$(LEFT$(FORMATS$(FORMAT.TO.DELETE), 6))
	ERROR.CODE = 0
	KILL RAM.DRIVE$ + FORMAT.NAME$
	IF ERROR.CODE = 53 THEN
	   ERROR.CODE = 0
	END IF
	N$ = LTRIM$(STR$(FORMAT.TO.DELETE))
	FILE.NAME$ = RAM.DRIVE$ + "HELP-" + N$ + ".DAT"
	KILL FILE.NAME$
	IF ERROR.CODE = 53 THEN
	   ERROR.CODE = 0
	END IF
	FILE.NAME$ = RAM.DRIVE$ + "TRIG-" + N$ + ".DAT"
	KILL FILE.NAME$
	IF ERROR.CODE = 53 THEN
	   ERROR.CODE = 0
	END IF
	FORMATS$(FORMAT.TO.DELETE) = ""
	RETURN

ERROR.ROUTINE:
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

PROGRAM.END:

	CLOSE
	END


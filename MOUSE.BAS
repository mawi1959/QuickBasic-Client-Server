'Program:   Master Mouse Routine Library
'           MOUSE.BAS

'$INCLUDE: 'QB.BI'
'$INCLUDE: 'MOUSE.BI'

DEFINT A-Z

DIM SHARED MouseVisible, MHMax, MVMax
DIM SHARED MouseIntr, MHCell, MVCell

SUB ClearButton (Button)
	WHILE NOT GetButtonUpStatus(Button, 0, 0): WEND
END SUB

FUNCTION GetButtonUpStatus (Button, MPosX, MPosY)
	ax = 6
	bx = Button
	OutRegs.AX = 0
	OutRegs.BX = 0
	OutRegs.CX = 0
	OutRegs.DX = 0
	MouseHandler AX, BX, 0, 0
	MPosX = OutRegs.CX \ MHCell + 1
	MPosY = OutRegs.DX \ MVCell + 1
	IF OutRegs.AX = 0 THEN
		GetButtonUpStatus = True
	ELSE
		GetButtonUpStatus = False
	END IF
END FUNCTION

FUNCTION GetMouseStatus (MPosX, MPosY)
	AX = 3
	OutRegs.BX = 0: OutRegs.CX = 0: OutRegs.DX = 0
	MouseHandler AX, 0, 0, 0
	GetMouseStatus = OutRegs.BX
	MPosX = OutRegs.CX \ MHCell + 1
	MPosY = OutRegs.DX \ MVCell + 1
END FUNCTION

SUB MouseHandler (AX, BX, CX, DX)
DIM InRegs AS RegType
	InRegs.AX = AX
	InRegs.BX = BX
	InRegs.CX = CX
	InRegs.DX = DX
	INTERRUPT MouseIntr, InRegs, OutRegs
END SUB

SUB MouseOff
	IF MouseVisible THEN
		MouseHandler 2, 0, 0, 0
		MouseVisible = False
	END IF
END SUB

SUB MouseOn
	IF NOT MouseVisible THEN
		MouseHandler 1, 0, 0, 0
		MouseVisible = True
	END IF
END SUB

FUNCTION MouseReset
  MHMax = 639 'Max virtual horizontal mouse pos
  MVMax = 199 'Max virtual vertical mouse pos
  MHCell = 8  'Mouse horizontal cell width
  MVCell = 8  'Mouse vertical cell height
  MouseIntr = &H33
  MouseHandler 0, 0, 0, 0
  MouseReset = OutRegs.AX
  MouseVisible = False
END FUNCTION

SUB SetMouseSoftCursor_
   (MouseChar, MouseFGColor, MouseBGColor)
	MouseOn
	AX = 10
	BX = 0       'Select software cursor
	CX = &H8800  'Screen Mask Value 
	DX = &H8800 + MouseBGColor * 4096 +_
	     MouseFGColor * 256 + MouseChar
	MouseHandler AX, BX, CX, DX
	MouseOff
END SUB

FUNCTION ThereIsAMouse
	IRET = 207
	DEF SEG = 0    'Set to base system address
	MouseSegment = PEEK(207) * 256 + PEEK(206)
	MouseOffset = PEEK(205) * 256 + PEEK(204)
	IF MouseSegment = 0 AND MouseOffset = 0 THEN
		ThereIsAMouse = False
	ELSE
		DEF SEG = MouseSegment
		MouseInstruction = PEEK(MouseOffset)
		IF MouseInstruction = IRET THEN
			ThereIsAMouse = False
		ELSE
			ThereIsAMouse = True
		END IF
	END IF
	DEF SEG
END FUNCTION

;
;	XSHP.LSP
;
;	- Explodes AutoCAD text or shape entities into polylines.
;	- Shape descriptions are read from ASCII .shp files.
;	- Xshp will explode any text or shape entity except ones
;	  using a Big Font.
;	- Xshp supports widths, oblique angles, thicknesses,
;	  under/over scores and other special characters.
;
;   Copyright (C) 1990-92 by Upper Canada Software, Inc.
;   All rights reserved.
;
;	If you have a font or shape which fools XShp, please send me
;	an example and I'll see what I can do.
;	  Len Switzer
;	  76207,254
;
; Notes:
;  - Xshp assumes the .shp file can be found in the same subdirectory
;    as the .shx file.
;  - If an entity has a width or oblique angle, its arc segments are
;    broken into lines. The # of lines produced is controlled by system
;    variable SplineSegs. For my purposes, it is the number of line
;    segments in a 45 degree arc.
;  - Entity layers and colors are ignored. Plines are created on the current
;    layer with the current entity color and linetype.
;  - Xshp cannot be used transparently.
;

(princ "\nUpper Canada Software, Inc.\nLoading XShp.lsp....")

;
;	Entry point.
;
;	All symbols are local.
;	All subfunctions are prefixed with "XS_".
;

(defun C:XShp ( / SS Flags OldErr Ent Width OblTan Gen Stack Style CurPt ShpFH
		  Scale Above )

  (princ
    "\nSelect text and shapes to explode.")
  (setq SS (ssget)			;Select entities to explode.
	Flags 0)			;Flags is the bitsum of:
					;  01h = AutoCAD Release 10+.
					;  02h = Pen Down.
					;  04h = Vertical shape file.
					;  08h = Temporary flag.

  (if (A_AcadVer 10)			;Is this release 10 or better??
    (setq Flags (logior Flags 1)))

  (setq OldErr *Error*)			;Save and set *Error* handler.
  (defun *Error* ( Str )
    (if ShpFH				;Close file??
      (setq ShpFH (close ShpFH)))
    (setq SS nil			;Explicitely release pickset.
	  *Error* OldErr		;Restore old error handler.
	  OldErr nil)
    (A_SysVar nil)			;Restore all system variables.
    (if (/= Str "quit / exit abort")	;Issue error.
      (princ
	(strcat "\n oops... ERROR: " Str)))
    (princ))

  (A_SysVar				;Save and set system variables.
    '(("CmdEcho" . 0) ("BlipMode" . 0) ("UcsIcon" . 0)
      ("Thickness" . 0) ("Elevation" . 0)))

  (while				;Process selected entities.
    (progn
      (while				;Loop to filter unwanted entities.
	(cond
	  ((null			;No more entities??
	     (setq Ent (ssname SS 0)))
	    nil)
	  ((not				;Not TEXT or SHAPE??
	     (member
	       (cdr
		 (assoc 0 (setq Ent (entget Ent))))
	       '("TEXT" "SHAPE")))
	    (ssdel			;Remove ent from pickset.
	      (cdr (assoc -1 Ent)) SS))))
      Ent)				;Found TEXT or SHAPE??

    (ssdel (cdr (assoc -1 Ent)) SS)	;Remove Ent from pickset.
    (setq Width (cdr (assoc 41 Ent))	;Initialize constants.
	  OblTan (/ (sin (cdr (assoc 51 Ent))) ;Tangent of oblique angle.
		   (cos (cdr (assoc 51 Ent))))
	  Gen (if (assoc 71 Ent)	;Gen = X and Y mirror operations.
		(list
		  (if (eq (logand (cdr (assoc 71 Ent)) 2) 2)
		    '-			;Mirror vertically.
		    '+)
		  (if (eq (logand (cdr (assoc 71 Ent)) 4) 4)
		    '-			;Mirror horizontally.
		    '+))
		'(+ +))			;Default generation = Do not mirror.
	  Stack nil			;Used for push/pop current location.
	  Style nil			;Clear last entity's Style.
	  Scale nil)			;Clear last entity's Scale.

    (command)				;XShp cannot be used transparently.

    (if (eq (logand Flags 1) 1)		;Rel.10+??
      (progn				;Set UCS to entity.
	(command "_UCS" "_E" (cdr (assoc -1 Ent)))
	(setq CurPt '(0.0 0.0)))
      (setq CurPt (cdr (assoc 10 Ent)))) ;Init CurPt to entity's start point.
    (entdel (cdr (assoc -1 Ent)))	;Delete entity.
    (setvar "Thickness"			;Set Thickness to entity's.
      (if (assoc 39 Ent)
	(cdr (assoc 39 Ent))
	0.0))

    (if (eq (cdr (assoc 0 Ent)) "SHAPE") ;Handle TEXT & SHAPES differently.

      (XS_DrawChar			;Call XS_DrawChar with name of shape.
	(cdr (assoc 2 Ent)))

      (XS_DrawText))			;Loop for each char in string.

    (command)
    (if (eq (logand Flags 1) 1)		;If R.10+, restore UCS.
      (command "_UCS" "_P")))		;Loop for next entity.

  (*Error* "quit / exit abort"))	;Close ShpFH, restore sysvars and exit.

;
;	Explode each character in text entity.
;
;	Unlike shapes, text entities can consist of more than one character.
;	As well, text strings must be parsed for special characters like
;	%%O, %%D and %%128.
;
;	External:
;	  Ent		- Text entity's data list.
;	  CurPt		- Text start point in current UCS.
;
;	Local:
;	  Score		- As %%O and %%U chars are found, the pen's current
;			  baseline position is recorded in Score.
;			  After all chars are drawn, Score is used to draw
;			  any over and under scores.
;

(defun XS_DrawText ( / Score Text Char Str Cnt )
  (setq Text (cdr (assoc 1 Ent)))
  (while (/= Text "")			;Loop for each character.
    (princ "\n")
    (if (/= (substr Text 1 2) "%%")	;Not special char??

      (progn
	(XS_DrawChar (ascii Text))	;Draw single ASCII char.
	(setq Text (substr Text 2)))	;Trim text string.

      (if
	(cond
	  ((null			;Never true.
	     (setq Char (strcase (substr Text 3 1)) ;Grab code char.
		   Text (substr Text 4)))) ;Trim text string.

	  ((eq Char "O")		;Overscore??
	    (setq Score (cons		;Add CurPt to Score.
			  (cons T (car CurPt))
			  Score)
		  Char nil))		;Don't draw Char.

	  ((eq Char "U")		;Underscore??
	    (setq Score (cons		;Add CurPt to Score.
			  (cons nil (car CurPt))
			  Score)
		  Char nil))		;Don't draw Char.

	  ((member Char '("D" "P" "C"))	;Special char??
	    (setq Char			;Set Char to special code.
	      (cadr
		(assoc Char
		  '(("D" 127)("P" 128)("C" 129))))))

	  ((and				;Digit??
	     (> (ascii Char) 47)	;> "0"-1??
	     (< (ascii Char) 58))	;< "9"+1??
	    (setq Str ""
		  Text (strcat Char Text)
		  Cnt 0)
	    (while			;Find end of digits.
	      (and
		(< (setq Cnt (1+ Cnt)) 4) ;Refuse more than 3 digits.
		(not			;Not end of string??
		  (zerop
		    (setq Char
		      (ascii
			(substr Text Cnt 1)))))
		(> Char 47)		;> "0"-1??
		(< Char 58)))		;< "9"+1??
	    (setq Char (atoi (substr Text 1 (1- Cnt)))
		  Text (substr Text Cnt)))

	  (T))				;Draw unknown escaped char. (ie. "%%%")

	(XS_DrawChar Char))))		;Draw single ASCII char.

  (if Score				;Any %%O or %%U chars found??
    (XS_DrawScores)))			;Draw over/under scores.

;
;	(XS_DrawChar <ASCII# or "Shape name"> )
;
;	Opens .shp file, locates character description, and draws character.
;
;	External:
;	  Flags		- Uses all flags.
;	  Scale		- Entity height / Font's Above scale.
;			  ( Set in XS_GetShpLst. )
;	  Stack		- Stack of current location pushes & pops
;	  CurPt		- Current pen location.
;
;	Local:
;	  ShpLst	- List of integer shape description bytes.
;

(defun XS_DrawChar ( Char / ShpLst ShpCode Temp Temp2
			    Radius Ang1 Ang2 Count)

  (princ "\nXS_DrawChar")
  (princ "\nXS_DrawChar")

  (setq ShpLst (XS_GetShpLst Char)	;Get list of shape codes for Char.
	Flags (logior Flags 2))		;Pen is down by default.

  (command)
  (command "_Pline" CurPt)		;Leave pline command active.


  (while ShpLst				;Loop for each command in shape.
    (setq ShpCode (car ShpLst)		;Grab next shape code.
	  ShpLst (cdr ShpLst))		;Trim list.
    (if (> ShpCode 15)			;Length & direction in one byte??

      (progn
	(setq Temp (rem ShpCode 16)	;Direction.
	      Temp2 (lsh ShpCode -4))	;Length.
	(XS_DispPt			;Displace CurPt with vector.
	  (list
	    (* Temp2			;Length * X Direction.
	      (nth Temp
		'(1.0 1.0 1.0 0.5 0.0 -0.5 -1.0 -1.0
		  -1.0 -1.0 -1.0 -0.5 0.0 0.5 1.0 1.0)))
	    (* Temp2			;Length * Y Direction.
	      (nth Temp
		'(0.0 0.5 1.0 1.0 1.0 1.0 1.0 0.5 0.0
		  -0.5 -1.0 -1.0 -1.0 -1.0 -1.0 -0.5))))))

      ;ShpCode is a shape command from 0 to 15.

      ((nth ShpCode (list		;Evaluate ShpCode's handler.

(lambda ()				;0 = End of shape
  (command))

(lambda ()				;1 = Pen down
  (setq Flags (logior Flags 2))
  (command)
  (command "_Pline" CurPt))

(lambda ()				;2 = Pen up
  (setq Flags (logand Flags -3))
  (command))

(lambda ()				;3 = Divide scale
  (setq Scale (/ Scale (car ShpLst))
	ShpLst (cdr ShpLst)))

(lambda ()				;4 = Multiply scale
  (setq Scale (* Scale (car ShpLst))
	ShpLst (cdr ShpLst)))

(lambda ()				;5 = Push current location
  (setq Stack (cons CurPt Stack)))

(lambda ()				;6 = Pop location
  (if Stack
    (progn
      (setq CurPt (car Stack)
	    Stack (cdr Stack))
      (if (eq (logand Flags 2) 2)	;Pen down??
	(progn
	  (command)
	  (command "_Pline" CurPt))))
    (princ
      "\n**More pops than pushes.")))

(lambda ()				;7 = Draw subshape.
  (princ "  subshape...\n")
  (XS_DrawChar (car ShpLst))		;Recursively draw subshape.
  (setq ShpLst (cdr ShpLst)))

(lambda ()				;8 = XY displacement
  (XS_DispPt
    (list (car ShpLst) (cadr ShpLst)))
  (setq ShpLst (cddr ShpLst)))

(lambda ()				;9 = Multiple XY displacements
  (while (not				;Loop until 0,0 displacement.
	   (and
	     (zerop (car ShpLst))
	     (zerop (cadr ShpLst))))
    (XS_DispPt
      (list (car ShpLst) (cadr ShpLst)))
    (setq ShpLst (cddr ShpLst)))
  (setq ShpLst (cddr ShpLst)))

(lambda()				;10 = Octant arc
  (setq Radius (car ShpLst)		;Arc's radius in shape units.
	Ang1 (cadr ShpLst)		;Direction/Start octant/Octant span.
	Flags (if (minusp Ang1)		;Negative start angle??
		(logior Flags 8)	;Set Temp flag.
		(logand Flags -9))	;Clear Temp flag.
	ShpLst (cddr ShpLst)
	Count (logand (abs Ang1) 7)	;Number of octants spanned.
	Ang1 (* (lsh (abs Ang1) -4) (/ pi 4)) ;Start angle in degrees.
	Ang2(if (zerop Count)		;End angle in degrees.
	      Ang1
	      (apply
		(if (zerop (logand Flags 8)) '+ '-)
		(list
		  Ang1
		  (* Count (/ pi 4))))))
  (XS_DrawArc))

(lambda ()				;11 = Fractional arc
  (setq Temp (car ShpLst)		;Start offset.
	Temp2 (cadr ShpLst)		;End offset.
	Radius (+ (lsh (caddr ShpLst) 8) ;Add Radius' high & low bytes.
		 (cadddr ShpLst))
	ShpLst (cddddr ShpLst)
	Ang1 (car ShpLst)		;Direction/Start octant/Octant span.
	ShpLst (cdr ShpLst)
	Flags (if (minusp Ang1)		;Negative angle??
		(logior Flags 8)	;Set Temp flag.
		(logand Flags -9))	;Clear Temp flag.
	Temp (/ (* Temp 45.0) 256)	;Start offset in degrees.
	Temp2 (/ (* Temp2 45.0) 256)	;End offset in degrees.
	Count (logand (abs Ang1) 7)	;Number of octants spanned.
	Ang1 (* (lsh (abs Ang1) -4) (/ pi 4)) ;Start angle in degrees.
	Ang2 (apply			;End angle in degrees.
	       (if (zerop (logand Flags 8)) '+ '-)
	       (list Ang1
		 (*
		   (if (zerop Count)
		     7
		     (if (zerop Temp2)
		       Count
		       (1- Count)))
		   (/ pi 4))))
	Ang1(apply			;Start angle plus start offset.
	      (if (zerop (logand Flags 8)) '+ '-)
	      (list Ang1 (* (/ Temp 180) pi)))
	Ang2(apply			;End angle plus end offset.
	      (if (zerop (logand Flags 8)) '+ '-)
	      (list Ang2 (* (/ Temp2 180) pi))))
  (XS_DrawArc))

(lambda ()				;12 = Bulge arc
  (XS_DrawBulge))

(lambda ()				;13 = Multiple bulge arcs
  (while (not				;Loop until (0 0) bulge.
	   (and
	     (zerop (car ShpLst))
	     (zerop (cadr ShpLst))))
    (XS_DrawBulge))
  (setq ShpLst (cddr ShpLst)))

(lambda ()				;14 = Vertical command.
  (if (zerop (logand Flags 4))		;If text isn't vertical, skip next cmd.
    (cond				;# of bytes to skip depends on code.
      ((null
	 (setq ShpCode (car ShpLst)
	       ShpLst (cdr ShpLst))))
      ((member ShpCode '(3 4 7))
	(setq ShpLst (cdr ShpLst)))
      ((member ShpCode '(8 10 12))
	(setq ShpLst (cddr ShpLst)))
      ((eq ShpCode 11)
	(setq ShpLst (cddddr ShpLst)))
      ((member ShpCode (list 9 13))
	(while
	  (and
	    (not
	      (and
		(zerop (car ShpLst))
		(zerop (cadr ShpLst))))
	    (setq ShpLst (cddr ShpLst)))
	  (if (eq ShpCode 13)
	    (setq ShpLst (cdr ShpLst))))))))

(lambda()))))))				;15 = Illegal cmd.
					;Loop for next shape code.

  (if ShpFH				;Close file & exit.
    (setq ShpFH (close ShpFH))))

;
;	Displace CurPt by offset passed to XS_DispPt.
;
;	Displacement is scaled by entity's width and current shape scale,
;	mirrored by entity's generation flags, modified by entity's
;	oblique angle, and possibly rotated by entity's rotation angle.
;
;	If the shape "pen" is currently down, a new pline segment is passed
;	to acad, else the penup segment is shown with an xor line.
;	(I've had several comments of confusion about the penup xor lines.
;	 Too bad, I like it. <g>)
;
;	External:
;	  CurPt		- Current shape location.
;	  Scale		- Current shape scale.
;	  Width		- Entity's width.
;	  Gen		- Entity's generation flags.
;	  OblTan	- Tangent of entity's oblique angle.
;	  Flags		- Acad R.10 and pen down flags.
;
;	Modifies:
;	  CurPt
;

(defun XS_DispPt ( Pt )

  (princ "\nXS_DispPt")
  (princ "\nXS_DispPt")
  (setq Pt				;Scale offset by Scale and Width.
    (mapcar '*
      Pt
      (list (* Scale Width) Scale)))

  (setq	Pt				;Displace CurPt by mirrored offset.
    (mapcar
      '(lambda ( Sym Pt1 Pt2 )
	 (apply Sym (list Pt1 Pt2)))
      Gen
      CurPt
      (list
	(+ (car Pt) (* OblTan (cadr Pt))) ;Apply oblique angle to offset.
	(cadr Pt))))

  (if (zerop (logand Flags 1))		;If UCS is not set to entity...
    (setq Pt				;Rotate offset by ent's rotation angle.
      (polar CurPt (cdr (assoc 50 Ent)) (distance CurPt Pt))))

  (if (zerop (logand Flags 2))		;Is the shape pen up??
;    (progn
;      (command "_Color" "_red")
;      (command "_PLine" CurPt Pt) (command)
;      (command "_Color" "_bylayer")
;    )
    (grdraw CurPt Pt -1)		;Draw xor penup line.
    (command Pt))			;Add new pline segment.

  (princ " \010")			;Force display on ADI 4.0/Acad386.
  (setq CurPt Pt))			;Set new CurPt.

;
;	(XS_GetShpLst <ASCII # or "Shape Name"> )
;
;	Reads .shp file & returns list of shape cmds for Char.
;
;	External:
;	  Ent		- Text or shape entity data.
;	  Style		- If nil, init Style, Above, Scale.
;
;	Modifies:
;	  Style		- Font spec in group 3 is set to legal file spec.
;	  Above		- Font scale.
;			  Read from text font files for TEXT.
;			  Set to 1.0 for SHAPES.
;	  Scale		- Init to (/ <Ent height> Above).
;			  Subsequently modified by mult or divide scale cmds.
;

(defun XS_GetShpLst ( Char / ShpFH Str Cnt )

  (if (and
	(eq (cdr (assoc 0 Ent)) "TEXT")	;TEXT entity??
	(null Style)			;First pass for this entity??
	(null (XS_InitFont)))		;Open file & search for Above scale.

    (cond				;Error.
      ((null ShpFH)			;Can't open file??
	(princ
	  (strcat
	    "\n**Can't open " (cdr (assoc 3 Style))))
	(setq Style nil))

      ((null Str)			;Can't find font's Above byte??
	(setq ShpFH (close ShpFH))
	(princ
	  (strcat
	    "\n**Can't find \042*0\042 character in " (cdr (assoc 3 Style))))
	(setq Style nil))))

  (if (and Style			;Was file opened OK??
	(numberp Char))
    (XS_FindShpNum)			;Search .shp file for shape number.
    (XS_FindShpName))			;Search .shp file for shape name.

  (if (and Style ShpFH Str)		;Did we find shape description??

    (progn
      (setq Cnt (XS_GetByte)		;Number of bytes in shape description.
	    Str "")			;Init Str for XS_GetByte.
      (if (null Scale)			;Not subshape??
	(setq Scale (/ (cdr (assoc 40 Ent)) Above))) ;Init Scale.
      (repeat (1- Cnt)			;Grab byte commands. Skip trailing "0".
	(setq ShpLst			;Add a byte to ShpLst.
	  (cons (XS_GetByte) ShpLst)))
      (setq ShpFH (close ShpFH))	;Close file.
      (reverse ShpLst))			;Return shape description bytes.

    (progn				;Error.
      (entdel (cdr (assoc -1 Ent)))	;Undelete old entity.
      nil)))				;Return nil.

;
;	Try to open .shp file and find "*0" character.
;	Return T on success, else nil.
;

(defun XS_InitFont ()
  (and
    (setq Style				;Legitimate style name??
      (tblsearch "Style" (cdr (assoc 7 Ent))))
    (setq Str (cdr (assoc 3 Style))) 	;Style's font file.
    (setq Style				;Force .shp file extension.
      (subst
	(cons
	  3
	  (strcat
	    (if (and
		  (> (strlen Str) 4)
		  (eq "."		;File extension??
		    (substr Str (- (strlen Str) 3) 1)))
	      (substr Str 1 (- (strlen Str) 4))
	      Str)
	    ".SHP"))
	(assoc 3 Style)
	Style))
    (setq Str				;Try to open file.
      (XS_OpenFile (cdr (assoc 3 Style))))
    (setq ShpFH (open Str "r"))
    (setq Style
      (subst
	(cons 3 Str)
	(assoc 3 Style)
	Style))
    (progn				;Look for special "*0" font char.
      (while
        (and
	  (setq Str
	    (read-line ShpFH))
	  (or				;Not "*0" character??
	    (/= (ascii Str) 42)		;42 = (ascii "*")
	    (/= (atoi (substr Str 2)) 0))))
      Str)				;Found special char??
    (setq Str (strcase (read-line ShpFH)))
    (null
      (setq Above (Float (XS_GetByte))	;Font height.
	    Flags (logior		;Set vertical font flag from Style.
		    (logand Flags -5)
		    (logand (cdr (assoc 70 Style)) 4))
	    ShpFH (close ShpFH)))))

;
;	Search .shp file for ASCII number.
;

(defun XS_FindShpNum ()
  (setq ShpFH				;Open file.
    (open (cdr (assoc 3 Style)) "r"))
  (while				;Look for "*#" character header.
    (and
      (progn
	(while				;Look for "*" prefix.
	  (and
	    (setq Str (read-line ShpFH))
	    (/= (ascii Str) 42)))
	Str)
      (setq Str (strcase (substr Str 2)))
      (princ "\r")
      (/= (princ (XS_GetByte)) Char)	;Not our character??
      (princ "     ")))
  (princ "     ")
  (if (null Str)			;Didn't find character??
    (princ
      (strcat
	"\n**Character \042" (chr Char) "\042 not found in "
	(strcase (cdr (assoc 3 Style)))))))

;
;	Search .shp file for shape name.
;	Since shape entities store only the shape name, and not the .shp
;	file's name, each .shp file referenced by a shape must be checked.
;

(defun XS_FindShpName ()
  (setq Style (tblnext"Style"T)		;Check first style.
	Above 1.0)			;Unlike fonts shapes have no "*0" char.
  (while				;Loop through each style.
    (and
      (progn
	(while
	  (and
	    (or
	      (zerop			;Not shape's style??
		(logand (cdr (assoc 70 Style)) 1))
	      (eq (cdr (assoc 3 Style)) "")) ;Invalid file name??
	    (setq Style (tblnext "Style")))) ;Check next style.
	Style)
      (setq Str				;Try to open file.
	(XS_OpenFile
	  (strcat
	    (cdr (assoc 3 Style)) ".SHP")))
      (setq ShpFH			;Open file.
	(open Str "r"))
      (setq Style			;Record file name in Style.
	(subst
	  (cons 3 Str)
	  (assoc 3 Style)
	  Style))
      (while				;Search .shp file for shape name.
	(and
	  (progn
	    (while			;Loop for shape header "*" prefix.
	      (and
		(setq Str (read-line ShpFH))
		(/= (ascii Str) 42)))
	    Str)
	  (setq Cnt 2)
	  (progn
	    (while			;Search past ASCII number.
	      (not
		(member
		  (substr Str (setq Cnt (1+ Cnt)) 1)
		  '(","""))))
	    T)
	  (setq Str (substr Str (1+ Cnt))
		Cnt 0)
	  (progn
	    (while			;Search past defbytes.
	      (not
		(member
		  (substr Str (setq Cnt (1+ Cnt)) 1)
		  '(","""))))
	    T)
	  (princ "\r")
	  (/=				;Not our shape name??
	    (princ (strcase (substr Str (1+ Cnt))))
	    (cdr (assoc 2 Ent)))
	  (princ "          ")))))	;Loop back to while.
  (princ "          ")
  (cond
    ((null ShpFH)			;Can't open file??
      (princ
	(strcat
	  "\n**Can't open " (strcase (cdr (assoc 3 Style))))))
    ((null Style)			;Can't find shape name??
      (princ
	(strcat
	  "\n**Shape " (cdr (assoc 2 Ent)) " not found in shape files.")))))

;
;	(XS_OpenFile <"FileSpec"> )
;
;	Try to open FileSpec.
;	If not successful, search AcadPrefix paths.
;	If not successful, prompt user to enter new path or file spec.
;	Return name used to open file, or nil.
;

(defun XS_OpenFile ( FileNm / Str Cnt FileHdl )
  (while
    (null
      (cond
	((setq FileHdl			;No problem opening file??
	   (open FileNm "r")))

	((and
	   (eq (logand Flags 1) 1)	;Acad R.10+??
	   (setq Str (findfile FileNm))	;Let findfile try.
	   (setq FileNm Str)
	   (setq FileHdl (open FileNm "r"))))

	((and				;OK to prefix spec with library path??
	   (/= (substr FileNm 2 1) ":") ;No drive??
	   (/= (ascii FileNm) 92)	;No leading backslash??
	   (setq Str (getvar"AcadPrefix"))
	   (while (/= Str "")
	     (setq Cnt 0)
	     (while
	       (not
		 (member
		   (substr Str (setq Cnt (1+ Cnt)) 1)
		   '(";" ""))))
	     (if (setq FileHdl
		   (open
		     (strcat
		       (substr Str 1 (1- Cnt))
		       FileNm)
		     "r"))
	       (setq FileNm (strcat
			      (substr Str 1 (1- Cnt))
			      FileNm)
		     Str "")
	       (setq Str (substr Str (1+ Cnt)))))
	   FileHdl))		;Found in library paths??

	((eq				;Exit if user enters "".
	   (setq Str			;Prompt user for new path or spec.
	     (getstring
	       (strcat "\nFile not found " FileNm
	         "\nEnter path or new file spec: ")))
	   ""))

	((setq FileNm
	   (if (eq (substr Str (strlen Str)) "\\") ;New path for FileNm??
	     (strcat Str FileNm)
	     Str))
	  nil))))			;Loop again.

  (if FileHdl				;Return FileNm or nil.
    (progn
      (close FileHdl)
      FileNm)))

;
;	Parse a number from .SHP file. Convert from hex if needed.
;
;	External:
;	  Str		- Current line being parsed.
;			  When Str = "", next line is read.
;

(defun XS_GetByte ( / Cnt Cnt2 Sum NumBit )

  (while
    (cond
      ((and (eq Str "")			;Get next line in file??
	 (setq Str (read-line ShpFH)))
	(setq Str (strcase Str)))

      ((null Str)			;End of file??
	(setq Cnt 0)			;Return 0.
	nil)				;Exit while loop.

      ((and				;Trim leading crap.
	 (while
	   (not
	     (or
	       (eq Str "")
	       (eq (ascii Str) 45)	;Minus sign??
	       (eq (ascii Str) 43)	;Plus sign??
	       (and			;Digit??
		 (> (ascii Str) 47)	;> "0"-1
		 (< (ascii Str) 58))	;< "9"+1
	       (and			;Hex digit??
		 (> (ascii Str) 64)	;> "A"-1
		 (< (ascii Str) 71))	;< "F"+1
	       (eq (ascii Str) 59)))	;59 = (ascii ";")
	   (setq Str (substr Str 2)))	;Trim one char from Str.
	 nil))

      ((eq Str ""))			;End of string??

      ((eq (ascii Str) 59)		;Semi-colon??
	(setq Str ""))			;Skip rest of string.

      ((setq Cnt 0)			;Scan past digits.
	(while
	  (and
	    (/= (substr Str (setq Cnt (1+ Cnt)) 1) "")
	    (setq Cnt2			;ASCII value of character.
	      (ascii (substr Str Cnt)))
	    (or
	      (eq Cnt2 45)		;Minus sign??
	      (eq Cnt2 43)		;Plus sign??
	      (and			;Digit??
		(> Cnt2 47)
		(< Cnt2 58))
	      (and			;Hex digit??
		(> Cnt2 64)
		(< Cnt2 71)))))

	(if (or
	      (and			;Hex number??
	        (eq (ascii Str) 48)	;Leading "0"??
	        (> Cnt 2)		;More than one digit??
		(setq Str (substr Str 2))) ;Trim leading "0".
	      (and
		(member (ascii Str) '(43 45)) ;Sign on hex number??
		(eq (substr Str 2 1) "0") ;Leading "0"??
		(> Cnt 3)		;More than one digit??
		(setq Str		;Trim leading "0".
		  (strcat
		    (substr Str 1 1)
		    (substr Str 3)))))

	  (progn
	    (if (eq (ascii Str) 43)	;Leading "+"??
	      (setq Str (substr Str 2)
		    Cnt (1- Cnt)))
	    (if (eq (ascii Str) 45)	;Leading "-"??
	      (setq Str (substr Str 2)
		    Cnt (1- Cnt)
		    Cnt2 -1)
	      (setq Cnt2 1))
	    (setq Sum 0			;Init hex sum.
		  NumBit (lsh (- Cnt 3) 2)) ;Numbit = shift count.
 	    (repeat (- Cnt 2)
	      (setq Sum			;Add first hex digit to Sum.
		(+ Sum
		  (* (-
		       (ascii Str)
		       (if (< (ascii Str) 58)
			 48
			 55))
		    (expt 2 NumBit))))
	      (setq Str (substr Str 2)
		    NumBit (- NumBit 4)))
	    (setq Sum (* Sum Cnt2))) ;Return Sum.

	  (setq Sum (atoi (substr Str 1 (1- Cnt)))
		Str (substr Str Cnt)))

	nil)))				;Exit while loop.

  Sum)					;Return 8-bit number.

;
;	Draw arc.
;
;	If entity has Width /= 0.0 or OblTan /= 0.0, elliptical arc is
;	drawn as a series of line segments. The number of lines produced
;	is controlled by system variable SplineSegs. For my purposes, it
;	is the number of line segments in a 45 degree arc.
;
;	Circular arcs are drawn with polyarcs.
;
;	External:
;	  Radius	- Radius of arc.
;	  Ang1		- Angle from arc center to start point.
;	  Ang2		- Angle from arc center to end point.
;	  Count		- Number of octants spanned.
;	  Flags		- Plus/minus direction of arc.
;	  OblTan	- Tangent of entity's oblique angle.
;	  Width		- Entity's width scale.
;

(defun XS_DrawArc ( / Center Span Pt1 Pt2 Inc )
  (princ "\nXS_DrawArc")
  (princ "\nXS_DrawArc")
  (setq Center				;Center of arc.
    (polar '(0 0) (+ Ang1 pi) Radius))

  (setq Span				;Span of arc in radians.
    (if (and				;360 degree arc??
	  (zerop Count)
	  (equal Ang1 Ang2 1.0e-9))
      (* pi 2)
      (*
	(/
	  (if (< Ang1 Ang2)
	    (if (zerop (logand Flags 8))
	      (- Ang2 Ang1)
	      (* (+ (- (* pi 2) Ang2) Ang1) -1))
	    (if (zerop (logand Flags 8))
	      (+ (- (* pi 2) Ang1) Ang2)
	      (* (- Ang1 Ang2) -1)))
	  pi)
	180.0)))

  (cond
    ((zerop (logand Flags 2))		;Is the pen up??
      (XS_DispPt			;Skip arc stuff.
	(polar Center Ang2 Radius)))

    ((and (zerop OblTan) (eq Width 1.0)) ;Arc not elliptical??
      (if (equal Span (* pi 2.0) 1.0e-6) ;360 degree arc??
	(progn				;Draw first 180 degrees.
	  (command "_A" "_CE")		;Polyarc CEnter.
	  (XS_DispPt Center)		;Center.
	  (XS_DispPt Center)		;End point.
	  (command "_CE")		;Finish arc.
	  (setq Ang1 (- Ang1 pi))	;Reverse Ang1 direction.
	  (XS_DispPt			;Center.
	    (polar '(0 0) (+ Ang1 pi) Radius))
	  (XS_DispPt			;End point.
	    (polar '(0 0) Ang2 Radius)))
	(progn
	  (command "_A" "_A" Span)	;Polyarc Angle.
	  (XS_DispPt
	    (polar Center Ang2 Radius))))
      (command "_L"))			;Back to default pline prompt.

    (T					;Draw elliptical arc with lines.
      (setq Cnt (1+			;Number of segments to draw.
		  (fix
		    (*
		      (/ (abs Span) (/ pi 4))
		      (getvar "SplineSegs"))))
	    Inc (/ Span Cnt)		;Span of each segment in radians.
	    Pt1 '(0.0 0.0))
      (repeat Cnt			;Draw each segment.
	(XS_DispPt			;Pass point to acad.
	  (mapcar
	    '-
	    (setq Pt2
	      (polar
		Center
		(setq Ang1 (+ Ang1 Inc))
		Radius))
	    Pt1))
	(setq Pt1 Pt2)))))		;Advance to next segment.

;
;	Draw bulge arc.
;

(defun XS_DrawBulge ( / XDisp YDisp Temp Count Dist Hgt Center )
  (Princ "\n XS_DrawBulge")
  (Princ "\n XS_DrawBulge")
  (setq XDisp (car ShpLst)		;X displacement of end point.
	YDisp (cadr ShpLst)		;Y displacement of end point.
	Temp (caddr ShpLst)		;Bulge factor.
	ShpLst (cdddr ShpLst)
	Flags (if (minusp Temp)		;Negative direction??
		(logior Flags 8)	;Set Temp Flag.
		(logand Flags -9))	;Clear Temp Flag.
	Temp (abs Temp)
	Count 1)
  (cond
    ((or
       (zerop Temp)			;No bulge?? (Straight line.)
       (zerop (logand Flags 2)))	;Pen up??

      (XS_DispPt (list XDisp YDisp)))	;Skip arc stuff.

    ((and
       (zerop XDisp)
       (zerop YDisp)))

    (T
      (setq Dist (distance '(0 0)	;Length of chord.
		   (list XDisp YDisp))
	    Hgt (/ (* Dist Temp) 254)	;Height of bulge in shape units.
	    Center (polar		;Center of arc.
		     (list (/ XDisp 2.0) (/ YDisp 2.0))
		     (apply
		       (if (zerop (logand Flags 8)) '+'-)
		       (list
			 (angle '(0 0) (list XDisp YDisp))
			 (/ pi 2)))
		     (/ (-
			  (expt Dist 2)
			  (*
			    (expt Hgt 2)
			    4))
		       (* Hgt 8)))
	    Ang1 (angle Center '(0 0))	;Angle from Center to start point.
	    Ang2 (angle Center (list XDisp YDisp)) ;Angle from Center to endpt.
	    Radius (distance Center '(0 0)) ;Arc's Radius.
	    Count 1)
      (XS_DrawArc))))

;
;	Draw under and over score lines for text entity.
;
;	As under and over score characters were found in XS_DrawText, their
;	CurPt locations were recorded in Score. After the entire text string
;	has been drawn, Score is now used to generate a series of over and
;	under score pline segments.
;
;	External:
;	  Score		- Stack of over/under score location points.
;	  CurPt		- Location of end of text string.
;

(defun XS_DrawScores ( / Lst )

  (setq Lst '(nil nil))			;Lst = (<Leading %%O> <Leading %%U>)

  (foreach Pt Score			;Foreach %%O and %%U char...
    (if (car Pt)			;Overscore??

      (if (car Lst)			;Already got leading %%O??
	(progn
	  (XS_DrawScore			;Draw overscore.
	    (car Lst) (cadr Pt) T)
	  (setq Lst			;Remove leading %%O from Lst.
	    (list nil (cadr Lst))))
	(setq Lst			;Add leading %%O to Lst.
	  (list (cdr Pt) (cadr Lst))))

      (if (cadr Lst)			;Already got leading %%U??
	(progn
	  (XS_DrawScore			;Draw underscore.
	    (cadr Lst) (cadr Pt) nil)
	  (setq Lst			;Remove leading %%U from Lst.
	    (list (car Lst) nil)))
	(setq Lst			;Add leading %%U to Lst.
	  (list (car Lst) (cdr Pt))))))

  (if (car Lst)				;%%O yet to be drawn??
    (XS_DrawScore		 	;Draw overscore to end of text.
      (car Lst) (car CurPt) T))

  (if (cadr Lst)			;%%U yet to be drawn??
    (XS_DrawScore			;Draw underscore to end of text.
      (cadr Lst) (car CurPt) nil)))

;
;	(XS_DrawScore <Start offset> <End offset> <True if overscore> )
;
;	Draw over/under score.
;
;	Call with start and end offsets along text baseline, and T or nil
;	for over or under score.
;
;	External:
;	  Flags		- Bit 1 flags PenDown mode.
;	  Ent		- TEXT entity data with group 40 height.
;	  Above		- Font file scale.
;

(defun XS_DrawScore ( Pt1 Pt2 Mode / CurPt )
  (command)
  (command "_Pline")			;Start pline command.

  (setq Flags (logior Flags 2)		;Set PenDown.
	CurPt (list Pt1 0))		;CurPt = Pt1 units along baseline.

  (XS_DispPt				;Draw pline start point.
    (list
      (/ (* (cdr (assoc 40 Ent)) -0.15)
	(/ (cdr (assoc 40 Ent)) Above))
      (/ (* (cdr (assoc 40 Ent)) (if Mode 1.2 -0.2))
	(/ (cdr (assoc 40 Ent)) Above))))

  (setq CurPt (list Pt2 0))		;CurPt = Pt2 units along baseline.
  (XS_DispPt				;Draw pline end point.
    (list
      (/ (* (cdr (assoc 40 Ent)) -0.15) Scale)
      (/ (* (cdr (assoc 40 Ent)) (if Mode 1.2 -0.2)) Scale)))

  (command ""))				;End pline segment.


;
;	(A_AcadVer <Number> )
;
;	Returns T if <Number> >= Acad version.
;

(defun A_AcadVer ( Cnt / Temp )
;  (setq Temp (getvar "AcadVer")) 
  (setq Temp (GetAcadVersion)) 
  (while (zerop (atof Temp))
    (setq Temp (substr Temp 2)))
  (>= (atof Temp) Cnt))


;
;	A_SysVar is system variable utility.
;	It may be used in several different ways, determined by the parameter.
;
;	A_SysVar maintains a global symbol called *A_SysVar*, which is a
;	list of system variable names and their values. Each sysvar may be
;	saved and set multiple times, with each current value being push
;	onto a stack in *A_SysVar*.
;
;	 Usage
;	~~~~~~~
;	(A_SysVar '(("SysVar1" . Value1) ("SysVar2" . Value2) ... ))
;
;	 - Save and set system variable(s).
;
;	(A_SysVar '(("SysVar1") ("SysVar2") ... ))
;
;	 - Restore sysvars to oldest values.
;
;	(A_SysVar '("SysVar1" "SysVar2" ... ))
;	  or
;	(A_SysVar "SysVar1")
;
;	 - Restore sysvars to newest values.
;	 - The second form of parameter may only be used with single system
;	   variables.
;
;	Restore and Set operations may be freely mixed in parameter list.
;
;	(A_SysVar nil)
;
;	 - Restore all sysvars in A_SysLst to oldest values.
;

(defun A_SysVar ( Lst / Temp )
  (if (eq (Type Lst) 'STR)
    (setq Lst
      (list Lst)))
  (if Lst
    (foreach Sym Lst
      (cond
	((and (listp Sym)
	   (cdr Sym))
	  (setq Temp
	    (getvar (car Sym)))
	  (setvar (car Sym)
	    (if (listp (cdr Sym))
	      (cadr Sym)
	      (cdr Sym)))
	  (if (assoc (strcase (car Sym)) *A_SysLst*)
	    (setq *A_SysLst*
	      (subst
		(cons
		  (strcase (car Sym))
		  (cons
		    Temp
		    (cdr
		      (assoc (strcase (car Sym)) *A_SysLst*))))
		(assoc (strcase (car Sym)) *A_SysLst*)
		*A_SysLst*))
	    (setq *A_SysLst*
	      (cons
		(list
		  (strcase (car Sym))
		  Temp)
		*A_SysLst*))))

	((and Sym
	   (listp Sym))
	  (if (setq Temp
		(assoc
		  (strcase (car Sym))
		  *A_SysLst*))
	    (progn
	      (setvar (car Sym) (last Temp))
	      (setq *A_SysLst*
		(append
		  (reverse
		    (cdr
		      (member Temp
			(reverse *A_SysLst*))))
		  (cdr
		    (member Temp *A_SysLst*)))))))

	((and Sym
	   (eq (type Sym) 'STR))
	  (if (setq Temp
		(assoc
		  (strcase Sym)
		  *A_SysLst*))
	    (progn
	      (setvar Sym (cadr Temp))
	      (if (caddr Temp)
		(setq *A_SysLst*
		  (subst
		    (cons
		      (car Temp)
		      (cddr Temp))
		    Temp
		    *A_SysLst*))
		(setq *A_SysLst*
		  (append
		    (reverse
		      (cdr
			(member Temp
			  (reverse *A_SysLst*))))
		    (cdr
		      (member Temp *A_SysLst*))))))))))

    (progn
      (foreach Temp *A_SysLst*
	(setvar (car Temp) (last Temp)))
      (setq *A_SysLst* nil)))

  (princ))


(princ "Done.\n")
(princ)

; CoordLT.lsp   copie de coordonn‚es dans le presse-papier
;{$PROFILE LISP}

; (c) avril 2000, O. Baumgartner

; Declare les variables golbales :

(prompt "\nLoading CoordLT...")

(unit "CoordLT" (uses '("UI" "ClipBrd")))


;***************************************************************

; conversion coord en chaine
(defun pointtoa(id p)
  (strcat (car id) (rtos (car p) 2) " " (cadr id) (rtos (cadr p) 2))
)


(defun GetCOORD(/ id p bkOsnap)
  (setq id '("X" "Y")
        p  "Point"
        bkOsnap (getvar "OSMODE"))
  (while (not (listp p))
    (setq p (UPOINT 0
                    "CEntre Point"
                    (strcat "\nCEntre/Point <" p ">")
                    nil nil))
    (if (not (listp p))
      (cond
        ((= p "CEntre")
           (setq id '("I" "J"))
           (setvar "OSMODE" (boole 7 4 bkOsnap)))
        ((= p "Point")
           (setq id '("X" "Y"))
           (setvar "OSMODE" bkOsnap))
      ))
  )
  (setvar "OSMODE" bkOsnap)
  (if p (pointtoa id p) nil)
)

; coord simple
(defun C:COORD(/ data)
  (setq data (GetCOORD))
  (if Data (progn
    (prompt data) (terpri)
    (clipboard_copy CF_TEXT data)
  ))
  (princ)
)


; coord multiples
(defun C:MCOORD(/ p data)
  (setq data nil
        p    (getCOORD))
  (while p
    (prompt p) (terpri)
    (setq data (append data (list p))
          p    (getCOORD))
  )
  (clipboard_copy CF_TEXT data)
  (princ)
)


(princ "\nCoordLT.lsp loaded")
(princ "\nCopie de coordonnees dans le presse-papier")
(princ "\n  COORD   copie d'une coord simple")
(princ "\n  MCOORD  copie multiple de coord")
(princ)

; Extraction de programmes CNC.
;{$PROFILE LISP}

; (c) O. Baumgartner, janvier 1998

; Utilise les variables globales :

;   #dwgsc       Echelle du dessin

; Declare les variables golbales :

;   #EFirst      Entity name du 1er titre
;   #tcLast      Handle du dernier ‚l‚ment
;   #tcDec       Decimales pour cercle & rayon
;   #tcNum       Numero courrant
;   #tcPos       Position de la case courrante du tableau

(setq TCLastMenu (getvar "MENUNAME"))
;(command "_MENU" "TCOORD")

(load "UI")
(load "BoumCNC")

(setq TCdec 3                                 ; decinale pour cercle & rayon
      #EFirst nil                             ; ename du block TB-COORD
      #tcLast nil
;     #dwgsc (/ 1 (getvar "UserR1"))
      #dwgname (getvar "DWGNAME")
      #dwgname "dwg"
      #TCnum "0"                              ; numero de depart
      AbsXY '(999.9 999.9)                    ; position fictive
)

; DXF

(defun dxf(Code elist)
  (cdr (assoc code elist))
); defun dxf

(defun tan(x)
  (/ (sin x) (cos x))
)

; ExtraitDat

(defun ExtraitDat(EName)
;  (setq PosRem (GetPosition EName)) ; !!! BoumCNC
  (setq EDat  (entget EName)
        EHan  (cdr (assoc 5 EDat))
        ETyp  (cdr (assoc 0 EDat))
        EFlag (cdr (assoc 70 EDat))
        ECenL (if (or (= ETyp "VERTEX") (= ETyp "SEQEND")) ECen nil)
        Bulge (cond ((null LBulge) nil)
                    ((zerop (cdr LBulge)) nil)
                    (T (cdr LBulge)))
        LBulge (assoc 42 EDat)
        ECen  (cond ((assoc 10 EDat)
                (trans (cdr (assoc 10 EDat)) EName 1))
                (T nil)) ; cond
        PT1   (cond ((assoc 10 EDat)
                (trans (cdr (assoc 10 EDat)) EName 1))
                (T nil))
        PT2   (cond ((assoc 11 EDat)
                (trans (cdr (assoc 11 EDat)) EName 1))
                (T nil))
        ERad  (cond ((member ETyp '("CIRCLE" "ARC")) (cdr (assoc 40 EDat)))
                    ((and (= ETyp "VERTEX") ECenL ECen Bulge)
                      (/ (setq Chord (distance ECen ECenL))
                         (* 2 (sin (setq IAng (* 2 (atan Bulge))))))) ; Vertex
                    ((and (= ETyp "SEQEND") ECenL P1 Bulge)
                      (/ (setq Chord (distance P1 ECenL))
                         (* 2 (sin (setq IAng (* 2 (atan Bulge))))))) ; Vertex
                    (T nil)) ; cond
  ) ; setq
) ; ExtraitDat

(defun Comment(str)
  (if (and ComFlg (/= Str ""))
    (princ
    (strcat "(" Str ")") f))
)

(defun Visu(str)
  (if (/= Str "")
    (cond
      ((= Machine "Almac")
        (EOB)
        (NUM) (princ (strcat "G4 K0.1 (*" Str ")") f)
        (EOB))
      ((= Machine "Centre")
        (EOB)
        (princ (strcat "$0" Str) f)
        (EOB))
      (T (Comment Str))
    )
  )
)

(defun EOB()
  (princ "\n" f)
  ; (princ "\r\n" f)
)

(defun NUM(/ StrNum)
  (if (numberp NInc) (progn
;    (setq StrNum (strcat "    " (itoa NNum))
;          StrNum (substr StrNum (- (strlen StrNum) 4) 5))
    (setq StrNum (itoa NNum))
    (princ (strcat "N" StrNum " ") f)
    (setq NNum (+ NNum NInc))))
) ; NUM

(defun NewSP(ComStr / StrNum)
  (cond
    ((= Machine "F35")
      (NUM)
        (setq StrNum (itoa NNum))
        (setq StrNum (cond
          ((= (strlen StrNum) 1) (strcat "00" StrNum))
          ((= (strlen StrNum) 2) (strcat "0" StrNum))
          (T StrNum)))
        (princ (strcat "G12 L01." StrNum) f)
        (comment ComStr) (EOB)
      (comment (strcat "SP_BEGIN " ComStr)) (EOB)
      (NUM) (princ "G10" f) (EOB)
    )
    ((= Machine "Almac")
      (NUM) (princ (strcat "G20 N" (itoa NumSP) ".1 ") f) (comment ComStr) (EOB)
      (comment (strcat "SP_BEGIN " ComStr))
      (NUM) (princ (strcat "G22 N" (itoa NumSP) " ") f) (EOB)
      (setq NumSP (1+ NumSP))
    )
    (T (comment (strcat "SP_BEGIN " ComStr)))
  )
)

(defun EndSP(ComStr)
  (CircleEnd)
  (setq ComStr (strcat "SP_END " ComStr))
  (cond
    ((= Machine "F35")
      (NUM) (princ "G11 " f) (comment ComStr) (EOB))
    ((= Machine "Almac")
      (NUM) (princ "G24 " f) (comment ComStr) (EOB))
    (T (comment ComStr))
  )
)

(defun CircleEnd()
  (if CirFlg (progn
    (setq CirFlg nil)
    (NUM) (princ "G80" f) (EOB)
	(EOB)
  )); progn & if
)

(defun VitAvance(iAvance)
 (if (not (equal iAvance Avance))
   (progn
     (setq Avance iAvance)
     (princ (strcat " F" Avance) f)
   ))
)

(defun Prologue()
  (setq Ligne "X")
  (while (/= Ligne "")
    (princ (strcat (setq Ligne (uString 0 "Ligne d'entˆte" "" T)) " ") f)
    (if (and ComFlg (/= Ligne "")) (Comment (getstring T "Commentaire:")))
    (EOB)
  )
)

(defun IncFichier()
 (setq f2Name (getfiled "Inclure fichier" "" "CNC" 4)
       f2 (open f2Name "r"))
 (comment (strcat "Fichier inclus " f2Name)) (EOB)
 (prompt (strcat "Fichier inclus " f2Name))
 (if (numberp NInc) (setq NNum (uint 4 "" "Num‚ro de d‚part" NNum)))
 (while (setq Ligne (read-line f2))
   (NUM) (princ (strcat Ligne "\n") f)
 ) ; eof
 (close f2)
)

(defun Outil()
  (if ComFlg (setq NomOutil (getstring T "Nom de l'outil:")))
  (setq NumOutil (uint 4 "" "Num‚ro d'outil" (1+ NumOutil))
        AvanceZ  (uString 0 "Avance axe Z" AvanceZ nil)
        AvanceXY (uString 0 "Avance axes XY" AvanceXY nil)
        VitBroche (uString 0 "Vitesse broche" VitBroche nil))
  (if (numberp NInc) (setq NNum (uint 4 "" "Num‚ro de d‚part" NNum)))
  (cond
    ((= Machine "Centre")
     (NUM) (princ (strcat "L100=" (itoa NumOutil) " ") f)
     (comment NomOutil) (EOB)
     (NUM) (princ "G77 H9998" f) (EOB)
     (NUM) (princ (strcat "D" (itoa NumOutil)
                          " S" VitBroche " M3 M40") f) (EOB)
    ) ; Centre
    ((= Machine "F35")
      (if (/= NumOutil 1) (progn
        (NUM) (princ "M05" f) (EOB)
        (NUM) (princ (strcat "G60 Z150 T" (itoa NumOutil)) f) (EOB)
      ))
      (NUM) (princ (strcat "G60 Z10 M6 T" (itoa NumOutil)) f)
      (comment NomOutil) (EOB)
      (NUM) (princ (strcat "F" AvanceZ " S" VitBroche " M13") f) (EOB)
    ) ; F35
    ((= Machine "Almac")
      (if (/= NumOutil 1) (progn
        (NUM) (princ "M05" f) (EOB)
        (NUM) (princ (strcat "G0 Z120 T0." (itoa NumOutil)) f) (EOB)
      ))
      (NUM) (princ (strcat "T0." (itoa NumOutil)) f)
            (comment (strcat "*METTRE T" (itoa NumOutil))) (EOB)
      (NUM) (princ "M06 M31" f)
            (comment (strcat "*METTRE T" (itoa NumOutil))) (EOB)
      (NUM) (princ (strcat "F" AvanceZ " S" VitBroche " M3 M11") f)
            (comment NomOutil) (EOB)
    ) ; Almac
    ((= Machine "Almac") ; Avec Platinum
      (comment NomOutil) (EOB)
      (NUM) (princ (strcat "P67=K" (itoa NumOutil)) f) (EOB)
      (NUM) (princ "G21 N93" f) (EOB)
      (NUM) (princ (strcat "F" AvanceZ " S" VitBroche " M3 M12") f) (EOB)
    ) ; Almac avec Platinum
    (T (comment NomOutil) (EOB)
    )
   ) ; cond
)

(defun PosXY(XY DeplNull / RelXY)
; DeplNull = T -> sortie des coords XY sans deplacement
  (setq RelXY (mapcar '- XY AbsXY)   ; coord relatives
        AbsXY XY                     ; coord absolue
        XY (if FlagG91 RelXY AbsXY)) ; XY Relatif/Absolu
  (if AxeRot
    (progn
      (if (or DeplNull (not (zerop (cadr RelXY))))
        (princ (strcat "W" (rtos (- (cadr XY)) 2 3) " ") f))
      (if (or DeplNull (not (zerop (car RelXY))))
        (princ (strcat "X" (rtos (car XY) 2 3) " ") f)))
    (progn
      (if (or DeplNull (not (zerop (car RelXY))))
        (princ (strcat "X" (rtos (car XY) 2 3) " ") f))
      (if (or DeplNull (not (zerop (cadr RelXY))))
        (princ (strcat "Y" (rtos (cadr XY) 2 3) " ") f)))
  )
)

(defun G0(XY Z)
  (cond
    ((= Machine "F35")
      (NUM) (princ (strcat (if FlagG91 "G91 " "") "G60 ") f))
    (T
      (NUM) (princ (strcat (if FlagG91 "G91 " "") "G0 ") f))
  )
  (if XY (PosXY XY nil))
  (if Z (princ (strcat "Z"
         (if (numberp Z) (rtos Z 2 3) Z) " ") f))
)

(defun G1(G4x XY Z)
  (cond
    ((= Machine "F35")
      (NUM)
      (if XY (princ (strcat (if FlagG91 "G91 " "") "G1 ") f) (princ "G61 " f)))
    (T (NUM) (princ (strcat (if FlagG91 "G91 " "") "G1 ") f)))
  (if (/= G4x "") (princ (strcat G4x " ") f))
  (if XY (PosXY XY nil))
  (if Z (princ (strcat "Z"
         (if (numberp Z) (rtos Z 2 3) Z) " ") f))
  (VitAvance (if XY AvanceXY AvanceZ))
)

(defun G2G3(G4x XY IJ Rad Bulge / Rayon Center)
  (NUM)
  (if FlagG91 (setq IJ (mapcar '- IJ AbsXY)))
  (if AxeRot (setq IJ (list (car IJ) (- (cadr IJ)))
                   Rad (- Rad)))
  (setq Rayon (strcat "R" (if (> (abs Bulge) 1) "-" "")
                      (rtos (abs Rad) 2 TCdec) " ")
        Center (strcat "I" (rtos (car IJ) 2 3) " J" (rtos (cadr IJ) 2 3) " "))
  (cond
    ((= Machine "Almac")
     (if FlagG91 (princ "G91 ") (princ "G06 " f)))
    (T
     (if FlagG91 (princ "G91 " f)))
  )
  (princ (if (< Rad 0.0) "G2 " "G3 ") f)
  (if (/= G4x "") (princ (strcat G4x " ") f))
  (PosXY XY T)
  (cond ((= Machine "FAnuc")
           (princ Rayon f) (VitAvance AvanceXY) (comment Center))
        (T
           (princ Center f) (VitAvance AvanceXY) (comment Rayon)))
)

(defun helicoid(XY Z IJ K Rad Bulge / Rayon Center)
  (NUM)
  (if AxeRot (setq IJ (list (car IJ) (- (cadr IJ)))
                   Rad (- Rad)))
  (setq Rayon (strcat "R" (if (> (abs Bulge) 1) "-" "")
                      (rtos (abs Rad) 2 TCdec) " ")
        Center (strcat "I" (rtos (car IJ) 2 3) " J" (rtos (cadr IJ) 2 3) " "))
  (princ (if (< Rad 0.0) "G2 " "G3 ") f)
  (PosXY XY T)
  (princ (strcat "Z" (rtos Z 2 3) " ") f)
  (cond ((or (= Machine "FAnuc") (= Machine "Almac"))
           (princ Rayon f))
        (T
           (princ Center f)))
  (princ (strcat " K" (rtos (abs K) 2 3) " ") f)
  (VitAvance AvanceXY)
)

(defun BoumCNC1(XY rad)
  (entmake
    (list '(0 . "CIRCLE") '(8 . "BOUMCNC1")
          (cons 10 XY) (cons 40 rad)))
)

(defun G8x(XY / ProfZp rem0)
  (if (= (strcase ProfZ) (strcase "Pointage"))
    (progn ; pointage
      (if (and PosRem (= "S" (substr (cadr PosRem) 1 1)))
        (setq ProfZp (+ (/ (atof (substr (cadr PosRem) 2)) 2) 0.03))
        (setq ProfZp (+ ERad 0.02))
      )
      (if (and CirFlg (/= ProfZp LastProfZp)) (CircleEnd))
      (setq LastProfZp ProfZp)
      (G82 XY (strcat "-" (rtos ProfZp 2 3)))
;      (BoumCNC1 XY ProfZp) ; dessine le cercle
    )
    (G81 XY ProfZ) ; normal
  )
)

(defun G81(XY ProfZ / Diam)
  (if PosRem
    (setq Diam (strcat (car PosRem) "  -> D=" (rtos (* 2 ERad) 2 TCdec) " " (cadr PosRem)))
    (setq Diam (strcat "diam " (rtos (* 2 ERad) 2 TCdec))))
  (cond ((= Machine "FAnuc")
           (if CirFlg
             (progn (NUM) (princ "    " f) (PosXY XY) (comment Diam)
              (EOB))
             (progn
               (G0 XY nil) (comment Diam) (EOB)
               (G0 nil DegaZ) (EOB)
               (NUM) (princ (strcat "G81 G98 Z " ProfZ
                       " R" (rtos DegaZ 2 3) " (!!)") f)
                     (VitAvance AvanceZ) (EOB)))) ; FAnuc
        ((= Machine "Centre")
           (G0 XY nil) (Comment Diam) (EOB)
           (if (not CirFlg) (progn
             (NUM) (princ (strcat "G81 ER " (rtos DegaZ 2 3) " Z " ProfZ) f)
             (VitAvance AvanceZ) (EOB))))
        ((= Machine "Almac")
           (G0 XY nil) (comment Diam) (EOB)
           (if (not CirFlg) (progn
             (EOB)
             (if SP_Flg (progn (G0 nil DegaZ) (EOB)))
             (NUM) (princ (strcat "G81 Z " (rtos DegaZ 2 3) " I " ProfZ) f)
             (VitAvance AvanceZ) (EOB))))
        ((= Machine "F35")
           (G0 XY nil) (comment diam) (EOB)
           (if (not CirFlg) (progn
             (EOB)
             (If SP_Flg (progn (G0 nil DegaZ) (EOB)))
             (NUM) (Princ (strcat "G81 X " (rtos DegaZ 2 3) " Z " ProfZ) f)
             (VitAvance AvanceZ) (EOB))))
        (T (G0 XY nil) (comment Diam) (EOB)
           (if (not CirFlg) (progn
             (EOB)
             (if SP_Flg (progn (G0 nil DegaZ) (EOB)))
             (NUM) (princ (strcat "G81 Z " ProfZ " (!!)") f)
             (VitAvance AvanceZ) (EOB))))
   )
   (setq CirFlg T)
)

(defun G82(XY ProfZ / Diam)
  (if PosRem
    (setq Diam (strcat (car PosRem) "  -> D=" (rtos (* 2 ERad) 2 TCdec) " " (cadr PosRem)))
    (setq Diam (strcat "diam " (rtos (* 2 ERad) 2 TCdec))))
  (cond 
        ((= Machine "F35")
           (G0 XY nil) (comment diam) (EOB)
           (if (not CirFlg) (progn
             (EOB)
             (If SP_Flg (progn (G0 nil DegaZ) (EOB)))
             (NUM) (Princ (strcat "G82 X " (rtos DegaZ 2 3) " Z " ProfZ " L1.010") f)
             (VitAvance AvanceZ) (EOB))))
        (T (G0 XY nil) (comment Diam) (EOB)
           (if (not CirFlg) (progn
             (EOB)
             (if SP_Flg (progn (G0 nil DegaZ) (EOB)))
             (NUM) (princ (strcat "G82 Z " ProfZ " (!!)") f)
             (VitAvance AvanceZ) (EOB))))
   )
   (setq CirFlg T)
)


(defun C:CNC(/ XY IJ Rad Ang G4x ComStr); Sel Index Count EName Val ECom Rayon)
  (setq ProfZ "0.0"
        DegaZ 1.0
        AngGrav 45.0
        AbsXY '(0.0 0.0))
  (setq CirFlg nil ; Flag cercles
        NumSP 1    ; Numero de SP
        FlagG91 nil)
  (setq AvanceZ ""
        AvanceXY ""
        Avance nil) ; vitesse d'avance
  (setq VitBroche "") ; vitesse broche
  (setq NumOutil 0) ; num‚ro d'outil
  (setq Correction "G40")
  (setq NInc (uint 6 "Aucun" "Incr‚ment de num‚rotation, Aucun, " 10)
        NNum (if (numberp NInc) NInc 0)
        ComFlg (if (= (ukword 1 "Oui Non" "Commentaires actifs(Oui/Non)"
                    "Oui") "Oui") T nil)
        Machine (ukword 1 "F35 Almac Centre FAnuc"
                 "Machine (F35/Almac/Centre/FAnuc)" "F35"))
  (setq fName (getfiled "Enregister programme" "" "CNC" 1))
  (if (findfile fName)
    (if (= (ukword 1 "Effacer Ajouter" "Le fichier existe (Effacer/Ajouter)" "Effacer")
             "Effacer")
      (progn
        (setq f (open fName "w"))
        (cond ((= Machine "F35") (princ "%" f) (EOB))
              ((= Machine "Almac")
                (princ "%00002" f) (EOB)
                (NUM) (princ "G55" f) (EOB)
                (NUM) (princ "G43" f) (EOB)
              )
      ))
      (setq f (open fName "a"))
    )
    (progn
      (setq f (open fName "w"))
      (cond ((= Machine "F35") (princ "%" f) (EOB))
            ((= Machine "Almac")
              (princ "%00002" f) (EOB)
              (NUM) (princ "G55" f) (EOB)
              (NUM) (princ "G43" f) (EOB)
            )
      ))
  ) ; if
  (comment (strcat "Machine " Machine)) (EOB)
  (setq Cmd T)
  (while Cmd ; boucle des commandes...
   (setq Cmd (ukword 0 "Prologue Fichier Outil Entites Sortir"
               "Prologue/Fichier/Outil/Sortir/Entites" "Entites"))
   (cond
     ((= Cmd "Prologue") (Prologue))
     ((= Cmd "Fichier") (IncFichier))
     ((= Cmd "Outil") (Outil))
     ((= Cmd "Sortir") (setq Cmd nil))
     ((= Cmd "Entites")
    (prompt "\nSélectionner les objets... ")
    (setq Sel   (ssget)
          Index 0
          Count (if (null Sel) 0 (sslength Sel)))
    (if (not (zerop Count)) (progn
      (setq DegaZ (ureal 0 "" "D‚gagement Z (G00)" DegaZ))
      (setq Grav (ukword 1 "Oui Non" "Gravage CNC" "Non"))
      (if (= Grav "Oui")
        (setq AngGrav (ureal 7 "" "Angle de fraise gravage" AngGrav)
              Correction "G40")
        (setq ProfZ (ustring 0 "Prof Z (G01) / Pointage" ProfZ nil)
              Correction (ustring 0 "Correction outil (G40/G41/G42)" Correction
              nil)))
      (setq G91 (ukword 1 "Oui Non" "Polyligne : XY Relatif" "Non")
            AxeRot (= (ukword 1 "Oui Non" "Axe Rotatif" "Non") "Oui"))
      (if (numberp NInc) (setq NNum (uint 4 "" "Num‚ro de d‚part" NNum)))
      (if ComFlg (setq ComStr (getstring T "Commentaire:")))
      (EOB)
    )); progn & if
    (setq SP_Flg T) ; 1er element de la liste d'objet
    (NewSP ComStr)
    (while (< Index Count)
      (setq EName (ssname Sel Index))
      (ExtraitDat EName)
      (cond ((= ETyp "LINE") (progn
        (CircleEnd)
        (G0 Pt1 nil) (EOB)
        (G0 nil DegaZ) (EOB)
        (G1 "" nil ProfZ) (EOB)
        (G1 "" Pt2 nil) (EOB)
        (G0 nil DegaZ) (EOB)
      ))) ; progn & cond
      (cond ((= ETyp "CIRCLE") (G8x ECen))) ; Per‡age
      (cond ((= ETyp "POLYLINE") (progn
              (CircleEnd)
              (setq Closed (not (zerop (logand EFlag 1)))
                    EName (entnext EName))
              (setq First T)
              (ExtraitDat EName)
              (if Closed (setq P1 ECen) (setq P1 ECen))
              (G0 ECen nil) (EOB)
              (G0 nil DegaZ) (EOB)
              (if (= Grav "Non") (progn (G1 "" nil ProfZ) (EOB)))
              (if (= G91 "Oui") (progn
                (setq FlagG91 T)
                (cond
                  ((= Machine "F35") (comment "DEPL. RELATIF"))
                  (T (NUM) (princ "G91" f) (comment "DEPL. RELATIF"))
                )
                (EOB)
              ))
              (setq EName (entnext EName))
              (ExtraitDat EName)
              (while (= ETyp "VERTEX")
                (setq XY ECen
                      Rad ERad)
                (setq G4x (if (and First (/= Correction "G40"))
                       Correction ""))
                (setq EName (entnext EName))
                (if ERad
                  (setq IJ (polar ECen (+ (angle ECen ECenL) Iang (/ Pi -2)) ERad))
                 )  ; IF
              (if (= Grav "Oui") (progn
                (setq ProfZ (- (/ (dxf 41 EDat) (tan (* (/ AngGrav
                            180.0) Pi)) 2.0)))
                (if ERad
                  (setq K (* (/ (abs (- (dxf 41 EDat) (dxf 40 EDat)))
                          (tan (* (/ AngGrav 180.0) Pi)) 2.0) (/ Pi IAng))))
                (if ERad (helicoid XY ProfZ IJ K Rad Bulge)
                         (G1 G4x XY ProfZ))
                ) ; gravage
                (If ERad (G2G3 G4x XY IJ Rad Bulge)
                         (G1 G4x XY nil)))
              (EOB)
              (ExtraitDat EName)
              (setq First nil)
              ) ; while
              (if Closed (progn
                (setq XY P1
                      Rad ERad)
                (setq EName (entnext EName))
                (if ERad
                  (setq IJ (polar P1 (+ (angle P1 ECenL) Iang (/ Pi -2)) ERad))
                 )  ; IF
              (if (= Grav "Oui") (progn
                (setq ProfZ (- (/ (dxf 41 EDat) (tan (* (/ AngGrav
                            180.0) Pi)) 2.0)))
                (if ERad
                  (setq K (* (/ (abs (- (dxf 41 EDat) (dxf 40 EDat)))
                          (tan (* (/ AngGrav 180.0) Pi)) 2.0) (/ Pi IAng))))
                (if ERad (helicoid XY ProfZ IJ K Rad Bulge)
                         (G1 G4x XY ProfZ))
                ) ; gravage
                (If ERad (G2G3 G4x XY IJ Rad Bulge)
                         (G1 G4x XY nil)))
              (EOB)
                )  ; progn & IF
                ) ; if Close
              (if (/= Correction "G40") (progn
                (NUM)
                (princ "G40" f)
                (EOB))) ; annulation correction G41/G42
              (if (= G91 "Oui") (progn
                (setq FlagG91 nil)
                (cond
                  ((= Machine "F35") (comment "DEPL. ABSOLU"))
                  (T (NUM) (Princ "G90" f) (comment "DEPL. ABSOLU"))
                )
                (EOB)
              ))
              (G0 nil DegaZ) (EOB)
              (EOB)
            ))) ; progn &cond
      (setq Index (1+ Index))
      (setq SP_Flg nil)
      ; (princ "\n" f)
    ) ; while
    (EndSP ComStr)
    (CircleEnd)
  ))) ; while cmd
  (if (= (ukword 1 "Oui Non" "Cloture d'usinage (Oui/Non)" "Oui")
       "Oui")
    (cond
      ((= Machine "FAnuc")
        (NUM) (princ "M99" f) (EOB)
      ) ; Fanuc
      ((= Machine "Almac")
        (NUM) (princ "M5" f) (EOB)
        (NUM) (princ "G0 G90 Z120" f) (EOB)
      ) ; Almac
      ((= Machine "F35")
        (NUM) (princ "M15" f) (EOB)
        (G0 nil 150.0) (EOB)
        (NUM) (princ "G60 Y60" f) (EOB)
        (NUM) (princ "M02" f) (EOB)
        (princ "M30" f) (EOB)
      ) ; F35
    ) ; cond
  ); cloture d'usinage
  (close f)
  (princ)
)


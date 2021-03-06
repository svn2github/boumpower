; Programmation CNC avec AutoCAD
; N�cessite le logiciel CNC-Studio
; compatible avec AutoCAD R13 R14
;                 AutoCAD LT97 LT98 (avec ACE-LT Manager, DDE4LT et MonitorDDE

;{$PROFILE LISP}

; (c) avril 2004, O. Baumgartner

; Declare les variables golbales :

;   #tcLastmenu  Menu actif

;   #ChnlSysDDE  Conversation DDE System
;   #PrgVersion  Version logiciel


(unit "CNC" (uses '("ClipBrd" "UI")))


(setq TCdec 3                                 ; decinale pour cercle & rayon
      #tcLastMenu (getvar "MENUNAME")
      #PrgVersion "B�ta 1.0"                  ; version du prog
;      #CncChnl    (appinit "BoumCNC"
;                            "System"
;                            "C:\\Biblio\\BoumCNC.EXE")
;      #CncItem    "CncAcad"                   ; item DDE
      #GrpName    "SansNom"
      #GrpOrigine '(0.0 0.0 0.0)
)

; ***************************************************************

(defun dxf(code elist)
 (cdr (assoc code elist))
)

; ***************************************************************


; ***************************************************************

(defun C:CNC(/ sel terminated kw)
  (setq sel (ssget "I"))
  (if (null sel) (progn
;    (prompt "\nChoix des entit�s :")
    (setq sel (ssget))
  ))
  (OpenClipboard)
  (setq terminated (null sel))
  (while (not terminated)
    (setq kw (ukword 1 "Nommer Origine Cnc"
                       (strcat "Nommer \"" #GrpName "\"/Orgine "
                       (etos #GrpOrigine) "/Cnc")
                       "Cnc"))
    (cond
      ((= kw "Nommer") (setq #GrpName (ustring 1 "Nom du groupe" #GrpName T)))
      ((= kw "Origine")
        (setq #GrpOrigine (upoint 1 "" "Origine" #GrpOrigine nil))
      )
      ((= kw "Cnc")
        (cnc_proceed_selection sel #GrpName #GrpOrigine)
        (setq sel nil
              terminated T)
      ))
  ) ; while
  (CloseClipboard CF_DXF)
  (prin1)
)

; sortie format�e du type id=data
(defun id_data(id data)
  (strcat "\t" id "=" (etos data))
)

(defun id_coord(ids coords)
  (apply 'strcat (mapcar 'id_data ids coords))
)

; exporte les donn�e d'entit�
; e            liste retourn�e par entget
; fulldxf      si T ajoute les donn�es de e
; desc_fichier fichier de sortie

; !!! r�entrente !!!
(defun cnc_proceed_entity(e fulldxf / dxfstr
                          etype XYZ XYZ1 XYZ2 FlgSEQ)
  (setq XYZ     '("X" "Y" "Z")
        XYZ1    '("X1" "Y1" "Z1")
        XYZ2    '("X2" "Y2" "Z2")
;        dxfstr  (if fulldxf (id_data "DXF" (cdddr e)) "")
        dxfstr  (if fulldxf (id_data "DXF" e) "")
        FlgSEQ  nil                ; T = sequence polyline ou insert
        etype   (dxf 0 e))         ; Type
  (cond
    ((= etype "POINT")
         (AddToClipboard dxfstr)
;        (AddToClipboard
;          (strcat "POINT"
;                  (id_coord XYZ (trans (dxf 10 e) (dxf -1 e) 1))
;                  dxfstr))
      ) ; POINT
    ((= etype "LINE")
         (AddToClipboard dxfstr)
;        (AddToClipboard
;          (strcat "LINE"
;                  (id_coord XYZ1 (trans (dxf 10 e) (dxf -1 e) 1))
;                  (id_coord XYZ2 (trans (dxf 11 e) (dxf -1 e) 1))
;                  dxfstr))
      ) ; LINE
    ((= etype "CIRCLE")
         (AddToClipboard dxfstr)
;        (AddToClipboard
;          (strcat "CIRCLE"
;                  (id_coord XYZ (trans (dxf 10 e) (dxf -1 e) 1))
;                  (id_data "R" (dxf 40 e))
;                  dxfstr))
      ) ; CIRCLE
    ((= etype "POLYLINE")
        (setq FlgSEQ T) ; extraction des vertex
        (AddToClipboard
          (strcat "POLYLINE"
                  (id_coord XYZ (trans (dxf 10 e) (dxf -1 e) 1))
                  (id_data "FLAGS" (dxf 70 e))
                  dxfstr))
      ) ; POLYLINE
    ((= etype "LWPOLYLINE")
         (AddToClipboard dxfstr)
;        (AddToClipboard
;          (strcat "LWPOLYLINE"
;                  (id_coord XYZ (trans (dxf 10 e) (dxf -1 e) 1))
;                  (id_data "FLAGS" (dxf 70 e))
;                  dxfstr))
      ) ; LWPOLYLINE
    ((= etype "VERTEX")
        (AddToClipboard
          (strcat "VERTEX"
                  (id_coord XYZ (trans (dxf 10 e) (dxf -1 e) 1))
                  (if (not (zerop (dxf 40 e))) (id_data "LI" (dxf 40 e)) "")
                  (if (not (zerop (dxf 41 e))) (id_data "LF" (dxf 41 e)) "")
                  (if (not (zerop (dxf 42 e))) (id_data "BULGE" (dxf 42 e)) "")
                  (if (not (zerop (dxf 70 e))) (id_data "FLAGS" (dxf 70 e)) "")
                  dxfstr))
      ) ; VERTEX
    ((= etype "SEQEND")
        (AddToClipboard (strcat "SEQEND" dxfstr))
      ) ; SEQEND
  ); cond
 (while FlgSEQ ; envoye les sub-entity si n�cessaire
   (setq e      (entget (entnext (dxf -1 e)))
         FlgSEQ (/= (dxf 0 e) "SEQEND"))
   (cnc_proceed_entity e fulldxf)
 )
; fin du parcour de la sequence
)

(defun cnc_proceed_selection(Sel Name Origine / Index Count)
  (setq Index 0
        Count (if (null Sel) 0 (sslength Sel)))
;  (AddToClipboard (strcat
;          "GROUPE"
;          (id_data "NAME" Name)
;          (id_coord '("X" "Y" "Z") Origine)))

  (while (< Index Count) ; on parcourt tous les objets
    (cnc_proceed_entity (entget (ssname Sel Index)) T)
    (setq Index (1+ Index)) ; objet suivant
  ); while parcourt de la selection
;  (AddToClipboard (strcat
;          "GRPEND"
;          (id_data "NAME" Name)))
)


(princ "\n")
(princ "\n  CNC     programme CNC interractif")
(prin1)

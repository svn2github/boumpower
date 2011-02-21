{$PROFILE LISP}
{$TITLE Texte artistique}

(uses (list "MATH" "STRINGS" "UI"))

(defun StyleSize (style)
  (cdr (assoc 40 (tblsearch "Style" style)))
); defun

(setq compress 1.0)

; !!! La hauteur du texte dans la commande TEXT doit etre ajustable
(defun C:ATEXT(/ midp radp txt radi txtlen txtspc txthgt arclen
                 arcang sang orent txtang txtp char fixsize
                 justific txtsize accrp txtj txtoffs)
  (setq cmdech (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (graphscr)

  (setq justific "Centre"
        radp     ""
        compress 1.0)
  (while (not (listp radp))
    (setq radp (upoint 1 "Justifier"
                   "Justifier/<Pt. Centre de rotation>" nil nil))
    (if (not (listp radp))
      (cond
        ((= radp "Justifier")
         (setq justific (ukword 0
          "Aligne Centre Milieu Gauche Droite HG HC HD MG MC MD BG BC BD"
          "Aligne/Centre/Milieu/Gauche/Droite/HG/HC/HD/MG/MC/MD/BG/BC/BD"
          "Centre")))
      ))
  ) ; while pour donn‚es de formatage
  (setq fixsize (stylesize (getvar "TEXTSTYLE"))
        txtsize (getvar "TEXTSIZE")
        txt     (ustring 1 "Texte" nil T)
        txtlen  (strlen txt))

  (if (zerop fixsize)
    (setq txthgt (udist 1 "" "Hauteur du texte" txtsize nil))
    (setq txthgt fixsize))

  (if (/= Justific "Aligne")
    (setq compress (ureal 6 "" "Facteur de compression" compress)))

  (setq orent (ukword 1 "Horaire Anti-horaire" "Sens (Horaire/Anti)" "Horaire"))
  ; prise du/des point(s) d'accrochage du texte
  (cond
    ((= Justific "Aligne")
       (setq accrp  (upoint 1 "" "Pt. de depart du texte" nil radp)
;             accr2  (upoint 1 "" "Angle a couvrir" nil radp)
             arclen (* (- (angle radp accrp)
                          (uangle 1 "" "Angle a couvrir" nil radp))
                       (distance radp accrp)))
    )
    (T (setq accrp  (upoint 1 ""
                          (strcat "Pt. " Justific " du texte")
                          nil radp)
             arclen (* txtlen txthgt compress))
    )
  ); cond
;  (setq midp accrp)
  (princ (* 180 (/ arclen 3.1415)))
  (setq radi    (distance radp accrp)
        arcang  (/ arclen radi)
        txtspc  (/ arclen txtlen)
        txtoffs (/ txtspc radi -2.0))

  (cond
    ((/= 0 (strpos Justific "Aligne Gauche HG MG BG"))
      (setq sang (+ (angle radp accrp) txtoffs)))
    ((/= 0 (strpos Justific "Centre Milieu HC MC BC"))
      (setq sang (+ (angle radp accrp) (/ arcang 2) txtoffs)))
    ((/= 0 (strpos Justific "Droite HD MD BD"))
      (setq sang (+ (+ (angle radp accrp) arcang) txtoffs)))
  ); cond

  (cond
    ((/= 0 (strpos Justific "Aligne Gauche Centre Droite"))
      (setq txtj "C"))
    ((/= 0 (strpos Justific "BG BC BD"))
      (setq txtj "BC"))
    ((/= 0 (strpos Justific "MG MC MD"))
      (setq txtj "MC"))
    ((/= 0 (strpos Justific "HG HC HD"))
      (setq txtj "HC"))
    ((/= 0 (strpos Justific "Milieu"))
      (setq txtj "Milieu"))
  ); cond
;  (if (= orent "Horaire")
;    (setq radi (- radi (/ txthgt 2)))
;    (setq radi (+ radi (/ txthgt 2)))
;  )

  (setq
;    sang (- (+ (angle radp midp) (/ arcang 2)) (/ txtspc radi 2))
    count 1
  )

  (command "_UNDO" "_Begin")

  (repeat txtlen
    (if (= orent "Horaire")
      (setq
        txtang (angtos (- sang (/ pi 2)) 0)
        txtpos count
      )
      (setq
        txtang (angtos (+ sang (/ pi 2)) 0)
        txtpos (- (1+ txtlen) count)
      )
    )
    (setq
      txtp (polar radp sang radi)
      char (substr txt txtpos 1)
    )
    (if (zerop fixsize)
      (command "_TEXT" "J" txtj txtp txthgt txtang char)
      (command "_TEXT" "J" txtj txtp txtang char)
    )
    (setq
      count (1+ count)
      sang (- sang (/ txtspc radi))
    )
  );repeat
  (command "_UNDO" "_End")
  (setvar "CMDECHO" cmdech)
  (princ)
)

(princ)
;{$PROFILE LISP}
;{$TITLE Fonctions chaines ASCII}

(defun StrPos(Sub Str / Pos i len)
  (setq Pos 0)
  (if (and (= (type Sub) 'STR) (= (type Str) 'STR))
    (progn
      (setq i 1
            len (strlen Sub))
      (while (and (zerop Pos) (<= len (- (1+ (strlen Str)) i)))
        (if (= Sub (substr Str i  len)) (setq Pos i))
        (setq i (1+ i))
      ); while
    )); if
  Pos
)

; retire les accents d'une chaine de caractères.
; (setq #Accent nil) ; pour les versions AutoCAD supportant les caratères accentués
(setq #Accent (list
          '(" " . "a") '("…" . "a") '("ƒ" . "a") '("„" . "a")
          '("‚" . "e") '("Š" . "e") '("ˆ" . "e") '("‰" . "e")
          '("Œ" . "i") '("‹" . "i")
          '("“" . "o") '("”" . "o")
          '("–" . "u") '("" . "u")))

(defun NoAccent(S / C1 C2 P) ; non récursif /LispStack OverFlow !/
  (setq P (strlen S))
  (if (and (> P 0) #Accent) ; Chaine non vide et liste de substitution
    (while (> P 0)
      (setq C1 (substr S P 1)
            C2 (cdr (assoc C1 #Accent)) ; substitue le caractère ?
            S (if (not C2)
                 (strcat (substr S 1 (1- P)) C1 (substr S (1+ P)))
                 (strcat (substr S 1 (1- P)) C2 (substr S (1+ P))))
            P (1- P)) ; suivant
    )) ; while & if
  S ; Retourne la chaine
)

(princ)

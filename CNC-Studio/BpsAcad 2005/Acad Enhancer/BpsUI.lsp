;;; BpsUI.lsp
;;; (c)2005 www.boumpower.ch / CH-1345 Le Lieu
;;; 

(vl-load-com)
(vl-doc-export 'ustring)      
(vl-doc-export 'uint)      
(vl-doc-export 'ureal)      
(vl-doc-export 'upoint)      
(vl-doc-export 'uangle)      
(vl-doc-export 'udist)      
(vl-doc-export 'ukword)      
(vl-doc-export 'udiam)      



(defun ustring(bit msg def spflag / inp nval)
  (if (and def (/= def ""))
    (setq msg (strcat "\n" msg " <" def ">: ")
          inp (getstring msg spflag)
          inp (if (= inp "") def inp))
    (progn 
      (setq msg (strcat "\n" msg ": "))
      (if (= bit 1) 
        (while (= "" (setq inp (getstring msg spflag))))
        (setq inp (getstring msg spflag))
      )
    )
  )
  inp
) 
 

(defun uint(bit kwd msg def / inp)
  (if def 
    (setq msg (strcat "\n" msg " <" (itoa def) ">: ")
          bit (* 2 (fix (/ bit 2))))
    (setq msg (strcat "\n" msg ": ")))
  (initget bit kwd)
  (setq inp (getint msg))
  (if inp inp def)
)  

(defun ureal(bit kwd msg def / inp)
  (if def 
    (setq msg (strcat "\n" msg " <" (rtos def 2) ">: ")
          bit (* 2 (fix (/ bit 2))))
    (setq msg (strcat "\n" msg ": ")))
  (initget bit kwd)
  (setq inp (getreal msg))
  (if inp inp def)
)  

(defun upoint(bit kwd msg def bpt / inp pts)
  (if def 
    (setq pts (strcat 
                (rtos (car def)) "," (rtos (cadr def))
                (if (and (caddr def) (= 0 (getvar "FLATLAND"))) 
                  (strcat "," (rtos (caddr def))) "" )) 
          msg (strcat "\n" msg " <" pts ">: ") 
          bit (* 2 (fix (/ bit 2))))
    (setq msg (strcat "\n" msg ": ")))
  (initget bit kwd)
  (setq inp (if bpt (getpoint msg bpt) (getpoint msg)))
  (if inp inp def)
) 

(defun uangle(bit kwd msg def bpt / inp)
  (if def 
    (setq msg (strcat "\n" msg " <" (angtos def) ">: ")
          bit (* 2 (fix (/ bit 2))))
    (setq msg (strcat "\n" msg ": ")))
  (initget bit kwd) 
  (setq inp (if bpt (getangle msg bpt) (getangle msg))) 
  (if inp inp def) 
)  

(defun udist(bit kwd msg def bpt / inp)
  (if def 
    (setq msg (strcat "\n" msg " <" (rtos def) ">: ")
          bit (* 2 (fix (/ bit 2))))
    (setq msg (strcat "\n" msg ": ")))
  (initget bit kwd) 
  (setq inp (if bpt (getdist msg bpt) (getdist msg))) 
  (if inp inp def) 
)  

(defun ukword(bit kwd msg def / inp) 
  (if (and def (/= def "")) 
    (setq msg (strcat "\n" msg " <" def ">: ") 
          bit (* 2 (fix (/ bit 2))))
    (setq msg (strcat "\n" msg ": ")))
  (initget bit kwd) 
  (setq inp (getkword msg)) 
  (if inp inp def) 
) 

(defun udiam(bit kwd msg def bpt / inp) 
  (if def 
    (setq msg (strcat "\nRayon/" msg " <diam. " (rtos def 2) ">: ") 
          bit (* 2 (fix (/ bit 2))))
    (setq msg (strcat "\nRayon/" msg " <diam.>: "))) 
  (initget bit (strcat "Rayon" kwd)) 
  (setq inp (getreal msg)) 
  (cond (
    (null inp) def) 
    ((= inp "Rayon") 
      (initget bit kwd)
      (setq inp (if bpt (getdist "Rayon: " bpt) (getdist "Rayon: ")))
      (cond (
        (null inp) def)
        ((numberp inp) (* inp 2))
        (T inp)))
    (T inp))
)  

(princ) ; Silent load
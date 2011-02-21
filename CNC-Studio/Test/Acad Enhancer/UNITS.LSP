; Units.lsp   routines diverses pour BoumPowerSoftware
; for AutoCAD Release 10-13 (AutoLISP source file)
; (c) 2000, O. Baumgartner, CH 1345 Le Sentier

; Global-var
; *MODULES* list of strings ("modulename1" "modulename2" ...)
;
; (list-all-packages) 
; (package-use-list "packagename")
; (PACKAGE-USED-BY-LIST  "packagename")
; (find-package "name")



; (Unit Uame UsedUnits)  ; definit l'unité
; (ReleaseUnit Name)     ; libère l'unité
; (Uses Modules)         ; charge les dépendences


(prompt "\nLoading Units...")

(setq #modules   '()
      #extension ".LSP")

(defun unit(name uses)
  (princ)
)

(defun releaseunit(name)
  (princ)
)

(defun uses(modules / fname u)
  (foreach u modules
    (if (null (member u #modules))
      (if (setq fname (findfile (strcat u #extension)))
        (progn
          (if (eq (load fname "ERROR") "ERROR")
            (print (strcat "Erreur chargement : " fname)))
          (setq #modules (cons u #modules)))
        (print (strcat "Erreur localisation : " u #extension))
      )
    )
  )
  modules
)

(prompt "\n")
(princ)

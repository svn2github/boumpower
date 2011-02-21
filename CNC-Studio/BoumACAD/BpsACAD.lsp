; BpsACAD.LSP
; (c) 2004 , O. Baumgartner, CH-1345 Le lieu

;                         *** Boum Power Software ***


; Les modifications suivantes sont à apporter au fichier Acad.lsp (noter que le nom de
; ce fichier peut varier d'une version à l'autre d'AutoCAD : AcadR13.lsp AcLT.lsp, etc)
; se référer à la documentation d'AutoCAD pour plus de détails.
; Veiller également aux réglages d'AutoCAD 

; Ajouter la ligne suivante (sans le ";") 
; (load "BpsACAD")

; Créer	ou modifier la fonction S::STARTUP afin d'appeler (BpsACAD::Startup)
; (defun S::STARTUP()
;   (BpsACAD::Startup)
;   (princ)
; )


;==============================================================================

;* Attention : seules les lignes entre les blocs # BPS begin et # BPS end
;* peuvent être modifiées.
;* ce fichier est mis à jour par les setup des logiciels Boum Power Software

;# BPS version 1.01
;# BPS Language French

;==============================================================================

;;; === Load LISP Application ===

;# BPS begin Load Section
(load "Units") ; 2000, O. Baumgartner
;# BPS end Load Section

;;; === AutoLoad LISP Application ===

;# BPS begin AutoLoad Section
;# BPS end AutoLoad Section

;==============================================================================

;;; === General Utility Functions for Boum Power Software ===

; (GetAcadVersion)       ; retourne la version d'AutoCAD
; (GetPID)               ; retourne un pseudo-nombre si ACE-LT Manager n'est pas utilisé


(defun GetAcadVersion()
; warning : some functions check for R or LT
; just to distinct full / LT version
; R  xx Full AutoCAD
; LT xxxx AutoCAD LT
  (Cond
    ((= (substr (getvar "ACADVER") 1 2) "13") "R 13")
    ((= (getvar "ACADVER") "4.0") "LT 97")
    ((= (getvar "ACADVER") "15.05") "LT 2000")
    ((= (getvar "ACADVER") "16.0") "LT 2004")
    ((= (getvar "ACADVER") "16.1s (LMS Tech)") "LT 2005")
    (T (alert (strcat 
	     "BoumACAD.lsp doit être modifié pour la version courrante d'AutoCAD"
		 "\nVeuiller contacter BoumPowerSoftware en indiquant votre version"
		 "\nd'AutoCAD et la ligne suivante :"
		 "\n\n\t" (getvar "ACADVER") "\t"
	    ))
       "UNKNOWN")  ; else alert
  ) ; cond
)

;==============================================================================

; define GetPID for compatibility with ACE-LT Manager
; retourne un nombre qui devrait être unique

(if (null GetPID)
  (defun GetPID()
    (if #DummyPID
      #DummyPID
      (setq #DummyPID (getvar "TDUSRTIMER") ; build random number
            #DummyPID (fix (* 32768 (- #DummyPID (fix #DummyPID)))))
    )
  ) ; defun GetPID
)


;==============================================================================

; (StartAndWait cmdapp file) like startapp but wait untill process terminated

(cond
  ((wcmatch (ver) "ACE-LT*")
        (defun StartAndWait(cmdapp args)
          (wait (startapp cmdapp args))))
;  ((wcmatch (ver) "Visual LISP*")
;        nil)
  (T
        (defun StartAndWait(cmdapp args / f semaphore)
        ; create a semaphore and call process...
        ; wait until semaphore is deleted
        ; need StartAppAndWait.exe to lunch the application
          (setq semaphore (strcat (getenv "TEMP") "\\~StartAndWait" (itoa (getpid)) ".TMP")
                f (open semaphore "w"))
          (write-line cmdapp f) ; create semaphore
          (if (= (type args) 'STR)
            (write-line args f)
            (write-line "" f))
          (close f)
          (startapp "StartAppAndWait" semaphore)
          (while (findfile semaphore) (grread T)) ; loop until deleted...
          nil
        ))
); cond

(defun C:essai()
  (startAndWait "Notepad" nil)
)

;==============================================================================

(defun C:BOUMACAD()
  (alert (strcat
;# BPS begin SoftwareTable
    "\t\tBoum Power Software\n"
	"\nBOUMACAD\tAffiche cette liste"
;# BPS end SoftwareTable
  ))
)

;==============================================================================

(defun BpsACAD::Startup()
  (if (/= (GetAcadVersion) "UNKNOWN")
    (progn
;# BPS begin Startup Section
;      (C:BOUMACAD)
;       (load "CNStudio")
;# BPS end Startup Section
    ))
)

;==============================================================================

(prompt "\nBpsACAD version 1.01             (c) 2000, O. Baumgartner, CH 1347 Le Sentier\n")
(princ)

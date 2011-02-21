(vl-load-com)

(vl-doc-export 'getacadversion) ; (GetAcadVersion) return AcadVersion

;==============================================================================

;;; uncomment the following line to enable Kernel debugging

(setq *BpsDebug* T)

;==============================================================================

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
	     "BpsACAD.lsp doit être modifié pour la version courrante d'AutoCAD"
		 "\nVeuiller contacter www.boumpower.ch en indiquant votre version"
		 "\nd'AutoCAD et la ligne suivante :"
		 "\n\n\t" (getvar "ACADVER") "\t"
	    ))
       "UNKNOWN")  ; else alert
  ) ; cond
)

;==============================================================================

;;; Load BpsKernel

(if *BpsDebug*
  (load "BpsDebug")
  (progn 
    (defun BpsDebug(Msg id) nil)
    (defun BpsLoad(FName) (load FName))
  )
)

(Bpsload "BpsKernel-2k4")            ; !!! Kernel is AutoCAD version dependent !!!
(Bpsload "BpsSTART")

(if S::Startup 
  (progn 
    (BpsDebug "Call S::Startup" 2)
    (S::Startup)
    (BpsDebug "" -2)
  )
  (BpsDebug "!!! S::Startup not defined")
)

;==============================================================================

(if *BpsDebug* (BpsDebugEnd))

(princ) ; Silent load
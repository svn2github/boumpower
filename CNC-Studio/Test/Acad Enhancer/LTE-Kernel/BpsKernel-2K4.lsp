(vl-load-com)
(vl-doc-export 'notepad)
(vl-doc-export 'getpid)
(vl-doc-export 'getacadversion)
(vl-doc-export 'Startapp-And-Wait) 
 

; BpsKernel.LSP
; (c) 2005 , O. Baumgartner, CH-1345 Le lieu

;                         *** Boum Power Software ***


(load "Units") ; 2000, O. Baumgartner
(load "AcadInfo")

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

(if (or (null getpid) (null (getpid)))
  (defun getpid()
    (if #DummyPID
      #DummyPID
      (setq #DummyPID (getvar "TDUSRTIMER") ; build random number
            #DummyPID (fix (* 32768 (- #DummyPID (fix #DummyPID)))))
    )
  ) ; defun getpid
)


;==============================================================================

; (Startapp-And-Wait cmdapp file) like startapp but wait untill process terminated
(cond
  ((wcmatch (ver) "ACE-LT*")
        (defun Startapp-And-Wait(cmdapp args)
          (wait (startapp cmdapp args))))
;  ((wcmatch (ver) "Visual LISP*")
;        (defun Startapp-And-Wait(cmdapp args)
;          (startandwait cmdapp args)))
;  ((wcmatch (ver) "LT LISP 2004*")
;        (defun Startapp-And-Wait(cmdapp args)
;          (startandwait cmdapp args)))
  (T
        (defun Startapp-And-Wait(cmdapp args / f semaphore)
;          (prompt "\nStartapp-And-Wait ") 
;          (prompt (strcat "\n  CmdApp => " cmdapp)) 
;          (prompt (strcat "\n  Args   => " Args)) 
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
;          (prompt (strcat "\nStartappAndWait.EXE " semaphore)) 
          (startapp "StartAppAndWait" semaphore)
          (while (findfile semaphore) (grread T)) ; loop until deleted...
          nil
        ))
); cond

(defun C:NOTEPAD(/ f)
;  (prompt "\nFindfile BpsKernel-2K4") 
; (setq f (findfile "BpsKernel-2K4.lsp"))
;  (prompt "\nFound")
;  (prompt (strcat "\nNotePAD " f))
;  (prompt "\nDebug")
  (startapp "Notepad" "")
)


;==============================================================================

(defun C:TEST()
  (prompt "\nBps Kernel Loaded")
  (shell "NotePAD.exe" "")
  (prompt "\nShell return")
)

;==============================================================================

(defun C:BPSACAD()
  (alert (strcat
;# BPS begin SoftwareTable
    "\t\tBoum Power Software\n"
	"\nBOUMACAD\tAffiche cette liste"
;# BPS end SoftwareTable
  ))
)

;==============================================================================

(defun S::Startup()
  (if (/= (GetAcadVersion) "UNKNOWN")
    (progn
;# BPS begin Startup Section
;      (C:BPSACAD)
      (load "CNStudio")
;# BPS end Startup Section
    ))
)

;==============================================================================

(prompt "\nBpsKernel Version 1.11             (c) 2005, www.boumpower.ch, CH 1345 Le Lieu\n")
(princ)

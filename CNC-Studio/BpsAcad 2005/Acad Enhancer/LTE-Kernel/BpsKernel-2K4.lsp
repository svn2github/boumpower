;;; BpsKernel.LSP
;;; (c) 2004-2005 www.boumpower.ch  CH-1345 Le lieu


(vl-load-com)
(vl-doc-export 'bpskernel-ver)      ; 
(vl-doc-export 'bpskernel-version)
(vl-doc-export 'etos)		    ; conversion en chaîne
(vl-doc-export 'getpid)             ; retourne un pseudo-nombre si ACE-LT Manager n'est pas utilisé
(vl-doc-export 'Startapp-And-Wait) 

;==============================================================================

;;; !!!! CORRECT BUG !!!!
(if (null libload) 
  (progn
    (defun libload(libname) (load libname))) ; !!! MISSING FUNCTION
    (BpsDebug "!!! Defined missing (libload ...) : bug corrected" 0)
  )

;==============================================================================

(setq *BpsKernelVersion* 1.11)
 
(defun bpskernel-ver()
  *BpsKernelVersion*
)

(defun bpskernel-version()
  (strcat "Kernel-2k4 " (rtos *BpsKernelVersion*))
)

(BpsDebug (BpsKernel-Version) 0)

;==============================================================================


;;; define GetPID for compatibility with ACE-LT Manager
;;; Return a integer like PID but negativ (to differentiate from real PID)

(if (or (null getpid) (null (getpid)))
    (defun getpid()
      (if *DummyPID
        *DummyPID
        (setq *DummyPID (getvar "TDUSRTIMER") ; build random number
              *DummyPID (fix (* -32768 (- *DummyPID (fix *DummyPID)))))
      )
    ) ; defun getpid
)

(BpsDebug (strcat "AutoCAD PID=" (itoa (getpid))) 0)

;==============================================================================


;;; (etos arg mode) return string equivalent of arg
;;;                  mode=0 string without delimiter
;;;		     mode=1 string always delimited
;;;		     mode=2 string like mode=1, but allow string like /File="test"	  

(defun etos(arg mode / len)
  (cond 
    ((= 'STR (type arg))
      (cond 
        ((= mode 0) arg)
        ((= mode 1) (strcat "\"" arg "\""))
        ((= mode 2)
          (setq len (strlen arg))
          (if (and (/= 0 len) (= (substr arg 1 1) "/")  (= (substr arg Len 1) "\""))
             arg
             (strcat "\"" arg "\"")
          )
        ) 
      )) ; type 'STR
    (T (write-to-string arg))
  )
)
(BpsDebug "Defined (etos ...)" 0)

(if (null write-to-string)
  (progn 
    (defun write-to-string(arg / file fname) 
      ;    (if (= 'STR (type arg))
      ;      (setq arg (strcat "\"" arg "\"")))
      (setq file (open (setq fname (strcat (getvar "TEMPPREFIX") "$")) "w"))
      (princ arg file)
      (close file)
      (setq file (open fname "r"))
      (setq arg (read-line file))
      (close file)
      (close (open fname "w"))
      arg
    )
    (BpsDebug "!!! Defined missing (write-to-string ...)" 0)
  )
) 


;==============================================================================

;;; (Startapp-And-Wait cmdapp file) like startapp but wait untill process terminated


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
        )
        (BpsDebug "Defined (Startapp-And-Wait ...) with semaphore use" 0))
); cond

;==============================================================================

(princ) ; Silent load

;;; BpsDebug.lsp

(vl-load-com)
(vl-doc-export 'BpsDebug)      
(vl-doc-export 'BpsLoad)      
(vl-doc-export 'BpsDebugEnd)      

(defglobal *DebugIndent* 0)

(setq *DebugLog* "BpsDebug.log"
      *DebugStream* (open *DebugLog* "w"))


(defun BpsDebug(Msg id)
  (if (minusp *DebugIndent*) (setq *DebugIndent* (+ *DebugIndent* id)))
  (write-line (strcat (create-string *DebugIndent*) Msg) *DebugStream*)
  (if (> *DebugIndent* 0) (setq *DebugIndent* (+ *DebugIndent* id)))
)

(defun BpsLoad(FName)
  (BpsDebug (strcat "Load \"" (findfile (strcat FName ".lsp")) "\"...") 2)
  (load FName)
  (BpsDebug "End Load : " -2) 
)

(defun BpsDebugEnd()
  (if *DebugStream* 
    (progn
      (BpsDebug "Debug ended" 0) 
      (close *DebugStream*)
      (setq *DebugStream* nil)
      (startapp "Notepad" (findfile *DebugLog*))
  )) 
)

(BpsDebug "Debug start " 0)
(BpsDebug (strcat "ACADVER=" (getvar "ACADVER")) 0)
(BpsDebug (strcat "VER=" (ver)) 0)

(princ) ; Silent load
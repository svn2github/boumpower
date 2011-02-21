;;; BpsSTART.lsp

(vl-load-com)
(vl-doc-export 'bpsaddinfo)     ; (BpsAddInfo txt) Add txt for BPSINFO

;==============================================================================


;;; ===== AutoLoad =====

;;; Code copied from R13

;;; Check list of loaded <apptype> applications ("ads" or "arx")
;;; for the name of a certain appplication <appname>.
;;; Returns T if <appname> is loaded.

(defun ai_AppLoaded (appname apptype)
   (apply 'or
      (mapcar 
        '(lambda (j)
	    (wcmatch
               (strcase j T)
               (strcase (strcat "*" appname "*") T)
            )   
         )
	 (eval (list (read apptype)))
      )
   )
)

;;  
;;  Native Rx commands cannot be called with the "C:" syntax.  They must 
;;  be called via (command).  Therefore they require their own autoload 
;;  command.

(defun autonativeload (app cmdliste / qapp)
  (setq qapp (strcat "\"" app "\""))
  (setq initstring "\nInitialisation...")
  (mapcar
   '(lambda (cmd / nom_cmd native_cmd)
      (progn
        (setq nom_cmd (strcat "C:" cmd))
        (setq native_cmd (strcat "\"_" cmd "\""))
        (if (not (eval (read nom_cmd)))
            (eval
             (read (strcat
                    "(defun " nom_cmd "()"
                    "(setq m:err *error* *error* *merrmsg*)"
                    "(if (ai_ffile " qapp ")"
                    "(progn (princ initstring)"
                    "(_autoarxload " qapp ") (command " native_cmd "))"
                    "(ai_nofile " qapp "))"
                    "(setq *error* m:err m:err nil))"
                    ))))))
   cmdliste)
  nil
)

(defun _autoqload (quoi app cmdliste / qapp symnam)
  (setq qapp (strcat "\"" app "\""))
  (setq initstring "\nInitialisation...")
  (mapcar
   '(lambda (cmd / nom_cmd)
      (progn
        (setq nom_cmd (strcat "C:" cmd))
        (if (not (eval (read nom_cmd)))
            (eval
             (read (strcat
                    "(defun " nom_cmd "( / rtn)"
                    "(setq m:err *error* *error* *merrmsg*)"
                    "(if (ai_ffile " qapp ")"
                    "(progn (princ initstring)"
                    "(_auto" quoi "load " qapp ") (setq rtn (" nom_cmd ")))"
                    "(ai_nofile " qapp "))"
                    "(setq *error* m:err m:err nil)"
                    "rtn)"
                    ))))))
   cmdliste)
  nil
)

(defun autoload (app cmdliste)
  (_autoqload "" app cmdliste)
)

(defun autoxload (app cmdliste)
  (_autoqload "x" app cmdliste)
)

(defun autoarxload (app cmdliste)
  (_autoqload "arx" app cmdliste)
)

(defun _autoload (app)
; (princ "Auto:(load ") (princ app) (princ ")") (terpri)
  (load app)
)

(defun _autoxload (app)
; (princ "Auto:(xload ") (princ app) (princ ")") (terpri)
  (if (= app "region") (ai_select))
  (xload app)
  (if (= app "region") (ai_amegrey "~"))
)

(defun _autoarxload (app)
; (princ "Auto:(arxload ") (princ app) (princ ")") (terpri)
  (arxload app)
)

(defun ai_ffile (app)
  (or (findfile (strcat app ".lsp"))
      (findfile (strcat app ".exp"))
      (findfile (strcat app ".exe"))
      (findfile (strcat app ".arx"))
      (findfile app)
  )
)

(defun ai_nofile (filename)
  (princ
    (strcat "\nLe fichier "
            filename
            "(.lsp/.exp/.exe/.arx) n'a pas été trouvé dans votre chemin de recherche des répertoires."
    )
  )
  (princ "\nVérifiez l'installation de vos fichiers de support et essayez à nouveau.")
  (princ)
)

;==============================================================================

(defun C:BPSINFO()
  (alert (strcat
    (bpskernel-version) " (c)2004-2005 Boum Power Software / CH-1345 Le Lieu\n"
    *bpsinfo*))
)

(defun BpsAddInfo(txt)
  (setq *bpsinfo* (strcat *bpsinfo* txt))
)

(setq *bpsinfo* "\nBPSINFO\t\tAffiche cette fenêtre") ; 

;==============================================================================

(defun BpsAutoload(FName CmdList Info)
  (autoload FName CmdList)
  (BpsAddInfo Info)
)

(defun S::Startup()
  (if (/= (GetAcadVersion) "UNKNOWN")
    (progn
      (BpsAutoload "BpsUtils" '("NOTEPAD") "\nNOTEPAD\t\tLance le bloc-notes")
      (BpsAutoload "AcadInfo" '("ACADINFO") "\nACADINFO\tInformations techniques")
      (BpsAutoload "BpsTEST" '("BPSTEST") "\nBPSTEST\t\tTests")
      (BpsAutoload "CNC" '("CNC") "\nCNC\t\tProgramation CNC")
      (prompt (strcat "\n* <" (bpskernel-version) ">   active."))
      (prompt " (c)2004-2005 BoumPower Software.")
      (prompt " Use <BPSINFO>")
    ))
)

;==============================================================================


(princ) ; Silent load
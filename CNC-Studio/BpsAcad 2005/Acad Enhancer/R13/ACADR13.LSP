; Next available MSG number is    86
; MODULE_ID ACADR13_LSP_
;;;    ACADR13.LSP Version 13.1 for Release 13 
;;;
;;;    Copyright (C) 1994 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;.
;;;
;;;    Note:
;;;            This file is loaded automatically by AutoCAD every time 
;;;            a drawing is opened.  It establishes an autoloader and
;;;            other utility functions.
;;;
;;;    Globalization Note:   
;;;            We do not support autoloading applications by the native 
;;;            language command call (e.g. with the leading underscore
;;;            mechanism.)

;;;=== General Utility Functions ===

;   R12 compatibility - In R12 (acad_helpdlg) was an externally-defined 
;   ADS function.  Now it's a simple AutoLISP function that calls the 
;   built-in function (help).  It's only purpose is R12 compatibility.  
;   If you are calling it for anything else, you should almost certainly 
;   be calling (help) instead. 
 
(defun acad_helpdlg (helpfile topic)
  (help helpfile topic)
)


(defun *merr* (msg)
  (setq *error* m:err m:err nil)
  (princ)
)

(defun *merrmsg* (msg)
  (princ msg)
  (setq *error* m:err m:err nil)
  (princ)
)

;;; ===== Tutorial Utility Functions =====

;;; If you can find the Toolbook Viewer and the Toolbook file, 
;;; start this Toolbook Tutorial

(defun playtbk (book / exe full fbook)
  (setq exe   (findfile "tbook.exe"))

  (setq full (strcat book ".tbk"))
  (if exe
    (setq fbook 
      (cond
        ((findfile full))
        ((findfile book))
        (T nil) 
      )
    )
  )
  (if (and exe fbook)
    (startapp exe fbook)
    (alert "Ne peut pas lancer le didacticiel.")
  )
)

;;; ===== AutoLoad =====

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


;;;===== AutoLoad LISP Applications =====
;  Set help for those apps with a command line interface

(autoload "appload" '("appload" "appload"))

(autoload "edge"  '("arête"))
(setfunhelp "C:edge" "" "edge")

(autoload "filter" '("filter " "filter"))

(autoload "3d" '("3d" "3d" "ai_box" "ai_pyramid" "ai_wedge" "ai_dome"
                 "ai_mesh" "ai_sphere" "ai_cone" "ai_torus" "ai_dish")
)
(setfunhelp "C:3d" "" "3d")
(setfunhelp "C:ai_box" "" "3d_box")
(setfunhelp "C:ai_pyramid" "" "3d_pyramid")
(setfunhelp "C:ai__wedge" "" "3d_wedge")
(setfunhelp "C:ai_dome" "" "3d_dome")
(setfunhelp "C:ai_mesh" "" "3d_mesh")
(setfunhelp "C:ai_sphere" "" "3d_sphere")
(setfunhelp "C:ai_cone" "" "3d_cone")
(setfunhelp "C:ai_torus" "" "3d_torus")
(setfunhelp "C:ai_dish" "" "3d_dish")

(autoload "ddinsert" '("ddinsert"))

(autoload "ddattdef" '("ddattdef"))

(autoload "ddattext" '("ddattext"))

(autoload "3darray" '("3darray"))
(setfunhelp "C:3darray" "" "3darray")

(autoload "ddmodify" '("ddmodify"))

(autoload "ddchprop" '("ddchprop"))

(autoload "ddview" '("ddview"))

(autoload "ddvpoint" '("ddvpoint"))

(autoload "mvsetup" '("mvsetup"))
(setfunhelp "C:mvsetup" "" "mvsetup")

(autoload "ddosnap" '("ddosnap"))

(autoload "ddptype" '("ddptype"))

(autoload "dducsp" '("dducsp"))

(autoload "ddunits" '("ddunits"))

(autoload "ddgrips" '("ddgrips"))

(autoload "ddselect" '("ddselect"))

(autoload "ddrename" '("ddrename"))

(autoload "ddcolor" '("ddcolor"))

(autoload "xrefclip" '("xrefclip"))
(setfunhelp "C:xrefclip" "" "xrefclip")

(autoload "attredef" '("attredef"))
(setfunhelp "C:attredef" "" "attredef")

(autoload "xplode" '("xp" "xplode"))
(setfunhelp "C:xplode" "" "xplode")

;;;===== Autoload platform-specific applications =====

(if (or (wcmatch (getvar "platform") "*DOS*")
        (wcmatch (getvar "platform") "Solaris*")
        (wcmatch (getvar "platform") "HP*")
        (wcmatch (getvar "platform") "Silicon*")
        (wcmatch (getvar "platform") "IBM*"))
    (autoload "r13new" '("whatsnew" "whatsnew"))
    (autoload "tutorial" '("tutdemo" "tutclear"
				       "tutdemo" 
				       "tutclear"))
)

;;;===== AutoXLoad ADS Applications =====

(autoxload "rasterin" '( "gifin"	"riaspect"	"pcxin"	
			 "riedge"	"rigamut"	"rigrey"
			 "ribackg"	"rithresh"	"tiffin"
			"gifin" "pcxin"
			"riaspect" "ribackg"
			"riedge" "rigamut"
			"rigrey" "rithresh"
			"tiffin")
)

(autoxload "geomcal" '("cal" "cal"))

(autoxload "geom3d" '("mirror3d" "rotate3d" "align"
		      "mirror3d" "rotate3d" 
                                 "align"))

(autoxload "hpmplot" ' ("hpconfig" "hprender" "hpmplot"
			"hpconfig" "hprender" 
                                  "hpmplot"))

;;;===== AutoArxLoad Arx Applications =====

(defun AutoVisionPresent ()
  (setq AutoVisionPresent
    (list '()
      (and
          (getenv "ACAD")
          (wcmatch
            (strcase (getenv "ACAD") T)
            "*avis_sup*"
          )
          (findfile "autovis.arx")
      )
    )
  )
)

(AutoVisionPresent)

(defun autoloadrender (/ filedia cmdecho)
  (if (AutoVisionPresent)
      (progn (autoarxload "autovis" '("render"  "rpref"  "rmat"       "light" 
				      "matlib"  "replay" "saveimg"    "3dsin" 
				      "3dsout"  "vlconv" "rconfig"    "scene"
                                      "stats"   "setuv"  "showmat"    "rfileopt"
                                      "rendscr" "fog"    "background" "lsnew"
                                      "lsedit"  "lslib"
				      "rendu"
                                      "rdpref" 
                                      "litmat"
                                      "lumiere" 
                                      "biblmat"
                                      "affimg" 
                                      "sauveim"
                                      "charg3ds" 
                                      "sauve3ds"
                                      "convlink" 
                                      "rdconfig"
                                      "scene" 
                                      "rdstat"
                                      "mapping"
                                      "affmat"
                                      "rfileopt"
                                      "rdecran"
                                      "brouillard"
                                      "arriere-plan"
                                      "nvpays"
                                      "modpays"
                                      "biblpays"))
	     (autoload "anim" '("animate" "animation"
				"animer"
				"animation"))

        )
                              
       (autoarxload "render" '("render"  "rpref"   "rmat"    "light"	
                               "matlib"  "replay"  "saveimg" "3dsin"
                               "3dsout"  "vlconv"  "rconfig" "scene"
			       "showmat" "rendscr" "stats"
			      "rendu"
                              "rdpref" 
                              "litmat"
                              "lumiere" 
                              "biblmat"
                              "affimg" 
                              "sauveim" 
                              "charg3ds" 
                              "sauve3ds"
                              "convlink" 
                              "rdconfig"
                              "scene" 
                              "affmat" 
                              "rdecran" 
                              "rdstat"))
   )
)
(autoloadrender)

;;; For Windows File/Import dialogue, make sure
;;; render or autovis arx app is loaded before 3DSIN

(defun c:ai_3dsFiles (/ qapp)
   (setq m:err *error* *error* *merrmsg*)
   (if (AutoVisionPresent)
       (setq qapp "autovis")
       (setq qapp "render")
   )
   (if (not (ai_AppLoaded qapp "arx"))
       (if (ai_ffile  qapp )
	   (progn
              (prompt "\nInitialisation...")
	      (arxload qapp)
           )
           (ai_nofile qapp) 
       )
   )
   (setq *error* m:err m:err nil)
   (princ)
)


(defun C:RENDERUNLOAD ()
  (if (if (autovisionPresent)
        (arxunload "autovis" nil)
        (arxunload "render" nil)
      )
      (progn
        (autoloadrender)
        (princ "\nLe rendu a été enlevé de la mémoire. ")
      )
      (princ "\nLe rendu n'est pas chargé. ")
  )
  (princ)
)

(defun autoloadase ()
  (autonativeload "ase" '("aseadmin"	"aserows"	"aselinks"
			"aseselect"	"aseexport"	"asesqled"
		    "aseadmin" 
		    "aserows" 
		    "aselinks" 
		    "aseselect" 
		    "aseexport" 
		    "asesqled"))
)
(autoloadase)

(load "BPSAcad")

(defun S::STARTUP()
   (BpsACAD::Startup)
   (princ)
)

(princ)

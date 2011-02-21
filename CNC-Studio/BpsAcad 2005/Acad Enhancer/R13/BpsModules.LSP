;;; modules.lsp 

;;; functions to be compatible with OpenLisp packages and modules
;;; (c)2005 www.boumpower.ch


(vl-load-com)
(vl-doc-export 'require)
(vl-doc-export 'provide)


(if (null etos) (alert "modules.lsp : (etos ...) not defined")) 

(setq *modules* '())     ; list of loaded modules

(defun require(module / libname)
  (setq module (etos module)) ; convert to string
  (if (null (member module *modules*))
    (if (setq libname (findfile (strcat module ".lsp")))
      (progn
        (if (eq (load libname "ERROR") "ERROR")
          (print (strcat "Erreur chargement : " libname)))
        (setq *modules* (cons module *modules*)))
      (prompt (strcat "Erreur localisation : " module ".lsp"))
    )
  )
  module ; return string with module name
)

(defun provide(func)
  (etos func)
)


(princ) ; Silent load
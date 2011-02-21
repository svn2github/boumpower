;;; BpsDXF.lsp
;;; (c)2005 www.boumpower.ch / CH-1345 Le Lieu
;;; 

(vl-load-com)
(vl-doc-export 'dxf)      
(vl-doc-export 'BpsTrans)      

;===============================================================

;;; Extract dxf code from elist
(defun dxf(code elist) (cdr (assoc code elist)) )

;===============================================================

;;; Perform a (trans ...) for each coordinate of elist
(defun BpsTrans(elist dest / EName EType x)
  (setq EName (dxf -1 elist)
        EType (dxf 0 elist))
  (cond
    ((= etype "POINT")
      (subst (cons 10 (trans (cdr (assoc 10 elist)) EName Dest)) (assoc 10 elist) elist))
    ((= etype "CIRCLE")
      (subst (cons 10 (trans (cdr (assoc 10 elist)) EName Dest)) (assoc 10 elist) elist))
    ((or (= etype "POLYLINE") (= etype "VERTEX"))
      (subst (cons 10 (trans (cdr (assoc 10 elist)) EName Dest)) (assoc 10 elist) elist))
  )
)

;===============================================================

(princ) ; Silent load
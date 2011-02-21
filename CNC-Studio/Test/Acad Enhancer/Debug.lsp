; BpsDebug

(defun C:DEBUG()
  (setq *BPS-DEBUG-LOG* (open "console" "w"))
  (setq *BPS-DEBUG* T)
  (prompt "\nBps DEBUG active")
  (princ)
)

(defun BpsDebug(info)
  (if *BPS-DEBUG*
    (print info *BPS-DEBUG-LOG*))
)
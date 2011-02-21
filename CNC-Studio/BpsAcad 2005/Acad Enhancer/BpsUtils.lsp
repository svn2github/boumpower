;;; BpsUtils.LSP
;;; (c) 2004-2005 www.boumpower.ch  CH-1345 Le lieu


(vl-load-com)
(vl-doc-export 'notepad)            ; Call NotePAD

(require 'BpsUI)       ; User Interface 

;==============================================================================

(defun C:NOTEPAD()
  (setq *NotePAD (ustring 0 "Bloc-notes" *NotePAD nil))
  (startapp "Notepad" *NotePAD)
)

(setq *NotePAD "")

;==============================================================================

(princ) ; Silent load

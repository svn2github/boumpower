;;; BpsTEST.lsp

(vl-load-com)

(require 'BpsHTML)

(defun Test-Kernel(f)
  (html-tag "P" (GetAcadVersion) f)
)

(defun C:BpsTEST(/ f)
  (setq f (html-create "Test.htm" "Bps TEST"))
  (Test-Kernel f)
  (html-done f)
  (html-display (findfile "Test.htm"))
)
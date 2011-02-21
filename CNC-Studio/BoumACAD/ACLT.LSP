; ACLT.LSP   Auto loaded AutoLISP file for ACE-LT97 or ACE-LT98

(load "BpsACAD")

(defun S::STARTUP()
  (startapp (findFile "ACE_DEMO.EXE") (itoa (getpid))) ; add-on pour version evaluation
  (BpsACAD::STARTUP)
  (princ)
)

(prompt "\nACLT.LSP ")
(princ)

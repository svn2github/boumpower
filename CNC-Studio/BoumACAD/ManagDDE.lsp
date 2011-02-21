; ManagDDE.lsp    Support DDE via DDELISP ou MonitDDE
; (c) avril 2000, O. Baumgartner


(cond
  ((wcmatch (GetAcadVersion) "R*")
     (unit "ManagDDE" (Uses '()))
     (xload "DDELISP"))
  ((wcmatch (GetAcadVersion) "LT*")
     (unit "ManagDDE" (Uses '("MonitDDE")))
     ; Basic DDE emulation
     (defun appinit(AppName TopicName PathEXE)
       (MonitorDDE "/APPINIT" (list AppName TopicName PathEXE) T))

     (defun initiate(AppName TopicName)
       (MonitorDDE "/INITIATE" (list AppName TopicName) T))

     (defun terminate(chnl)
       (MonitorDDE "/TERMINATE" (list Chnl) T))

     (defun request(chnl ItemName)
       (MonitorDDE "/REQUEST" (list Chnl ItemName) T))

     (defun poke(Chnl ItemName Data)
       (MonitorDDE "/POKE" (list Chnl ItemName (QuoteStr Data)) T))

     (defun execute(Chnl Macro)
       (MonitorDDE "/EXECUTE" (list Chnl Macro) T))

     (defun advise(Chnl ItemName Macro)
       (MonitorDDE "/ADVISE" (list Chnl ItemName Macro) T))

     (defun unadvise(Chnl ItemName)
       (MonitorDDE "/UNADVISE" (list Chnl ItemName) T))

     (defun ddedone()
       (MonitorDDE "/DDEDONE" nil T))
  )
  (T (alert "Check Units.lsp for current AutoCAD version"))
) ; Cond

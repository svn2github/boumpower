(setq PipeArgs "\\\\.\\pipe\\AcadEnh_ARGS")
(setq PipeResult "\\\\.\\pipe\\AcadEnh_RESULT")

(defun OpenPipes()
  (Setq fRESULT (open PipeResult "r"))
)

(defun TestPipes()
  (setq fARGS (open PipeArgs "w"))
  (write-line "/TEST 34 38" fARGS)
  (close fARGS)
  (setq tmp (read (read-line fRESULT)))
  (eval tmp)
)

(defun ClosePipes()
  (Close fRESULT)
)


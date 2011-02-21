(defun QuoteStr(y)
  (prompt "\nQuoteStr")
  (strcat "\"" y "\"")
)

; QuoteStr if it's not done
; allow special form like /FILE="..."
(defun EnhStr(s / Len)
  (setq Len (strlen s))
  (prompt "\nLen=")
  (prin1 Len)
  (if (zerop Len)
    ""
    (cond
      ((and (= (substr s 1 1) "/")  (= (substr s Len 1) "\"")) s)
      (T (strcat "\"" s "\"")))
  ) ; if
)


(defun test1()
  (prompt (strcat "\n" "\"test1\""))
)

(defun test2()
  (setq #a "\"test2\"")
  (prompt (strcat "\n" #a))
)


(defun test3()
  (prompt (strcat "\n" (enhstr "test3")))
)

(defun test4()
  (setq #a "test4")
  (prompt (strcat "\n" (enhstr #a) (enhstr #a)))
)

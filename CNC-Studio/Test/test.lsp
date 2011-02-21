(defun QuoteStr(y)
  (prompt "\nQuoteStr")
  (strcat "\"" y "\"")
)

; QuoteStr if it's not done
; allow special form like /FILE="..."
(defun EnhStr(y / Len)
  (setq Len (strlen y))
  (prompt "\nLen=")
  (print Len)
  (if (zerop Len)
    ""
    (cond
      ((and (= (substr y 1 1) "/")  (= (substr y Len 1) "\"")) y)
      (T (QuoteStr y)))
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

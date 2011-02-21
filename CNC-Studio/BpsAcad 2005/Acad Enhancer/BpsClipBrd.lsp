;;; BpsClipBrd.lsp  ClipBoard support for AutoCAD including LT version
;;; (c) 2005 www.boumpower.ch / CH-1345 Le Lieu


; (clipboard_copy Fmt Data)
; (clipboard_paste Fmt)
; (clipboard_formats)
; (clipbaord_hasformat Fmt)


(require 'BpsAcadEnh)


(setq CF_TEXT 1     ; Clipboard formats
      CF_SYLK 3
      CF_DXF 77)

(defun Clipboard_copy(Fmt Data / f)
  (cond
    ((= (type Data) 'STR) ; simple Text
       (if (< (strlen Data) 80)
         (AcadEnh "/COPYCLIP" (list Fmt Data) nil)
         (progn
           (setq f (AcadEnhOpenTemp "w"))
           (write-line Data f)
           (close f)
           (AcadEnh "/COPYCLIP" (list Fmt (strcat "/FILE="
               (QuoteStr #AcadEnhTempFileName))) nil)
         )))
    ((and (= (type Data) 'LIST) (= (type (car Data)) 'STR)) ; multi-line Text
      (setq f (AcadEnhOpenTemp "w"))
      (foreach s Data (write-line s f))
      (close f)
      (AcadEnh "/COPYCLIP" (list Fmt (strcat "/FILE="
          (QuoteStr #AcadEnhTempFileName))) nil)
    )
   ) ; cond
)

(defun Clipboard_paste(Fmt)
  (AcadEnh "/CLIPPASTE" (list Fmt) T)
)

(defun Clipboard_formats()
  (AcadEnh "/CLIPFORMATS" nil T)
)

(defun Clipboard_hasformat(Fmt)
  (AcadEnh "/CLIPHASFMT" (list Fmt) T)
)


; Clipboard transfert by file

; open Clipboard file
(defun OpenClipboard()
  (setq #ClpBrdFile (AcadEnhOpenTemp "w"))
)

; Add text data
(defun AddToClipboard(Data)
  (write-line Data #ClpBrdFile)
)

; Close file and Copy to Clipboard as Fmt
(defun CloseClipboard(Fmt)
  (close #ClpBrdFile)
  (AcadEnh "/COPYCLIP" (list Fmt (strcat "/FILE="
    (QuoteStr #AcadEnhTempFileName))) nil)
)

(princ) ; Silent Load

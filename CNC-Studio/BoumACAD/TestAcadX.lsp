(defun C:TESTACADX ( / ACADX)
	(vl-load-com)

	(setq acadx
		(vla-getInterfaceObject
			(vlax-get-acad-object)
			"AcadX.Application"
		)
	)

	(if AcadX
	  (alert "AcadX successfully installed")
	  (alert "AcadX installation failed.")
	)
)
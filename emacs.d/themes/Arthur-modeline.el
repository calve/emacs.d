(setq mode-line-preformat
	      (list
	       
	       '(:eval (propertize "%b" 'face (if control-mode '(t(:inverse-video t )) nil)
		       'help-echo (buffer-file-name)))
	       (propertize "%b" 'face 'font-lock-default-face)
	       ;; line and column
	       " (" ;; '%02' to set to 2 chars at least; prevents flickering
	       (propertize "%02l" 'face 'font-lock-type-face) ","
	       (propertize "%02c" 'face 'font-lock-type-face) 
	       ") "
	       
	       "[" ;; insert vs overwrite mode, input-method in a tooltip
	       '(:eval (propertize (if overwrite-mode "Ovr" (if control-mode "Ctrl" "Ins"))
				   'face 'font-lock-preprocessor-face
				   'help-echo (concat "Buffer is in "
						      (if overwrite-mode "overwrite" (if control-mode "control" "insert") " mode"))))
	       
	       ;; was this buffer modified since the last save?
	       '(:eval (when (buffer-modified-p)
			(concat ","  (propertize "m"
						 'face 'font-lock-warning-face
						 'help-echo "Buffer has been modified"))))
	       
	       ;; is this buffer read-only?
	       '(:eval (when buffer-read-only
			 (concat ","  (propertize "ro"
						  'face 'font-lock-type-face
						  'help-echo "Buffer is read-only"))))  
	       "] "
	       
	       ;;relative position, size of file
	       "["
	       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
	       "/"
	       (propertize "%I" 'face 'font-lock-constant-face) ;; size
	       "] "
	       
	       ;; the current major mode for the buffer.
	       "["
	       
	       '(:eval (propertize "%m" 'face 'font-lock-string-face
				   'help-echo buffer-file-coding-system))
	       "] "
	       
	       ;; add the time, with the date and the emacs uptime in the tooltip
	       '(:eval (propertize (format-time-string "%H:%M")
				   'help-echo
				   (concat (format-time-string "%c; ")
					   (emacs-uptime "Uptime:%hh"))))
	       " --"
	       minor-mode-alist ;; list of minor modes
	       "%-" ;; fill with '-'
	       )
	      
	      )

(setq-default mode-line-format
	      mode-line-preformat
)


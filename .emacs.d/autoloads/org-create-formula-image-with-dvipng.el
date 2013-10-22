;; Override this function to pass -shell-escape to latex when needed (for
;; minted).

;; This function borrows from Ganesh Swami's latex2png.el
(defun org-create-formula-image-with-dvipng (string tofile options buffer)
  "This calls dvipng."
  (require 'org-latex)
  (let* ((tmpdir (if (featurep 'xemacs)
		     (temp-directory)
		   temporary-file-directory))
	 (texfilebase (make-temp-name
		       (expand-file-name "orgtex" tmpdir)))
	 (texfile (concat texfilebase ".tex"))
	 (dvifile (concat texfilebase ".dvi"))
	 (pngfile (concat texfilebase ".png"))
	 (fnh (if (featurep 'xemacs)
                  (font-height (face-font 'default))
                (face-attribute 'default :height nil)))
	 (scale (or (plist-get options (if buffer :scale :html-scale)) 1.0))
	 (dpi (number-to-string (* scale (floor (* 0.9 (if buffer fnh 140.))))))
	 (fg (or (plist-get options (if buffer :foreground :html-foreground))
		 "Black"))
	 (bg (or (plist-get options (if buffer :background :html-background))
		 "Transparent")))
    (if (eq fg 'default) (setq fg (org-dvipng-color :foreground)))
    (if (eq bg 'default) (setq bg (org-dvipng-color :background)))
    (with-temp-file texfile
      (insert (org-splice-latex-header
	       org-format-latex-header
	       org-export-latex-default-packages-alist
	       org-export-latex-packages-alist t
	       org-format-latex-header-extra))
      (insert "\n\\begin{document}\n" string "\n\\end{document}\n")
      (require 'org-latex)
      (org-export-latex-fix-inputenc))
    (let ((dir default-directory))
      (condition-case nil
	  (progn
	    (cd tmpdir)
        (if (eq org-export-latex-listings 'minted)
            (call-process "latex" nil nil nil "-shell-escape" texfile)
          (call-process "latex" nil nil nil texfile)))
	(error nil))
      (cd dir))
    (if (not (file-exists-p dvifile))
	(progn (message "Failed to create dvi file from %s" texfile) nil)
      (condition-case nil
	  (if (featurep 'xemacs)
	      (call-process "dvipng" nil nil nil
			    "-fg" fg "-bg" bg
			    "-T" "tight"
			    "-o" pngfile
			    dvifile)
	    (call-process "dvipng" nil nil nil
			  "-fg" fg "-bg" bg
			  "-D" dpi
			  ;;"-x" scale "-y" scale
			  "-T" "tight"
			  "-o" pngfile
			  dvifile))
	(error nil))
      (if (not (file-exists-p pngfile))
	  (if org-format-latex-signal-error
	      (error "Failed to create png file from %s" texfile)
	    (message "Failed to create png file from %s" texfile)
	    nil)
	;; Use the requested file name and clean up
	(copy-file pngfile tofile 'replace)
	(loop for e in '(".dvi" ".tex" ".aux" ".log" ".png" ".out") do
	      (if (file-exists-p (concat texfilebase e))
		  (delete-file (concat texfilebase e))))
	pngfile))))

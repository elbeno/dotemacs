;;------------------------------------------------------------------------------
;; Org-mode customization: allow left-alignment of numeric fields

(defcustom org-table-numeric-field-alignment "r"
  "The default alignment for fields containing numbers."
  :group 'org-table-settings
  :type 'string)

(defun my-org-table-align ()
  "Align the table at point by aligning all vertical bars."
  (interactive)
  (let ((beg (org-table-begin))
	(end (copy-marker (org-table-end))))
    (org-table-save-field
     ;; Make sure invisible characters in the table are at the right
     ;; place since column widths take them into account.
     (org-font-lock-ensure beg end)
     (move-marker org-table-aligned-begin-marker beg)
     (move-marker org-table-aligned-end-marker end)
     (goto-char beg)
     (org-table-with-shrunk-columns
      (let* ((indent (progn (looking-at "[ \t]*") (match-string 0)))
	     ;; Table's rows as lists of fields.  Rules are replaced
	     ;; by nil.  Trailing spaces are removed.
	     (fields (mapcar
		      (lambda (l)
			(and (not (string-match-p org-table-hline-regexp l))
			     (org-split-string l "[ \t]*|[ \t]*")))
		      (split-string (buffer-substring beg end) "\n" t)))
	     ;; Compute number of columns.  If the table contains no
	     ;; field, create a default table and bail out.
	     (columns-number
	      (if fields (apply #'max (mapcar #'length fields))
		(kill-region beg end)
		(org-table-create org-table-default-size)
		(user-error "Empty table - created default table")))
	     (widths nil)
	     (alignments nil))
	;; Compute alignment and width for each column.
	(dotimes (i columns-number)
	  (let* ((max-width 1)
		 (fixed-align? nil)
		 (numbers 0)
		 (non-empty 0))
	    (dolist (row fields)
	      (let ((cell (or (nth i row) "")))
		(setq max-width (max max-width (org-string-width cell)))
		(cond (fixed-align? nil)
		      ((equal cell "") nil)
		      ((string-match "\\`<\\([lrc]\\)[0-9]*>\\'" cell)
		       (setq fixed-align? (match-string 1 cell)))
		      (t
		       (cl-incf non-empty)
		       (when (string-match-p org-table-number-regexp cell)
			 (cl-incf numbers))))))
	    (push max-width widths)
	    (push (cond
		   (fixed-align?)
		   ((>= numbers (* org-table-number-fraction non-empty))
                    org-table-numeric-field-alignment) ;; normally always "r"
		   (t "l"))
		  alignments)))
	(setq widths (nreverse widths))
	(setq alignments (nreverse alignments))
	;; Store alignment of this table, for later editing of single
	;; fields.
	(setq org-table-last-alignment alignments)
	(setq org-table-last-column-widths widths)
	;; Build new table rows.  Only replace rows that actually
	;; changed.
	(dolist (row fields)
	  (let ((previous (buffer-substring (point) (line-end-position)))
		(new
		 (format "%s|%s|"
			 indent
			 (if (null row)	;horizontal rule
			     (mapconcat (lambda (w) (make-string (+ 2 w) ?-))
					widths
					"+")
			   (let ((cells	;add missing fields
				  (append row
					  (make-list (- columns-number
							(length row))
						     ""))))
			     (mapconcat #'identity
					(cl-mapcar #'org-table--align-field
						   cells
						   widths
						   alignments)
					"|"))))))
	    (if (equal new previous)
		(forward-line)
	      (insert new "\n")
	      (delete-region (point) (line-beginning-position 2)))))
	(set-marker end nil)
	(when org-table-overlay-coordinates (org-table-overlay-coordinates))
	(setq org-table-may-need-update nil))))))

(advice-add #'org-table-align :override #'my-org-table-align)

(defun my-clang-tidy-error-to-url-slug (args)
  (list (replace-regexp-in-string "^\\([^-]*\\)-" "\\1/" (car args))))

(advice-add #'lsp-cpp-flycheck-clang-tidy--show-documentation
            :filter-args #'my-clang-tidy-error-to-url-slug)

(advice-add #'flycheck-clang-tidy--show-documentation
            :filter-args #'my-clang-tidy-error-to-url-slug)

;;------------------------------------------------------------------------------
;; workaround for broken bm package
;; https://github.com/joodland/bm/issues/45

(defun my/bm-lists (&optional direction predicate)
  "Return a pair of lists giving all the bookmarks of the current buffer.
The car has all the bookmarks before the overlay center;
the cdr has all the bookmarks after the overlay center.
A bookmark implementation of `overlay-lists'.

If optional argument DIRECTION is provided, only return bookmarks
in the specified direction.

If optional argument PREDICATE is provided, it is used as a
selection criteria for filtering the lists."
  (if (null predicate)
    (setq predicate 'bm-bookmarkp))

  (overlay-recenter (point))
  (cond ((equal 'forward direction)
         (cons nil (remq nil (mapcar predicate (overlays-in (point) (point-max))))))
        ((equal 'backward direction)
         (cons (remq nil (mapcar predicate (overlays-in (point-min) (point)))) nil))
        (t
         (cons
          (remq nil (mapcar predicate (overlays-in (point-min) (point))))
          (remq nil (mapcar predicate (overlays-in (point) (point-max))))))))

(advice-add #'bm-lists
            :override #'my/bm-lists)

;;------------------------------------------------------------------------------
;; workaround for broken clipetty with local tmux
(defun my/clipetty--emit (string)
  "Emit STRING, optionally wrapped in a DCS, to an appropriate tty."
  (let ((tmux    (getenv "TMUX" (selected-frame)))
        (term    (getenv "TERM" (selected-frame)))
        (ssh-tty (or (getenv "SSH_TTY" (selected-frame)) (terminal-name))))
    (if (<= (length string) clipetty--max-cut)
        (write-region
         (clipetty--dcs-wrap string tmux term ssh-tty)
         nil
         (clipetty--tty ssh-tty tmux)
         t
         0)
      (message "Selection too long to send to terminal %d" (length string))
      (sit-for 1))))

(advice-add #'clipetty--emit
            :override #'my/clipetty--emit)

;; c++-ts-mode operators are not complete
;; added: <=>
(defvar c-ts-mode--operators
  '("=" "-" "*" "/" "+" "%" "~" "|" "&" "^" "<<" ">>" "->"
    "." "<" "<=" ">=" ">" "==" "!=" "!" "&&" "||" "-="
    "+=" "*=" "/=" "%=" "|=" "&=" "^=" ">>=" "<<=" "--" "++" "<=>")
  "C/C++ operators for tree-sitter font-locking.")

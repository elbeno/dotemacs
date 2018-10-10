(defcustom boost-sml-table-start "make_transition_table"
  "Marker for the beginning of a Boost SML table."
  :group 'boost-sml
  :type 'string
  :safe 'stringp)

(defcustom boost-sml-align-guards-beyond-events nil
  "When true, align guards at the end of all events."
  :group 'boost-sml
  :type 'boolean
  :safe 'booleanp)

(defcustom boost-sml-align-guard-closes nil
  "When true, align the closing ] of guards."
  :group 'boost-sml
  :type 'boolean
  :safe 'booleanp)

(defcustom boost-sml-spaces-in-guards 0
  "Put N spaces inside [ and ] for guards."
  :group 'boost-sml
  :type 'integer
  :safe 'integerp)

(defun align-boost-sml-part (start regexp &optional group spacing)
 (goto-char start)
 (forward-sexp)
 (backward-char)
 (align-regexp start (point) regexp group spacing))

(defun position-boost-sml-guard-closes (start)
  (goto-char start)
  (forward-sexp)
  (let ((end (point)))
    (goto-char start)
    (while (re-search-forward "[[:space:]]*\\]" end t)
      (goto-char (match-beginning 0))
      (when (looking-at "[[:space:]]*")
        (replace-match (make-string boost-sml-spaces-in-guards ?\s))
        (goto-char (+ (match-end 0) 2))))))

(defun align-boost-sml (start)
  (forward-sexp)
  (indent-region start (point))
  (align-boost-sml-part start "[[:space:]]*\\([[:space:]]\\)\\*[^/]" 1 0) ;; back up default *
  (align-boost-sml-part start "\\([[:space:]]*\\)\\+") ;; align +, one space before
  (align-boost-sml-part start "\\+\\([[:space:]]*\\)") ;; align +, one space after
  (align-boost-sml-part start "\\([[:space:]]*\\)\\[") ;; align [, one space before
  (align-boost-sml-part start "\\[\\([[:space:]]*\\)" 1 boost-sml-spaces-in-guards) ;; align [
  (if boost-sml-align-guard-closes
      (align-boost-sml-part start "\\([[:space:]]*\\)\\]" 1 boost-sml-spaces-in-guards) ;; align ]
    (position-boost-sml-guard-closes start))
  (align-boost-sml-part start "[^/*]\\([[:space:]]*\\)[/][^/*]") ;; align /, one space before
  (align-boost-sml-part start "[^/*][/]\\([[:space:]]*\\)[^/*]") ;; align /, one space after
  (when boost-sml-align-guards-beyond-events
    (align-boost-sml-part start "\\([[:space:]]+\\)[/[][^/*]") ;; align [ or /
    (align-boost-sml-part start "\\([[:space:]]+\\)/[[:space:]]")) ;; re-align /
  (align-boost-sml-part start "\\([[:space:]]*\\)=") ;; align =, one space before
  (align-boost-sml-part start "=\\([[:space:]]*\\)")) ;; align =, one space after

;;;###autoload
(defun find-and-align-boost-sml ()
  "Align all Boost.SML tables in the buffer using align-boost-sml.
This function is controlled by variables in the customization group boost-sml."
  (interactive)
  (save-excursion
    (ignore-errors
      (goto-char (point-min))
      (while (search-forward boost-sml-table-start nil t)
        (align-boost-sml (point))))))

(provide 'boost-sml)

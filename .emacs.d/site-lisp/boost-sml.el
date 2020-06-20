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
  "When true, align the closing ] of guards. This only takes effect
boost-sml-align-guards-beyond-events is true (so guard opens are also aligned)."
  :group 'boost-sml
  :type 'boolean
  :safe 'booleanp)

(defcustom boost-sml-spaces-in-guards 0
  "Put N spaces inside [ and ] for guards."
  :group 'boost-sml
  :type 'integer
  :safe 'integerp)

(defcustom boost-sml-align-actions-beyond-guards nil
  "When true, align actions at the end of all guards."
  :group 'boost-sml
  :type 'boolean
  :safe 'booleanp)

(defcustom boost-sml-spaces-after-default-star 0
  "The number of spaces to put after the default state marker (*)."
  :group 'boost-sml
  :type 'integer
  :safe 'integerp)

(defcustom boost-sml-insert-clang-format-guards t
  "When true, insert clang-format guard comments to prevent
clang-format from meddling with the formatting."
  :group 'boost-sml
  :type 'boolean
  :safe 'booleanp)

(defcustom boost-sml-align-eol-commas nil
  "When true, align the commas at the end of lines."
  :group 'boost-sml
  :type 'boolean
  :safe 'booleanp)

(defcustom boost-sml-spaces-in-action-parens 0
  "Put N spaces inside ( and ) around actions."
  :group 'boost-sml
  :type 'integer
  :safe 'integerp)

(defcustom boost-sml-align-parens-around-actions nil
  "When true, align the parens placed around actions."
  :group 'boost-sml
  :type 'boolean
  :safe 'booleanp)

(defcustom boost-sml-consistent-parens-around-actions nil
  "When true, put parens around all actions if any have parens."
  :group 'boost-sml
  :type 'boolean
  :safe 'booleanp)

(require 'cl-lib)
(require 'rx)
(require 'subr-x)

(defun boost-sml/whitespace-p (c)
  (string-match-p "[[:space:]]" (char-to-string c)))

(defun boost-sml/skip-whitespace ()
  (while (boost-sml/whitespace-p (char-after)) (forward-char)))

(defun boost-sml/skip-to (sep)
  (while (not (looking-at sep))
    (cond ((eolp) (forward-char))
          ((looking-at "[[:space:]]") (boost-sml/skip-whitespace))
          (t (forward-sexp)))))

(defun boost-sml/next-token ()
  (let ((start (point)))
    (boost-sml/skip-whitespace)
    (cond ((eolp) t)
          ((looking-at (rx "/*")) (forward-comment 1))
          ((looking-at "//") (end-of-line))
          ((looking-at (rx (or "{" "<" "(" "[" "\"" "'"))) (forward-sexp))
          (t (forward-same-syntax)))
    (string-trim (buffer-substring-no-properties start (point)))))

(defun boost-sml/default-state-token ()
  (let ((start (point)))
    (boost-sml/skip-whitespace)
    (when (looking-at (rx "*")) (forward-char))
    (buffer-substring-no-properties start (point))))

(defun boost-sml/guard-token ()
  (boost-sml/skip-whitespace)
  (if (looking-at (rx "[")) (boost-sml/next-token) ""))

(defun boost-sml/action-separator-token ()
  (boost-sml/skip-whitespace)
  (if (looking-at (rx "/")) (boost-sml/next-token) ""))

(defun boost-sml/transition-separator-token ()
  (boost-sml/skip-whitespace)
  (if (looking-at (rx "=")) (boost-sml/next-token) ""))

(defun boost-sml/eol-token ()
  (boost-sml/skip-whitespace)
  (if (looking-at (rx ",")) (boost-sml/next-token) ""))

(defun boost-sml/collect-until-including (pred)
  (let ((start (point))
        (next (boost-sml/next-token)))
    (while (not (funcall pred next))
      (setq next (boost-sml/next-token)))
    (let ((s (buffer-substring-no-properties start (point))))
      (list (string-trim-right (string-remove-suffix next (string-trim s)))
            next))))

(defun boost-sml/collect-until-excluding (pred)
  (let ((start (point))
        (next (boost-sml/next-token)))
    (while (not (funcall pred next))
      (setq next (boost-sml/next-token)))
    (backward-char (length next))
    (let ((s (buffer-substring-no-properties start (point))))
      (list (string-trim s)))))

(defun boost-sml/is-event-separator (s)
  (string-equal s "+"))

(defun boost-sml/is-guard (s)
  (and (> (length s) 0) (string-equal (substring s 0 1) "[")))

(defun boost-sml/is-action-separator (s)
  (string-equal s "/"))

(defun boost-sml/is-transition-separator (s)
  (string-equal s "="))

(defun boost-sml/is-eol (s)
  (or (= 0 (length s))
      (string-equal s ",")
      (string-prefix-p ")" s)))

;; a line is tokenized into:
;; 0: default state star, if any
;; 1: state
;; 2: event separator, if any (+)
;; 3: event
;; 4: guard, if any
;; 5: action separator, if any (/)
;; 6: action, if any
;; 7: transition separator, if any (=)
;; 8: new state, if any
;; 9: line terminator, if any (,)

(defun boost-sml/tokenize-line ()
  (beginning-of-line)
  (setq l (list (boost-sml/default-state-token)))
  (nconc l (boost-sml/collect-until-including
            (lambda (tok) (or (boost-sml/is-event-separator tok)
                         (boost-sml/is-eol tok)))))
  (nconc l (boost-sml/collect-until-excluding
            (lambda (tok) (or (boost-sml/is-guard tok)
                         (boost-sml/is-action-separator tok)
                         (boost-sml/is-transition-separator tok)
                         (boost-sml/is-eol tok)))))
  (nconc l (list (boost-sml/guard-token)))
  (nconc l (list (boost-sml/action-separator-token)))
  (nconc l (boost-sml/collect-until-excluding
            (lambda (tok) (or (boost-sml/is-transition-separator tok)
                         (boost-sml/is-eol tok)))))
  (nconc l (list (boost-sml/transition-separator-token)))
  (nconc l (boost-sml/collect-until-excluding
            (lambda (tok) (boost-sml/is-eol tok))))
  (nconc l (list (boost-sml/eol-token)))
  l)

(defun boost-sml/next-line-valid ()
  (let ((end-of-table (rx (and (* space) ")"))))
    (when (not (looking-at end-of-table))
      (forward-line))
    (not (looking-at end-of-table))))

(defun boost-sml/transform-nth (lists n f)
  (mapc (lambda (l) (setf (nth n l) (funcall f (nth n l)))) lists))

(defun boost-sml/reduce-nth (lists n f :initial-value init)
  (let ((nths (mapcar (lambda (l) (nth n l)) lists)))
    (cl-reduce f nths :initial-value init)))

(defun boost-sml/max-length-at-index (lists n)
  (boost-sml/reduce-nth lists n (lambda (acc s) (max acc (length s))) :initial-value 0))

(defun boost-sml/right-pad-string (s desired-len left-pad right-pad)
  (let ((unpadded (if (< (length s) desired-len)
                      (concat s (make-string (- desired-len (length s)) ?\s))
                    s)))
    (concat (make-string left-pad ?\s)
            unpadded
            (make-string right-pad ?\s))))

(cl-defun boost-sml/left-align-at (lines n &optional (left-pad 0) (right-pad 0))
  (let ((len (boost-sml/max-length-at-index lines n)))
    (when (> len 0)
      (boost-sml/transform-nth
       lines n
       (lambda (s) (boost-sml/right-pad-string s len left-pad right-pad))))))

(defun boost-sml/align-default-stars (lines)
  (boost-sml/left-align-at lines 0 0 boost-sml-spaces-after-default-star))

(defun boost-sml/align-states (lines)
  (boost-sml/left-align-at lines 1))

(defun boost-sml/align-event-separators (lines)
  (boost-sml/left-align-at lines 2 1 1))

(defun boost-sml/align-events (lines)
  (boost-sml/left-align-at lines 3))

(defun boost-sml/align-guards (lines)
  (boost-sml/left-align-at lines 4 1))

(defun boost-sml/event-and-guard (line)
  (concat (nth 3 line) (nth 4 line)))

(defun boost-sml/align-events-with-guards (lines)
  (let* ((event-guards (mapcar #'boost-sml/event-and-guard lines))
         (max-len (reduce #'max (mapcar #'length event-guards) :initial-value 0)))
    (mapc (lambda (l)
            (let ((pad-len (- max-len (length (boost-sml/event-and-guard l)))))
                (setf (nth 4 l)
                      (boost-sml/right-pad-string (nth 4 l) (length (nth 4 l)) 1 pad-len))))
          lines)))

(defun boost-sml/align-action-separators (lines)
  (boost-sml/left-align-at lines 5 1 1))

(defun boost-sml/align-actions (lines)
  (boost-sml/left-align-at lines 6))

(defun boost-sml/align-transition-separators (lines)
  (boost-sml/left-align-at lines 7 1 1))

(defun boost-sml/align-new-states (lines)
  (boost-sml/left-align-at lines 8))

(defun boost-sml/assemble-line (line)
  (string-trim-right (apply #'concat line)))

(defun boost-sml/fix-line-terminator (line)
   (let ((l (boost-sml/assemble-line line)))
     (when (and (> (length l) 0)
                (not (string-prefix-p "//" (string-trim l))))
       (setf (nth 9 line) ","))))

(defun boost-sml/fix-line-terminators (lines)
  (mapc #'boost-sml/fix-line-terminator lines)
  (setf (nth 9 (car (last lines))) ""))

(defun boost-sml/put-spaces-in-guard (line)
  (let ((guard (nth 4 line)))
    (setf (nth 4 line)
          (if (> (length guard) 0)
              (let ((bare-guard (string-trim (substring guard 1 (1- (length guard)))))
                    (spaces (make-string boost-sml-spaces-in-guards ?\s)))
                (concat "[" spaces bare-guard spaces "]"))
            guard))))

(defun boost-sml/put-spaces-in-guards (lines)
  (mapc #'boost-sml/put-spaces-in-guard lines))

(defun boost-sml/expand-guard (len line)
  (let ((guard (nth 4 line)))
    (setf (nth 4 line)
          (if (> (length guard) 0)
              (let ((untermed-guard (substring guard 0 (1- (length guard))))
                    (spaces (make-string (- len (length guard)) ?\s)))
                (concat untermed-guard spaces "]"))
            guard))))

(defun boost-sml/align-guard-closes (lines)
  (let ((max-len (reduce #'max (mapcar (lambda (l) (length (nth 4 l))) lines) :initial-value 0)))
    (mapc (lambda (l) (boost-sml/expand-guard max-len l)) lines)))

(defun boost-sml/put-parens-around-action (line)
  (let ((action (nth 6 line)))
    (setf (nth 6 line)
          (if (> (length action) 0)
              (let ((bare-action
                     (if (string-prefix-p "(" action)
                         (string-trim (substring action 1 (1- (length action))))
                       action))
                    (spaces (make-string boost-sml-spaces-in-action-parens ?\s)))
                (concat "(" spaces bare-action spaces ")"))
            action))))

(defun boost-sml/put-parens-around-actions (lines)
  (when (boost-sml/reduce-nth
         lines 6
         (lambda (acc action) (or acc (string-prefix-p "(" action)))
           :initial-value t)
    (mapc #'boost-sml/put-parens-around-action lines)))

(defun boost-sml/expand-action (len line)
  (let ((action (nth 6 line)))
    (setf (nth 6 line)
          (if (string-prefix-p "(" action)
              (let ((untermed-action (substring action 0 (1- (length action))))
                    (spaces (make-string (- len (length action)) ?\s)))
                (concat untermed-action spaces ")"))
            action))))

(defun boost-sml/align-action-closes (lines)
  (let ((max-len (reduce #'max (mapcar (lambda (l) (length (nth 6 l))) lines) :initial-value 0)))
    (mapc (lambda (l) (boost-sml/expand-action max-len l)) lines)))

(defun boost-sml/put-spaces-in-action (line)
  (let ((action (nth 6 line)))
    (setf (nth 6 line)
          (if (string-prefix-p "(" action)
              (let ((bare-action (string-trim (substring action 1 (1- (length action)))))
                    (spaces (make-string boost-sml-spaces-in-action-parens ?\s)))
                (concat "(" spaces bare-action spaces ")"))
            action))))

(defun boost-sml/put-spaces-in-actions (lines)
  (mapc #'boost-sml/put-spaces-in-action lines))

(defun boost-sml/align-lines (lines)
  (boost-sml/put-spaces-in-guards lines)
  (when (and boost-sml-align-guard-closes boost-sml-align-guards-beyond-events)
    (boost-sml/align-guard-closes lines))

  (boost-sml/put-spaces-in-actions lines)
  (when boost-sml-consistent-parens-around-actions
    (boost-sml/put-parens-around-actions lines))
  (when boost-sml-align-parens-around-actions
    (boost-sml/align-action-closes lines))

  (boost-sml/align-default-stars lines)
  (boost-sml/align-states lines)
  (boost-sml/align-event-separators lines)

  (if boost-sml-align-guards-beyond-events
    (progn (boost-sml/align-events lines)
           (boost-sml/align-guards lines))
    (boost-sml/align-events-with-guards lines))

  (boost-sml/align-action-separators lines)
  (boost-sml/align-actions lines)
  (boost-sml/align-transition-separators lines)
  (when boost-sml-align-eol-commas
    (boost-sml/align-new-states lines))
  (boost-sml/fix-line-terminators lines))

(defun boost-sml/replace-next-buffer-line-with-line (line)
  (forward-line)
  (when (not (eolp)) (kill-line))
  (insert (boost-sml/assemble-line line)))

(defun boost-sml/rest-of-current-line ()
  (if (bolp)
      ""
    (let ((start (point)))
      (end-of-line)
      (buffer-substring-no-properties start (point)))))

(defun boost-sml/align-table-at (start)
  (goto-char start)
  (let ((lines (cl-loop while (boost-sml/next-line-valid)
                        collecting (boost-sml/tokenize-line))))
    (let ((trailing-part (boost-sml/rest-of-current-line)))
      (boost-sml/align-lines lines)
      (goto-char start)
      (mapc #'boost-sml/replace-next-buffer-line-with-line lines)
      (insert trailing-part))))

(defun boost-sml/insert-and-indent (s)
  (beginning-of-line)
  (insert (concat s "\n"))
  (forward-line -1)
  (let ((start (point)))
    (end-of-line)
    (indent-region start (point))))

(defun boost-sml/insert-clang-format-guards (start)
  (goto-char start)
  (forward-line -1)
  (when (not (looking-at "[[:space:]]*// clang-format off"))
    (forward-line)
    (boost-sml/insert-and-indent "// clang-format off"))
  (search-forward boost-sml-table-start nil t)
  (boost-sml/skip-to "(")
  (forward-sexp)
  (forward-line)
  (when (not (looking-at "[[:space:]]*// clang-format on"))
    (boost-sml/insert-and-indent "// clang-format on")))

;;;###autoload
(defun find-and-align-boost-sml ()
  (interactive)
  (save-excursion
    (ignore-errors
      (goto-char (point-min))
      (while (search-forward boost-sml-table-start nil t)
        (boost-sml/skip-to "(")
        (let ((start (point)))
          (forward-sexp)
          (indent-region start (point))
          (boost-sml/align-table-at start)
          (when boost-sml-insert-clang-format-guards
            (boost-sml/insert-clang-format-guards start)))))))

(provide 'boost-sml)

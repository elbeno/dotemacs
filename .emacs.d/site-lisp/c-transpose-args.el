;;------------------------------------------------------------------------------
;; Transpose function args
(defun c-forward-to-argsep ()
  "Move to the end of the current c function argument.
Returns point."
  (interactive)
  (while
    (progn (comment-forward most-positive-fixnum)
      (looking-at "[^,)>}]"))
    (forward-sexp))
  (point))

(defun c-backward-to-argsep ()
  "Move to the beginning of the current c function argument.
Returns point."
  (interactive)
  (let ((pt (point)) cur)
    (up-list -1)
    (forward-char)
    (while
      (progn
        (setq cur (point))
        (> pt (c-forward-to-argsep)))
      (forward-char))
    (goto-char cur)))

(defun c-transpose-args-direction (is_forward)
  "Transpose two arguments of a c-function.
The first arg is the one with point in it."
  (interactive)
  (let*
      (;; only different to pt when not 'is_forward'
       (pt-original (point))
       (pt
        (progn
          (when (not is_forward)
            (goto-char (- (c-backward-to-argsep) 1))
            (unless (looking-at ",")
              (goto-char pt-original)
              (user-error "Argument separator not found")))
          (point)))
       (b (c-backward-to-argsep))
       (sep
        (progn (goto-char pt)
               (c-forward-to-argsep)))
       (e
        (progn
          (unless (looking-at ",")
            (goto-char pt-original)
            (user-error "Argument separator not found"))
          (forward-char)
          (c-forward-to-argsep)))
       (ws-first
        (buffer-substring-no-properties
         (goto-char b)
         (progn (skip-chars-forward "[[:space:]\n]")
                (point))))
       (first (buffer-substring-no-properties (point) sep))
       (ws-second
        (buffer-substring-no-properties
         (goto-char (1+ sep))
         (progn (skip-chars-forward "[[:space:]\n]")
                (point))))
       (second (buffer-substring-no-properties (point) e)))
    (delete-region b e)
    (insert ws-first second "," ws-second first)

    ;; Correct the cursor location to be on the same character.
    (if is_forward
        (goto-char
         (+
          ;; word start.
          (- (point) (length first))
          ;; Apply initial offset within the word.
          (- pt b (length ws-first))))
      (goto-char
       (+
        b (length ws-first)
        ;; Apply initial offset within the word.
        (- pt-original (+ pt 1 (length ws-second))))))))

;;;###autoload
(defun c-transpose-args-forward () (interactive) (c-transpose-args-direction t))
;;;###autoload
(defun c-transpose-args-backward () (interactive) (c-transpose-args-direction nil))

;;;###autoload
(defun c-transpose-args (prefix)
  "Transpose argument at point with the argument before it.
With prefix arg ARG, transpose with the argument after it."
  (interactive "P")
  (cond ((not prefix) (c-transpose-args-backward))
        (t (c-transpose-args-forward))))

(provide 'c-transpose-args)

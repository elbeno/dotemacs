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

(defun my/ts-transpose-nodes (n1 n2 arg)
  (when (and n1 n2)
    (let* ((b1 (treesit-node-start n1))
           (e1 (treesit-node-end n1))
           (b2 (treesit-node-start n2))
           (e2 (treesit-node-end n2))
           (s1 (buffer-substring b1 e1))
           (s2 (buffer-substring b2 e2)))
      (delete-region b2 e2)
      (goto-char b2)
      (insert s1)
      (delete-region b1 e1)
      (goto-char b1)
      (insert s2)
      (goto-char (if arg (1- b1) e2)))))

(defun my/cpp-ts-transpose-args (arg)
  (let* ((parent-is-arg-list
          (lambda (node) (let ((parent-type (treesit-node-type (treesit-node-parent node))))
                      (or (string-equal parent-type "argument_list")
                          (string-equal parent-type "parameter_list")
                          (string-equal parent-type "template_argument_list")
                          (string-equal parent-type "template_parameter_list")
                          (string-equal parent-type "initializer_list")))))
         (leaf-node (treesit-node-at (point)))
         (arg-node (or (treesit-parent-until leaf-node parent-is-arg-list)
                       leaf-node)))
    (cond ((string-equal (treesit-node-text arg-node) "(") nil)
          ((string-equal (treesit-node-text arg-node) ")") nil)
          ((string-equal (treesit-node-text arg-node) "{") nil)
          ((string-equal (treesit-node-text arg-node) "}") nil)
          ((string-equal (treesit-node-text arg-node) "<") nil)
          ((string-equal (treesit-node-text arg-node) ">") nil)
          ((string-equal (treesit-node-text arg-node) "[") nil)
          ((string-equal (treesit-node-text arg-node) "]") nil)
          ((string-equal (treesit-node-text arg-node) ",")
           (my/ts-transpose-nodes (treesit-node-prev-sibling arg-node)
                                  (treesit-node-next-sibling arg-node)
                                  arg))
          (t (my/ts-transpose-nodes (treesit-node-prev-sibling (treesit-node-prev-sibling arg-node))
                                    arg-node arg)))))

;;;###autoload
(defun c-transpose-args (arg)
  "Transpose argument around point, leaving point after both.
With prefix arg, leave point before both."
  (interactive "*P")
  (if (derived-mode-p 'c++-ts-mode)
      (my/cpp-ts-transpose-args arg)
    (cond ((not arg) (c-transpose-args-backward))
          (t (c-transpose-args-forward)))))

(provide 'c-transpose-args)

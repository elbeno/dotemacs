;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Git interactions

;; magit
(use-package magit
  :ensure t
  :bind
  (("C-c g" . magit-status)
   ("C-c B" . magit-blame)))

(use-package git-modes
  :ensure t)

;; git time machine
(use-package git-timemachine
  :ensure t
  :bind
  (("C-c h" . git-timemachine-toggle)))

;; git messenger
(use-package git-messenger
  :ensure t
  :bind
  (("C-c b" . git-messenger:popup-message))
  :config
  (bind-key "m" 'git-messenger:copy-message git-messenger-map)
  ;; Enable magit-commit-mode after typing 's', 'S', 'd'
  (add-hook 'git-messenger:popup-buffer-hook #'magit-commit-mode))

;; on-the-fly diff highlighting
(use-package diff-hl
  :ensure t
  :config
  (setq diff-hl-side 'right)
  (diff-hl-flydiff-mode)
  :hook
  (prog-mode . diff-hl-mode))

;; prevent annoying reverts during conflict editing
(setq auto-revert-vc-info nil)

;; get url links
(use-package git-link
  :ensure t
  :bind
  (("C-c u" . git-link)))

;; display TODOs
(use-package magit-todos
  :ensure t
  :after magit
  :config
  (setq magit-todos-insert-after '(bottom))
  (magit-todos-mode 1))

;; speed up magit refresh
(use-package magit-prime
  :init (my/vc-install "Azkae/magit-prime")
  :after magit
  :config
  (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache))

;; use forge for github etc
(use-package forge
  :after magit
  :ensure t
  :config
  (setq auth-sources '("~/.authinfo")))

;; be ready to edit commit messages
(require 'magit-commit)

;; difftastic for diffing
;; from https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html
(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))

(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(eval-after-load 'magit-status-mode
  (transient-append-suffix 'magit-diff "d"
    '("D" "Difftastic Diff" th/magit-diff-with-difftastic)))

;;------------------------------------------------------------------------------
;; Get rid of all buffers, going back to just *scratch*
(defun nuke-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapc (lambda (x) (kill-buffer x))
    (buffer-list))
  (delete-other-windows))

;;------------------------------------------------------------------------------
;; Evaluate s-exp at point and replace it with its value
;; cf. eval-last-sexp bound to C-x C-e
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;------------------------------------------------------------------------------
;; Toggle horizontal/vertical window split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;------------------------------------------------------------------------------
;; which minor modes are active?
(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

;;------------------------------------------------------------------------------
;; Insert current time/date
(defun insert-current-time (prefix)
  "Insert the current date. With prefix-argument, use 24h format.
   With two prefix arguments, write out an ISO 8601 date and
   time."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%I:%M:%S %p")
                 ((equal prefix '(4)) "%T")
                 ((equal prefix '(16)) "%FT%T%z"))))
    (insert (format-time-string format))))

(defun insert-current-date (prefix)
  "Insert the current date. With prefix-argument, use ISO 8601
   format. With two prefix arguments, write out the day and month
   name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%x")
                 ((equal prefix '(4)) "%F")
                 ((equal prefix '(16)) "%A, %d %B %Y"))))
    (insert (format-time-string format))))

;;------------------------------------------------------------------------------
;; Insert generated UUIDs
(random t)

(defun random-ms-uuid ()
  (format "%04x%04x-%04x-4%s-%s-%06x%06x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (substring (format "%04x" (random (expt 16 4))) 1)
          (concat
           (let ((n (random 4)))
             (substring "89ab" n (1+ n)))
           (substring (format "%04x" (random (expt 16 4))) 1))
          (random (expt 16 6))
          (random (expt 16 6))))

(defun random-xcode-uuid ()
  (format "%04X%04X%04X%04X%04X%04X"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))))

(defun insert-uuid (prefix)
  "Insert a random universally unique identifier (UUID). A UUID
is a 128-bit (16 byte) number formatted in a certain way.

Example of a UUID: 1df63142-a513-X850-Y1a3-535fc3520c3d
Where X is 4 and Y is one of {8,9,a,b}.

With a prefix argument, insert a random UUID suitable for use in
XCode projects. An XCode UUID is a 96-bit (12 byte) number
formatted as a hex string.

Example of an XCode UUID: a513b85041a3535fc3520c3d."
  (interactive "P")
  (insert
   (cond
    ((not prefix) (random-ms-uuid))
    ((equal prefix '(4)) (random-xcode-uuid)))))

;;------------------------------------------------------------------------------
;; copy various things to kill ring
(defun gk-copy-buffer-file-name ()
  "Push the buffer's file name to the ‘kill-ring’."
  (interactive)
  (if-let* ((fil (buffer-file-name)))
      (with-temp-buffer
        (insert fil)
        (clipboard-kill-ring-save (point-min) (point-max))
        (message fil))
    (error "Buffer not visiting a file.")))

(defun gk-copy-last-message ()
  "Copy-as-kill the last echoed message."
  (interactive)
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char (point-max))
      (forward-line -1)
      (clipboard-kill-ring-save
       (line-beginning-position) (line-end-position)))))

;;------------------------------------------------------------------------------
;; get the paths of all the submodules in the current git repo
(defun my-git-submodule-paths ()
  "Get a list of all the git submodule paths in the current
(projectile-project-root)."
  (interactive)
  (let* ((root (projectile-project-root)))
    (split-string (shell-command-to-string
                   (concat "cd " root " && "
                           "git submodule --quiet foreach 'echo $path'")))))

;; advise projectile-ripgrep: don't search in submodules
(defun my-projectile-ripgrep (func &rest args)
  (let ((saved projectile-globally-ignored-directories))
    (setq projectile-globally-ignored-directories
          (append projectile-globally-ignored-directories (my-git-submodule-paths)))
    (apply func args)
    (setq projectile-globally-ignored-directories saved)))

(advice-add #'projectile-ripgrep :around #'my-projectile-ripgrep)

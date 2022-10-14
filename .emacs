;;------------------------------------------------------------------------------
;; debugging
(setq debug-on-error nil)
;; plenty of memory, GC threshold is 100MB
(setq gc-cons-threshold 100000000)

;; this file's true directory
(setq dotfile-dir (file-name-directory
                   (file-chase-links
                    (or load-file-name
                        (buffer-file-name)))))

;; my stuff is in .emacs.d
(add-to-list 'load-path (concat dotfile-dir ".emacs.d/"))
;; 3rd party stuff is in site-lisp
(add-to-list 'load-path (concat dotfile-dir ".emacs.d/site-lisp/"))
;; packages
(setq package-user-dir (concat dotfile-dir ".emacs.d/packages/"))
;; extra binaries (if needed) are in site-bin
(add-to-list 'exec-path (concat dotfile-dir ".emacs.d/site-bin/"))
;; themes
(setq custom-theme-directory (concat dotfile-dir ".emacs.d/themes/"))

;;------------------------------------------------------------------------------
;; apply custom variables
(let ((custom-file (concat dotfile-dir ".emacs.d/custom.el")))
  (load custom-file))

;;------------------------------------------------------------------------------
;; custom stuff is per-installation/work private
(defun files-in-below-directory (directory)
  "List the .el files in DIRECTORY and in its sub-directories."
  ;; Although the function will be used non-interactively,
  ;; it will be easier to test if we make it interactive.
  ;; The directory will have a name such as
  ;;  "/usr/local/share/emacs/22.1.1/lisp/"
  (interactive "directory name: ")
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename ends in `.el'
       ;; and if so, append its name to a list.
       ((equal ".el" (substring (car (car current-directory-list)) -3))
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()
          ;; else descend into the directory and repeat the process
          (setq el-files-list
                (append
                 (files-in-below-directory
                  (car (car current-directory-list)))
                 el-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    el-files-list))

(if (file-directory-p ".emacs.d/custom/")
    (mapc 'load (files-in-below-directory (concat dotfile-dir ".emacs.d/custom/"))))

;;------------------------------------------------------------------------------
;; package setup
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("stable-melpa" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 ("org-contrib" . "http://elpa.nongnu.org/nongnu/")
			 ("cselpa" . "https://elpa.thecybershadow.net/packages/")))

(package-initialize)
(setq package-enable-at-startup nil)

(unless package-archive-contents
  (message "Refreshing package archives...")
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

(require 'use-package)
(setq use-package-minimum-reported-time 0
      use-package-verbose t)

(setq personal-keybindings nil)

;;------------------------------------------------------------------------------
;; Startup profiling
;; (use-package esup
;;   :ensure t
;;   :defer t)

;;------------------------------------------------------------------------------
;; Common settings
(load "common.el")

;;------------------------------------------------------------------------------
;; Graphic or terminal mode?
(if (display-graphic-p)
  (load "graphic-display.el")
  (load "terminal.el"))

;;------------------------------------------------------------------------------
;; Minor modes
(load "minor-modes.el")

;;------------------------------------------------------------------------------
;; Colors
(load "colors.el")

;;------------------------------------------------------------------------------
;; Global key bindings
(load "global-keys.el")

;;------------------------------------------------------------------------------
;; Major modes
(load "major-modes.el")

;;------------------------------------------------------------------------------
;; C++
(load "cpp-modes.el")

;;------------------------------------------------------------------------------
;; Python
(load "python-modes.el")

;;------------------------------------------------------------------------------
;; Git
(load "git-modes.el")

;;------------------------------------------------------------------------------
;; Org-mode stuff
(load "org-modes.el")

;;------------------------------------------------------------------------------
;; Byte-compile elisp on save
(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

;;------------------------------------------------------------------------------
;; My stuff
(load "misc")

(bind-key "C-x C-S-e" 'eval-and-replace)
(bind-key "C-c C-w" 'toggle-window-split)
(bind-key "C-c d" 'insert-current-date)
(bind-key "C-c t" 'insert-current-time)
(bind-key "C-c u" 'insert-uuid)

;;------------------------------------------------------------------------------
;; Graphic window settings
(when (display-graphic-p)
  (setq default-frame-height (frame-height))
  (setq default-frame-alist
        (append
         `((width . ,column-wrap-hard)
           (height . ,default-frame-height))
         default-frame-alist))

  (size-frame-default))

;;------------------------------------------------------------------------------
;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Restore GC threshold
(setq gc-cons-threshold 800000)

(put 'narrow-to-region 'disabled nil)

;;------------------------------------------------------------------------------
;; Hacks & late-bound overrides
(load "hacks.el")

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:


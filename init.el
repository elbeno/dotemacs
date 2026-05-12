;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; my stuff is in .emacs.d
(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))
;; 3rd party stuff is in site-lisp
(add-to-list 'load-path (expand-file-name "elisp/site-lisp/" user-emacs-directory))
;; packages
(setq package-user-dir (expand-file-name "packages/" user-emacs-directory))

;;------------------------------------------------------------------------------
;; apply custom variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;------------------------------------------------------------------------------
;; package setup
(package-initialize)

(unless package-archive-contents
  (message "Refreshing package archives...")
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

(require 'use-package)

(setq personal-keybindings nil)

(defun my/vc-install (repo)
  (let ((url (format "https://www.github.com/%s" repo))
        (pac-name (intern (file-name-base repo))))
    (unless (package-installed-p pac-name)
      (package-vc-install url))))

(defun my/graphic-mode-p ()
  (or (display-graphic-p) (string-equal (daemonp) "gui")))

;;------------------------------------------------------------------------------
;; Setup GC
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

;;------------------------------------------------------------------------------
;; Startup profiling
(use-package esup
  :ensure t
  :defer t)

;;------------------------------------------------------------------------------
;; Common settings
(load "common.el")

;;------------------------------------------------------------------------------
;; Graphic or terminal mode?
(if (my/graphic-mode-p)
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
;; General programming
(load "prog.el")

;;------------------------------------------------------------------------------
;; C++
(load "cpp-modes.el")

;;------------------------------------------------------------------------------
;; Python
(load "python-modes.el")

;;------------------------------------------------------------------------------
;; Git
(load "git-config.el")

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
(load "utilities.el")

(bind-key "C-x C-S-e" 'eval-and-replace)
(bind-key "C-c C-w" 'toggle-window-split)
(bind-key "C-c C-d" 'insert-current-date)
(bind-key "C-c C-t" 'insert-current-time)
(bind-key "C-c C-u" 'insert-uuid)
(bind-key "C-c C-f" 'my-search-forward-1)
(bind-key "C-c C-b" 'my-search-backward-1)

;; don't put deletions in kill-ring while in minibuffer
(define-key minibuffer-local-map (kbd "M-DEL") 'my-delete-backward-word)

;;------------------------------------------------------------------------------
;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))

(put 'narrow-to-region 'disabled nil)

;;------------------------------------------------------------------------------
;; Hacks & late-bound overrides
(load "hacks.el")

;------------------------------------------------------------------------------
;; apply local site-specific changes
(load "local.el")

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;; debugging
(setq debug-on-error nil)
;; plenty of memory, GC threshold is 100MB
(setq gc-cons-threshold 100000000)

(setq user-full-name "Ben Deane")

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

;;------------------------------------------------------------------------------
;; apply custom variables
(setq custom-file (concat dotfile-dir ".emacs.d/custom.el"))
(load custom-file)

;;------------------------------------------------------------------------------
;; package setup
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("elpa" . "http://tromey.com/elpa/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("stable-melpa" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

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
(use-package esup
  :ensure t
  :defer t)

;;------------------------------------------------------------------------------
;; Common packages
(use-package diminish
  :ensure t)

;;------------------------------------------------------------------------------
;; OS specifics

;; font: inconsolata
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 100
                    :weight 'normal
                    :width 'normal)
(set-fontset-font "fontset-default"
                  '(#x0100 . #xffff)
                  (font-spec :family "DejaVu Sans Mono"
                             :height 100
                             :weight 'normal
                             :width 'normal))

;;------------------------------------------------------------------------------
;; UTF8 defaults
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;------------------------------------------------------------------------------
;; Clean up display
(tool-bar-mode -1)
(menu-bar-mode -1)
(display-battery-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
      initial-scratch-message ""
      visible-bell 1)

;; Display defaults
(setq column-wrap-soft 80
      column-wrap-hard 100)
(setq sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; whitespace
(setq require-final-newline t)
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode)
  :diminish
  ws-butler-mode)

;;------------------------------------------------------------------------------
;; Copy/paste stuff

(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;;------------------------------------------------------------------------------
;; Set up the frame
(use-package dash
  :ensure t)

(when (display-graphic-p)

  ;; Sizing/docking
  (setq frame-resize-pixelwise t)

  (defun monitor-width (monitor)
    (nth 3 (assq 'geometry monitor)))

  (defun frame-max-height (&optional frame)
    (interactive)
    (set-frame-parameter frame 'fullscreen 'fullheight))

  (defun dock-frame-left (&optional frame monitor)
    (interactive)
    (setq frame (or frame (selected-frame)))
    (setq monitor (or monitor (frame-monitor-attributes frame)))
    (let* ((monitor-list (-take-while
                          (lambda (x) (not (equal monitor x)))
                          (display-monitor-attributes-list)))
           (widths (mapcar #'monitor-width monitor-list))
           (x (apply '+ widths)))
      (set-frame-parameter frame 'left x)))

  (defun dock-frame-right (&optional frame monitor)
    (interactive)
    (setq frame (or frame (selected-frame)))
    (setq monitor (or monitor (frame-monitor-attributes frame)))
    (let* ((monitor-list (-take-while
                          (lambda (x) (not (equal monitor x)))
                          (display-monitor-attributes-list)))
           (widths (mapcar #'monitor-width monitor-list))
           (x (+ (apply '+ widths) (monitor-width monitor))))
      (set-frame-parameter frame 'left (- x (frame-pixel-width frame)))))

  (defun size-frame-default ()
    (set-frame-parameter nil 'width column-wrap-hard)
    (frame-max-height))

  (bind-key "C-S-<f11>" 'frame-max-height)
  (bind-key "C-<f11>" 'dock-frame-left)
  (bind-key "C-<f12>" 'dock-frame-right)

  ;; Frame opacity
  (defun sanityinc/adjust-opacity (frame incr)
    (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
           (newalpha (+ incr oldalpha)))
      (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
        (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
  (bind-key "M-C-8" (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
  (bind-key "M-C-9" (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
  (bind-key "M-C-0" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100))))))

;;------------------------------------------------------------------------------
;; Autosaves/backups
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/autosaves" t)
(setq backup-directory-alist
      `((".*" . , "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/autosaves/" t)))

;; Delete backups older than one month
(defun delete-backups ()
  (interactive)
  (message "Deleting old backup files...")
  (let ((month (* 60 60 24 31))
        (current (float-time (current-time))))
    (dolist (file (directory-files temporary-file-directory t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (nth 5 (file-attributes file))))
                    month))
        (message file)
        (delete-file file)))))
(delete-backups)

;;------------------------------------------------------------------------------
;; Minor modes

;; Show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Syntactic close
(use-package syntactic-close
  :ensure t
  :bind ("M-]" . syntactic-close))

;; Show column numbers
(column-number-mode)

;; Auto-revert buffers
(global-auto-revert-mode)
(diminish 'auto-revert-mode)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Rainbow hex colors & color names
;; use custom regexp for #colors to avoid clash with e.g. #define
(use-package rainbow-mode
  :ensure t
  :config
  (setq rainbow-hexadecimal-colors-font-lock-keywords
	'(("[^&]\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)\\b"
	   (1 (rainbow-colorize-itself 1)))
	  ("^\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)\\b"
	   (0 (rainbow-colorize-itself)))
	  ("[Rr][Gg][Bb]:[0-9a-fA-F]\\{1,4\\}/[0-9a-fA-F]\\{1,4\\}/[0-9a-fA-F]\\{1,4\\}"
	   (0 (rainbow-colorize-itself)))
	  ("[Rr][Gg][Bb][Ii]:[0-9.]+/[0-9.]+/[0-9.]+"
	   (0 (rainbow-colorize-itself)))
	  ("\\(?:[Cc][Ii][Ee]\\(?:[Xx][Yy][Zz]\\|[Uu][Vv][Yy]\\|[Xx][Yy][Yy]\\|[Ll][Aa][Bb]\\|[Ll][Uu][Vv]\\)\\|[Tt][Ee][Kk][Hh][Vv][Cc]\\):[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?/[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?/[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?"
	   (0 (rainbow-colorize-itself)))))
  (add-hook 'prog-mode-hook #'rainbow-mode)
  (add-hook 'css-mode-hook #'rainbow-mode)
  :diminish rainbow-mode)

;; Highlight numbers, quoted things, escape sequences
(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  :diminish highlight-numbers-mode)

(use-package highlight-quoted
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-quoted-mode)
  :diminish highlight-quoted-mode)

(use-package highlight-escape-sequences
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'hes-mode)
  :diminish hes-mode)

;; elisp hints
(use-package eldoc
  :diminish eldoc-mode
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)))

;; comment-dwim-2
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; which-key to give help on keys
(use-package which-key
  :ensure t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode)
  :diminish which-key-mode)

;; use ibuffer instead of list-buffers
(defalias 'list-buffers 'ibuffer)

;; expand-region
(use-package expand-region
  :ensure t
  :bind (("C-'" . er/expand-region)
         ("C-@" . er/contract-region)))
(delete-selection-mode 1)

;; region-state: show lines/characters selected
(use-package region-state
  :ensure t
  :config
  (region-state-mode)
  :diminish region-state-mode)

;; prettify symbols in modes that support it
;; unprettify the symbol at point
(global-prettify-symbols-mode)
(setq prettify-symbols-unprettify-at-point 'right-edge)

;;------------------------------------------------------------------------------
;; Colors

(when (display-graphic-p)
  (set-face-foreground 'font-lock-comment-face "gray")
  (set-face-foreground 'font-lock-string-face "firebrick")
  (set-face-foreground 'font-lock-warning-face "black")
  (set-face-background 'font-lock-warning-face "orange")
  (set-face-background 'region "moccasin")
  (set-face-foreground 'region "navy"))

;; Highlight FIXME/TODO
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(FIXME\\|TODO\\).*?:" 0 font-lock-warning-face prepend)))

;; Special types of comments
(defface font-lock-comment-strike
  '((t (:strike-through t)))
  "For strike-through comments")

(defface font-lock-comment-important
  '((t (:foreground "#00ff00")))
  "For important")

(defface font-lock-comment-todo
  '((t (:foreground "#ff0000")))
  "For todo comments")

(defun add-custom-keyw()
  "adds a few special keywords"
  (font-lock-add-keywords
   nil
   '(("\\s<+x[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-strike prepend)
     ("\\s<+t[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-todo prepend)
     ("\\s<+i[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-important prepend))))

(add-hook 'prog-mode-hook #'add-custom-keyw)

(defun add-custom-keyw-cpp()
  "adds a few special keywords"
  (font-lock-add-keywords
   nil
   '(("//+x[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-strike prepend)
     ("//+t[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-todo prepend)
     ("//+i[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-important prepend))))

(add-hook 'c++-mode-hook #'add-custom-keyw-cpp)

;;------------------------------------------------------------------------------
;; Global key bindings
(bind-key "C-z" 'undo)
(bind-key "C-o" 'goto-line)
(bind-key "M-r" 'replace-string)
(bind-key "M-k" 'compile)
(bind-key "M-SPC" 'cycle-spacing)

;; better searching
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

;; Action of home key
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(bind-key "<home>" 'beginning-of-line-or-indentation)

;; Turn off insert
(defun do-nothing () (interactive))
(bind-key "<insert>" 'do-nothing)
(bind-key "<insertchar>" 'do-nothing)

;; Kill-ring menu
(defun popup-kill-ring-menu ()
  "Show the kill ring in a popup menu."
  (interactive)
  (popup-menu 'yank-menu))
(bind-key "C-c y" 'popup-kill-ring-menu)

;; Cycle buffers/windows with F5-F8
(bind-key "<f5>" 'next-multiframe-window)
(bind-key "<f6>" 'previous-multiframe-window)
(bind-key "<f7>" 'previous-buffer)
(bind-key "<f8>" 'next-buffer)

;; Moving windows
(bind-key "C-c <left>"  'windmove-left)
(bind-key "C-c <right>" 'windmove-right)
(bind-key "C-c <up>"    'windmove-up)
(bind-key "C-c <down>"  'windmove-down)

;;------------------------------------------------------------------------------
;; Highlight symbols
(bind-key "<f3>" 'highlight-symbol-at-point)
(bind-key "S-<f3>" 'hi-lock-mode)

;;------------------------------------------------------------------------------
;; Smooth scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;;------------------------------------------------------------------------------
;; Prevent prompt on opening large TAGS file
(setq large-file-warning-threshold 100000000)

;;------------------------------------------------------------------------------
;; Mark the fill column
(setq-default fill-column column-wrap-soft)

;; fci-mode doesn't play well with popups
(defun sanityinc/prog-mode-fci-settings ()
  (turn-on-fci-mode)
  (when show-trailing-whitespace
    (set (make-local-variable 'whitespace-style) '(face trailing))
    (whitespace-mode 1)))

(defun sanityinc/fci-enabled-p ()
  (and (boundp 'fci-mode) fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)

(defun suspend-fci-mode ()
  "Suspend fci-mode"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-enabled)
      (turn-off-fci-mode))))

(defun unsuspend-fci-mode ()
  "Restore fci-mode"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

(defadvice popup-create (before suppress-fci-mode activate)
  (suspend-fci-mode))

(defadvice popup-delete (after restore-fci-mode activate)
  (unsuspend-fci-mode))

;; Regenerate fci-mode line images after switching themes
(defadvice enable-theme (after recompute-fci-face activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (sanityinc/fci-enabled-p)
        (turn-on-fci-mode)))))

(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-color "#ff8888")
  (setq fci-always-use-textual-rule t))

(add-hook 'prog-mode-hook #'sanityinc/prog-mode-fci-settings)

;;------------------------------------------------------------------------------
;; Bookmarks
(use-package bm
  :ensure t
  :bind (("C-<f2>" . bm-toggle)
         ("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)))

;;------------------------------------------------------------------------------
;; IDO & smex
(use-package ido
  :ensure t
  :preface
  (defvar ido-cur-item               nil)
  (defvar ido-default-item           nil)
  (defvar inherit-input-method       nil)
  (defvar ido-cur-list               nil)
  (defvar ido-context-switch-command nil)
  (defvar ido-cr+-enable-next-call   nil)
  (defvar ido-cr+-replace-completely nil)
  (defvar ido-cr+-debug-mode         nil)
  (defvar ido-require-match          nil))

(use-package flx-ido
  :ensure t
  :config
  (ido-mode 1)
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point nil)
  (setq ido-create-new-buffer 'always)
  (flx-ido-mode 1)
  (setq ido-use-faces t))

(use-package ido-completing-read+
  :ensure t
  :preface
  (defvar ido-ubiquitous-debug-mode nil)
  :config
  (ido-ubiquitous-mode))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-show-count t)
  (ido-vertical-mode 1)
  (set-face-attribute 'ido-vertical-first-match-face nil
                      :background "#e5b7c0")
  (set-face-attribute 'ido-vertical-only-match-face nil
                      :background "#e52b50"
                      :foreground "white")
  (set-face-attribute 'ido-vertical-match-face nil
                      :foreground "#b00000")
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :bind
  (("M-x" . smex))
  :demand)

;;------------------------------------------------------------------------------
;; Undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  :diminish undo-tree-mode)

;;------------------------------------------------------------------------------
;; Google-this
(use-package google-this
  :ensure t
  :bind
  (("C-c <f1>" . google-this-cpp-reference)))

;;------------------------------------------------------------------------------
;; markdown mode
(use-package markdown-mode
  :ensure t)

;;------------------------------------------------------------------------------
;; Diffing things
(use-package ztree-diff
  :ensure ztree
  :bind
  (("C-c C-z" . ztree-diff)
   ("C-c z" . ztree-dir)))

;;------------------------------------------------------------------------------
;; Avy
(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-word-or-subword-1)
         ("C-;" . avy-goto-word-or-subword-1)
         ("C-." . avy-pop-mark)))

(use-package ace-window
  :ensure t
  :bind ("C-c w" . ace-window))

;;------------------------------------------------------------------------------
;; smart-scan: use M-n and M-p to jump to next/prev thing at point
(use-package smartscan
  :ensure t
  :config (global-smartscan-mode t))

;;------------------------------------------------------------------------------
;; Neotree - an in-buffer speedbar replacement

;; When opening a file, or a directory with dired, hide the neotree window. Just
;; using neo-enter-hook doesn't quite do it, because neotree routes all
;; functionality (eg refresh, toggle hidden, etc) through neo-buffer--execute.

(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t
        neo-hidden-regexp-list '("\\.pyc$" "~$" "^#.*#$" "^\\.#\\..*$" "\\.elc$")
        my/neotree-opening-file nil
        my/neotree-entering-dired nil)
  (defun neo-hide-on-enter (type path arg)
    (if (or (and (eq my/neotree-opening-file t)
                 (equal type 'file))
            (and (eq my/neotree-entering-dired t)
                 (equal type 'directory)))
        (neotree-hide))
    (setq my/neotree-opening-file nil
          my/neotree-entering-dired nil))
  (defun my/before-neobuffer-execute (arg0 &optional file-fn dir-fn &rest args)
    (when (eq dir-fn 'neo-open-dired)
      (setq my/neotree-entering-dired t))
    (when (or (eq file-fn 'neo-open-file)
              (eq file-fn 'neo-open-file-vertical-split)
              (eq file-fn 'neo-open-file-horizontal-split))
      (setq my/neotree-opening-file t)))
  (advice-add 'neo-buffer--execute :before #'my/before-neobuffer-execute)
  (add-hook 'neo-enter-hook #'neo-hide-on-enter)
  :bind (("<f12>" . neotree-toggle)))

;;------------------------------------------------------------------------------
;; C++ mode
(setq-default c-basic-offset 2)
(add-hook 'c++-mode-hook #'flyspell-prog-mode)
(setq flyspell-issue-message-flag nil)

(use-package modern-cpp-font-lock
  :ensure t
  :config  
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

;; clang-format
(use-package clang-format
  :ensure t
  :bind
  (("C-c f" . clang-format)))

;; Auto insertion of headers
(autoload 'cpp-auto-include/namespace-qualify-file "cpp-auto-include"
  "Explicitly qualify uses of the standard library with their namespace(s)." t)
(autoload 'cpp-auto-include/ensure-includes-for-file "cpp-auto-include"
  "Auto-insert #include line(s) required for the current buffer." t)
(autoload 'cpp-auto-include/ensure-includes-for-current-line "cpp-auto-include"
  "Auto-insert #include line(s) required for the current line." t)
(eval-after-load 'cc-mode
  '(bind-keys :map c++-mode-map
              ("C-c q" . cpp-auto-include/namespace-qualify-file)
              ("C-c i" . cpp-auto-include/ensure-includes-for-file)
              ("C-c o" . cpp-auto-include/ensure-includes-for-current-line)))

;; indentation rules
(defun indentation-c-mode-hook ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'member-init-cont '-)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open 0))
(add-hook 'c-mode-common-hook 'indentation-c-mode-hook)

;; Align boost SML tables
(defun align-boost-sml (start end)
 (interactive "r")
 (indent-region start end)
 (align-regexp start end "[[:space:]]*\\([[:space:]]\\)\\*" 1 0)
 (align-regexp start end "\\([[:space:]]*\\)\\+")
 (align-regexp start end "\\([[:space:]]*\\)\\[")
 (align-regexp start end "\\([[:space:]]*\\)/")
 (align-regexp start end "\\([[:space:]]*\\)="))

(eval-after-load 'cc-mode
  '(bind-keys :map c++-mode-map
              ("C-<tab>" . align)
              ("C-<insert>" . align-boost-sml)))

;;------------------------------------------------------------------------------
;; CMake
(use-package cmake-mode
  :ensure t)

;;------------------------------------------------------------------------------
;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t
        projectile-switch-project-action 'neotree-projectile-action)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :bind
  (("C-x f" . projectile-find-file)
   ("C-x g" . projectile-grep)
   ("C-c #" . projectile-find-file-dwim)
   ("C-x C-h" . projectile-find-other-file)))

;;------------------------------------------------------------------------------
;; Git interactions
;; modes
(use-package gitconfig-mode :ensure t)
(use-package gitignore-mode :ensure t)
(use-package git-commit :ensure t)

;; magit
(use-package magit
  :ensure t
  :bind
  (("C-c g" . magit-status)
   ("C-c C-b" . magit-blame))
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"))

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
  (add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode))

;; on-the-fly diff highlighting
(use-package diff-hl
  :ensure t
  :config
  (setq diff-hl-side 'right)
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;;------------------------------------------------------------------------------
;; Org-mode
(use-package org
  :ensure org-plus-contrib
  :commands (org-mode)
  :mode ("\\.org$" . org-mode)
  :pin org
  :config
  (setq org-log-done t)
  (setq org-support-shift-select t)
  (setq org-startup-indented t)
  (setq org-src-fontify-natively t)
  (setq org-completion-use-ido t)
  (setq org-export-allow-bind-keywords t)
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-minted-options
        '(("frame" "none")
          ("fontsize" "\\scriptsize")
          ("linenos" "")
          ))
  (setq org-beamer-outline-frame-title "Contents")
  (setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                            (sequence "⚑ WAITING(w)" "|")
                            (sequence "|" "✘ CANCELLED(c)")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (js . t)
     (haskell . t)
     (emacs-lisp . t))))

;; better header bullets
(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
        '("◉" "✸" "✿" "◎" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; better inline list bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1)
                                                        (match-end 1) "•"))))))

(use-package ox-reveal
  :ensure t
  :defer)

(use-package htmlize
  :ensure t
  :defer)

;; suspend fci mode and flyspell-mode when exporting html
(defvar modi/htmlize-initial-fci-state nil
  "Variable to store the state of `fci-mode' when `htmlize-buffer' is called.")
(defvar modi/htmlize-initial-flyspell-state nil
  "Variable to store the state of `flyspell-mode' when `htmlize-buffer' is called.")

(defun modi/htmlize-before-hook-fn ()
  (when (fboundp 'fci-mode)
    (setq modi/htmlize-initial-fci-state fci-mode)
    (when fci-mode
      (fci-mode -1)))
  (when (fboundp 'flyspell-mode)
    (setq modi/htmlize-initial-flyspell-state flyspell-mode)
    (when flyspell-mode
      (flyspell-mode -1))))
(add-hook 'htmlize-before-hook #'modi/htmlize-before-hook-fn)

(defun modi/htmlize-after-hook-fn ()
  (when (fboundp 'fci-mode)
    (when modi/htmlize-initial-fci-state
      (fci-mode 1)))
  (when (fboundp 'flyspell-mode)
    (when modi/htmlize-initial-flyspell-state
      (flyspell-mode 1))))
(add-hook 'htmlize-after-hook #'modi/htmlize-after-hook-fn)

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c i" 'org-iswitchb)

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-latex-packages-alist '("" "minted" nil))
            (bind-key "M-Q" 'toggle-truncate-lines org-mode-map)
            (require 'ox-latex)
            (require 'ox-beamer)
            (require 'ox-reveal)
            (require 'htmlize)
            (add-to-list 'org-beamer-environments-extra
			 '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))))

(setq initial-major-mode 'org-mode)

;;------------------------------------------------------------------------------
;; Other lesser-used modes
;; vlfi
(use-package vlf
  :ensure t
  :defer 5)

;; highlight line in dired
(add-hook 'dired-mode-hook 'hl-line-mode)

;; Shells
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "/bin/ls")

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

;; custom stuff is per-installation/work private
(if (file-directory-p ".emacs.d/custom/")
    (mapc 'load (files-in-below-directory (concat dotfile-dir ".emacs.d/custom/"))))

;;------------------------------------------------------------------------------
;; Size the frame
(setq default-frame-height (frame-height))
(setq default-frame-alist
      (append
       `((width . ,column-wrap-hard)
	 (height . ,default-frame-height))
       default-frame-alist))

(size-frame-default)

;;------------------------------------------------------------------------------
;; Start server
(require 'server)
(server-start)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

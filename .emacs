;;------------------------------------------------------------------------------
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
;; OS specifics

(defun my/cygwin-setup ()
  ;; we'll need cygwin-mount
  (use-package cygwin-mount
    :ensure t)
  ;; font: consolas
  (set-face-attribute 'default nil
                      :family "Consolas"
                      :height 100
                      :weight 'normal
                      :width 'normal)
  (set-fontset-font "fontset-default"
                    '(#x0100 . #xffff)
                    (font-spec :family "Arial Unicode MS"
                               :height 100
                               :weight 'normal
                               :width 'normal))
  ;; clipboard
  (set-clipboard-coding-system 'utf-16le-dos)
  ;; frame title
  (setq frame-title-format "%b [cygwin emacs]")
  ;; Windows paths
  (require 'windows-path)
  (windows-path-activate))

(defun my/linux-setup ()
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
                               :width 'normal)))

(cond ((eq system-type 'cygwin)
       (my/cygwin-setup))
      ((eq system-type 'gnu/linux)
       (my/linux-setup)))

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

(setq load-prefer-newer t)

;;------------------------------------------------------------------------------
;; Set up the frame
(use-package dash
  :ensure t)

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

;;------------------------------------------------------------------------------
;; Frame opacity
(defun sanityinc/adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))
(bind-key "M-C-8" (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(bind-key "M-C-9" (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(bind-key "M-C-0" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

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
  (let ((week (* 60 60 24 31))
        (current (float-time (current-time))))
    (dolist (file (directory-files temporary-file-directory t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (nth 5 (file-attributes file))))
                    week))
        (message file)
        (delete-file file)))))

;;------------------------------------------------------------------------------
;; Minor modes

;; Show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; General close
(autoload 'general-close "general-close" "Insert closing delimiter." t)
(bind-key "C-c ]" 'general-close)

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
(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
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

;; Highlight numbers in groups of 3
(use-package num3-mode
  :ensure t
  :config
  (global-num3-mode)
  :diminish num3-mode)

;; elisp hints
(use-package eldoc
  :diminish eldoc-mode
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
    (add-hook 'python-mode-hook 'eldoc-mode)))

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
(set-face-foreground 'font-lock-comment-face "gray")
(set-face-foreground 'font-lock-string-face "firebrick")
(set-face-foreground 'font-lock-warning-face "black")
(set-face-background 'font-lock-warning-face "orange")
(set-face-background 'region "moccasin")
(set-face-foreground 'region "navy")

;; Highlight FIXME/TODO
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(FIXME\\|TODO\\).*?:" 0 font-lock-warning-face prepend)))
(font-lock-add-keywords 'python-mode
                        '(("\\<\\(FIXME\\|TODO\\).*?:" 0 font-lock-warning-face prepend)))

;; SQL comments
(font-lock-add-keywords 'sql-mode
                        '(("\\s-*//.*$" 0 font-lock-comment-face prepend)))

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
(use-package column-marker
  :ensure t)
(autoload 'column-marker-1 "column-marker" "Highlight a column." t)
(autoload 'column-marker-2 "column-marker" "Highlight a column." t)
(add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-1 column-wrap-soft)))
(add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-2 column-wrap-hard)))
(setq-default fill-column column-wrap-soft)

;; fci-mode doesn't play well with company
;; (use-package fill-column-indicator
;;   :ensure t)
;; (add-hook 'c-mode-common-hook 'fci-mode)

;;------------------------------------------------------------------------------
;; Auto modes
(setq auto-mode-alist (append '(("\\.mm$" . objc-mode)
                                ("\\.h$" . c++-mode)
                                ("\\.lua$" . lua-mode)
                                ("\\.js$" . js2-mode)
                                ("\\.qml$" . js2-mode)
                                ("\\.json$" . json-mode)
                                ("\\.ui$" . nxml-mode)
                                ("SConstruct" . python-mode)
                                ("SConscript" . python-mode)
                                ("\\.tmpl$" . jinja2-mode)                                
                                ("\\.ml[iyl]?$" . caml-mode)
                                ("\\.pb$" . protobuf-mode)
                                ("\\.proto$" . protobuf-mode)
                                ("\\.presql$" . sql-mode)
                                ("\\.yml$" . yaml-mode))
                              auto-mode-alist))

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

(use-package ido-ubiquitous
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
;; Flycheck
(use-package flycheck
  :ensure t
  :defer 2
  :config
  (cond ((eq system-type 'gnu/linux)
         (flycheck-define-checker javascript-jslint-reporter
           "A JavaScript syntax and style checker based on JSLint Reporter.

See URL `https://github.com/FND/jslint-reporter'."
           :command ("jslint-reporter" source)
           :error-patterns
           ((error line-start (1+ nonl) ":" line ":" column ":" (message) line-end))
           :modes (js-mode js2-mode js3-mode))))

  (use-package flycheck-tip
    :ensure t
    :config
    (flycheck-tip-use-timer 'verbose)
    :bind ("C-c n" . flycheck-tip-cycle)
    :demand))

;;------------------------------------------------------------------------------
;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;;------------------------------------------------------------------------------
;; YASnippet
(use-package yasnippet
  :ensure t
  :pin melpa
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode)

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
;; Autocomplete: company
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (delete 'company-semantic company-backends)
  ;; Use dabbrev-code completion for windows
  (cond ((eq system-type 'cygwin)
         (add-to-list 'company-backends 'company-dabbrev-code)))
  (setq company-tooltip-align-annotations t
        company-show-numbers t))

;; Use help with company
(use-package pos-tip
  :ensure t
  :pin melpa)

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))

(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

;;------------------------------------------------------------------------------
;; Make sure tab works with indenting, completion, yasnippet
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (indent-for-tab-command)
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(bind-key "<tab>" 'tab-indent-or-complete)

;;------------------------------------------------------------------------------
;; Basic offset = 2
(setq-default c-basic-offset 2)
(setq lua-indent-level 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq python-indent-offset 2)

;;------------------------------------------------------------------------------
;; C++ mode
(defun indentation-c-mode-hook ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'member-init-cont '-)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open 0))
(add-hook 'c-mode-common-hook 'indentation-c-mode-hook)

;; Indent rules
(defun normal-indent-rules ()
  (interactive)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq c-basic-indent 2)
  (c-set-offset 'arglist-intro '++))
(defun tabbed-indent-rules ()
  (interactive)
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq c-basic-indent 4)
  (c-set-offset 'arglist-intro '+))
(defun qml-indent-rules ()
  (interactive)
  (setq indent-tabs-mode t)
  (setq tab-width 2))

(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
  Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (normal-indent-rules))
    (if (> tab-count space-count) (tabbed-indent-rules))))

(add-hook 'c-mode-common-hook 'infer-indentation-style)

;; Header completion
(defun complete-c-headers ()
  (use-package company-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-c-headers)))

(add-hook 'c-mode-common-hook 'complete-c-headers)

;; cmake-ide setup
(defun rtags-enable-my-keybindings (&optional map prefix)
  (interactive)
  (unless map
    (setq map c-mode-base-map))
  (unless prefix
    (setq prefix "\C-cr"))
  (ignore-errors
    (define-key map (concat prefix "t") (function rtags-symbol-type))))

(defun use-cmake-ide ()
  (use-package cmake-ide
    :ensure t
    :config
    (add-to-list 'load-path (concat dotfile-dir "../rtags/src/"))
    (add-to-list 'exec-path (concat dotfile-dir "../rtags/bin/"))
    (require 'rtags)
    (rtags-enable-standard-keybindings c-mode-base-map)
    (rtags-enable-my-keybindings c-mode-base-map)
    (cmake-ide-setup)
    (when (cmake-ide--locate-cmakelists)
      (setq cmake-ide-dir (concat (cmake-ide--locate-cmakelists) "build/")))))

(when (eq system-type 'gnu/linux)
  (add-hook 'c++-mode-hook 'use-cmake-ide)
  (add-hook 'c++-mode-hook 'flycheck-mode))

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

(when (eq system-type 'gnu/linux)
  (eval-after-load 'cc-mode
    '(bind-keys :map c++-mode-map
                ("M-."   . rtags-find-symbol-at-point)
                ("C-M-." . rtags-location-stack-back)
                ("M-,"   . rtags-find-references-at-point)
                ("M-]"   . rtags-next-match)
                ("M-["   . rtags-previous-match)
                ("M-k"   . cmake-ide-compile))))

;;------------------------------------------------------------------------------
;; CMake

(use-package cmake-mode
  :ensure t)

;;------------------------------------------------------------------------------
;; Compilation

;; Compilation
(bind-key "M-<up>" 'previous-error)
(bind-key "M-<down>" 'next-error)
(setq compilation-scroll-output t)

;; Remove compilation window on success
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "1 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No compilation errors!")))))

;; SCons builds into a 'build' subdir, but we want to find the errors
;; in the regular source dir.  So we remove build/XXX/YYY/{debug,release}/ from the
;; filenames.
(defun process-error-filename (filename)
  (let ((case-fold-search t))
    (setq f (replace-regexp-in-string
             "\\(build\\|export\\)\\/.*\\(debug\\|release\\)\\/" "src/" filename))
    (cond ((file-exists-p f)
           f)
          (t filename))))

(setq compilation-parse-errors-filename-function 'process-error-filename)

;;------------------------------------------------------------------------------
;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t
        projectile-switch-project-action 'neotree-projectile-action)
  :bind
  (("C-x f" . projectile-find-file)
   ("C-x g" . projectile-grep)
   ("C-c #" . projectile-find-file-dwim)
   ("C-x C-h" . projectile-find-other-file)))

;;------------------------------------------------------------------------------
;; Haskell mode
(use-package ghc
  :pin stable-melpa
  :ensure t
  :config
  (setq ghc-interactive-command "ghc-modi"
        ghc-debug t)
  (add-hook 'haskell-mode-hook 'ghc-init))

(use-package company-ghc
  :ensure t
  :config
  (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
  (setq company-ghc-show-info 'oneline))

(eval-after-load "haskell-cabal"
  '(bind-key "M-k" 'haskell-compile haskell-cabal-mode-map))

(defun find-haskell-definition-at-point ()
  (interactive)
  (inferior-haskell-find-definition (haskell-ident-at-point)))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs$"
  :config
  (setq font-lock-maximum-decoration '((haskell-mode . 2) (t . 0))
        haskell-tags-on-save t))

(eval-after-load "haskell-mode"
  '(bind-keys :map haskell-mode-map
              ("M-k" . haskell-compile)
              ("M-." . find-haskell-definition-at-point)
              ("C-c M-." . inferior-haskell-find-definition)
              ("C-c v c" . haskell-cabal-visit-file)
              ("C-c C-z" . inferior-haskell-load-file)))

(use-package flycheck-haskell
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'flycheck-haskell-setup))

(use-package hindent
  :ensure t
  :config
  (setq hindent-style "chris-done")
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'hindent-mode))

;;------------------------------------------------------------------------------
;; Python mode
(add-hook 'python-mode-hook (lambda ()
                              (flycheck-select-checker 'python-flake8)
                              (flycheck-mode)
                              (elpy-mode)))

(use-package jinja2-mode
  :ensure t
  :mode "\\.tmpl$")

(use-package elpy
  :ensure t
  :defer t)

(eval-after-load "elpy"
  '(progn
     (unbind-key "M-<down>" elpy-mode-map)
     (unbind-key "M-<up>" elpy-mode-map)
     (bind-key "M-k" 'elpy-check elpy-mode-map)))

;;------------------------------------------------------------------------------
;; Lua mode
(use-package lua-mode
  :ensure t
  :mode "\\.lua$")

;;------------------------------------------------------------------------------
;; Yaml mode
(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

;;------------------------------------------------------------------------------
;; Javascript
(use-package js2-mode
  :ensure t
  :mode ("\\.js$" "\\.qml$"))

(use-package json-mode
  :ensure t
  :mode "\\.json$")

(if (eq system-type 'gnu/linux)
  (add-hook 'js2-mode-hook (lambda ()
                            (flycheck-select-checker 'javascript-jslint-reporter)
                            (flycheck-mode))))

;;------------------------------------------------------------------------------
;; For Qt ui files
(use-package nxml-mode
  :ensure t
  :mode "\\.ui$")

;;------------------------------------------------------------------------------
;; Protobufs
;; protobuf mode requires cl
(use-package protobuf-mode
  :ensure t
  :init
  (require 'cl)
  :mode ("\\.proto$" "\\.pb$"))

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
                            (sequence "|" "✘ CANCELLED(c)"))))

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
        '("◉" "✸" "✿" "◎" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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

;; company for shell mode
(use-package company-shell
  :ensure t
  :config
  (add-to-list 'company-backends 'company-shell))

;; lilypond
(when (eq system-type 'gnu/linux)
  (add-to-list 'load-path "/home/bdeane/dev/lyqi")
  (autoload 'lyqi-mode "lyqi" "Lilypond mode." t)
  (add-to-list 'auto-mode-alist '("\\.ly$" . lyqi-mode))
  (add-to-list 'auto-mode-alist '("\\.ily$" . lyqi-mode))
  (setq lyqi:prefered-languages '(italiano english)
        lyqi:prefered-octave-mode 'absolute
        lyqi:keyboard-mapping 'qwerty
        lyqi:midi-backend 'alsa
        lyqi:pdf-command "evince"
        lyqi:midi-command "timidity")
  (eval-after-load "lyqi"
    '(bind-keys :map lyqi:normal-mode-map
                ("M-k" . lyqi:compile-ly))))

;;------------------------------------------------------------------------------
;; Add yasnippet support for all company backends
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas)
          (and (listp backend)
               (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

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
;; Useful functions
(defun nuke-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapc (lambda (x) (kill-buffer x))
    (buffer-list))
  (delete-other-windows))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(bind-key "C-x C-S-e" 'eval-and-replace)

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

(bind-key "C-c d" 'insert-current-date)
(bind-key "C-c t" 'insert-current-time)

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

(bind-key "C-c u" 'insert-uuid)

(defun insert-xcode-header-template ()
  (interactive)
  (let ((build-file-uuid (random-xcode-uuid))
        (file-ref-uuid (random-xcode-uuid)))
    (insert "\t\t")
    (insert build-file-uuid)
    (insert " /* something.h in Headers */ = {isa = PBXBuildFile; fileRef = ")
    (insert file-ref-uuid)
    (insert " /* something.h */; };\n\n")
    (insert "\t\t")
    (insert file-ref-uuid)
    (insert " /* something.h */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.c.h; name = something.h; path = something.h; sourceTree = \"<group>\"; };\n\n")
    (insert "\t\t\t\t")
    (insert file-ref-uuid)
    (insert " /* something.h */,\n")))

(defun insert-xcode-source-template ()
  (interactive)
  (let ((build-file-uuid (random-xcode-uuid))
        (file-ref-uuid (random-xcode-uuid)))
    (insert "\t\t")
    (insert build-file-uuid)
    (insert " /* something.cpp in Sources */ = {isa = PBXBuildFile; fileRef = ")
    (insert file-ref-uuid)
    (insert " /* something.cpp */; };\n\n")
    (insert "\t\t")
    (insert file-ref-uuid)
    (insert " /* something.cpp */ = {isa = PBXFileReference; fileEncoding = 4; lastKnownFileType = sourcecode.cpp.cpp; name = something.cpp; path = something.cpp; sourceTree = \"<group>\"; };\n\n")
    (insert "\t\t\t\t")
    (insert file-ref-uuid)
    (insert " /* something.cpp */,\n")))

;;------------------------------------------------------------------------------
;; load any further custom stuff
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

;; custom stuff is per-installation/work private
(mapc 'load (files-in-below-directory (concat dotfile-dir ".emacs.d/custom/")))

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

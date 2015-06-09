;;------------------------------------------------------------------------------
;; debugging
(setq debug-on-error t)

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
                         ("elpa" . "http://tromey.com/elpa/")
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
      use-package-verbose t
      use-package-always-ensure t)

;;------------------------------------------------------------------------------
;; Startup profiling
(use-package esup
  :ensure t
  :defer 5)

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
  (windows-path-activate)
  ;; Misc
  (setq haskell-process-path-ghci "ghcii.sh"))

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

;;------------------------------------------------------------------------------
;; Clean up display
(tool-bar-mode -1)
(menu-bar-mode -1)
(display-battery-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq visible-bell 1)

;; Display defaults
(setq column-wrap-soft 80)
(setq column-wrap-hard 100)
(setq sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;;------------------------------------------------------------------------------
;; Set up the frame
(use-package dash
  :ensure t)

(setq frame-resize-pixelwise t)
(defun monitor-width (monitor)
  (nth 3 (assq 'geometry monitor)))
(defun frame-x (&optional frame)
  (cdr (assq 'left (frame-parameters))))
(defun frame-y (&optional frame)
  (cdr (assq 'top (frame-parameters))))

(defun frame-max-height (&optional frame)
  (interactive)
  (set-frame-parameter frame 'fullscreen 'fullheight))

(defun dock-frame-left (&optional frame monitor)
  (interactive)
  (setq frame (or frame (selected-frame)))
  (setq monitor (or monitor (frame-monitor-attributes)))
  (let* ((monitor-list (-take-while
                       (lambda (x) (not (equal monitor x)))
                       (display-monitor-attributes-list)))
         (widths (mapc #'monitor-width monitor-list))
         (x (apply '+ widths))
         (y (frame-y frame)))
    (set-frame-position frame x y)))

(defun dock-frame-right (&optional frame monitor)
  (interactive)
  (setq frame (or frame (selected-frame)))
  (setq monitor (or monitor (frame-monitor-attributes)))
  (let* ((monitor-list (-take-while
                       (lambda (x) (not (equal monitor x)))
                       (display-monitor-attributes-list)))
         (widths (mapc #'monitor-width monitor-list))
         (x (+ (apply '+ widths) (monitor-width monitor)))
         (y (frame-y frame)))
    (set-frame-position frame (- 0 x) y)))

(defun size-frame-default ()
  (set-frame-parameter nil 'width column-wrap-hard)
  (frame-max-height))

(global-set-key (kbd "<C-S-f11>") 'frame-max-height)
(global-set-key (kbd "<C-f11>") 'dock-frame-left)
(global-set-key (kbd "<C-f12>") 'dock-frame-right)

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

;; Smart parens
(use-package smartparens
  :ensure t
  :bind
  (("C-S-k" . sp-kill-hybrid-sexp))
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (require 'smartparens-config)
  :demand
  :diminish smartparens-mode)

;; Show column numbers
(column-number-mode)

;;------------------------------------------------------------------------------
;; Colors
(set-face-foreground 'font-lock-comment-face "gray")
(set-face-foreground 'font-lock-string-face "FireBrick")
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

;; Hex colors for HTML etc
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'php-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'html-mode-hook 'hexcolour-add-to-font-lock)

;;------------------------------------------------------------------------------
;; Global key bindings
(setq personal-keybindings nil)
(global-set-key "\C-z" 'undo)
(global-set-key "\C-o" 'goto-line)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-k" 'compile)

;; Action of home key
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key [home] 'beginning-of-line-or-indentation)

;; Turn off insert
(global-set-key [insert] (lambda () (interactive)))
(global-set-key [insertchar] (lambda () (interactive)))

;; Kill-ring menu
(global-set-key "\C-cy" '(lambda () (interactive) (popup-menu 'yank-menu)))

;; Cycle buffers/windows with F5-F8
(global-set-key [f5] 'next-multiframe-window)
(global-set-key [f6] 'previous-multiframe-window)
(global-set-key [f7] 'previous-buffer)
(global-set-key [f8] 'next-buffer)

;; Moving windows
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Highlight symbols
(global-set-key [f3] 'highlight-symbol-at-point)
(global-set-key [(shift f3)] 'hi-lock-mode)

;;------------------------------------------------------------------------------
;; Smooth scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

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
                                ("\\.js$" . js2-mode)
                                ("\\.qml$" . js2-mode)
                                ("\\.json$" . json-mode)
                                ("\\.ui$" . nxml-mode)
                                ("SConstruct" . python-mode)
                                ("SConscript" . python-mode)
                                ("\\.ml[iyl]?$" . caml-mode)
                                ("\\.pb$" . protobuf-mode)
                                ("\\.proto$" . protobuf-mode)
                                ("\\.presql$" . sql-mode))
                              auto-mode-alist))

;;------------------------------------------------------------------------------
;; Bookmarks
(use-package bm
  :ensure t
  :bind (("<C-f2>" . bm-toggle)
         ("<f2>" . bm-next)
         ("<S-f2>" . bm-previous)))

;;------------------------------------------------------------------------------
;; IDO & smex
(use-package flx-ido
  :ensure t
  :config
  (ido-mode 1)
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point nil)
  (setq ido-create-new-buffer 'always)
  (flx-ido-mode 1)
  (setq ido-use-faces nil))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode))

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
  :diminish undo-tree-mode)

;;------------------------------------------------------------------------------
;; Google-this
(use-package google-this
  :ensure t
  :bind
  (("\C-cr" . google-this-cpp-reference)))

;;------------------------------------------------------------------------------
;; Ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
	 ("C-c DEL" . ace-jump-mode-pop-mark))
  :functions ace-jump-mode-enable-mark-sync
  :config (ace-jump-mode-enable-mark-sync))

;;------------------------------------------------------------------------------
;; Flycheck
(use-package flycheck
  :ensure t
  :defer 2
  :config
  (flycheck-define-checker javascript-jslint-reporter
    "A JavaScript syntax and style checker based on JSLint Reporter.

See URL `https://github.com/FND/jslint-reporter'."
    :command ("~/.emacs.d/jslint-reporter" source)
    :error-patterns
    ((error line-start (1+ nonl) ":" line ":" column ":" (message) line-end))
    :modes (js-mode js2-mode js3-mode)))

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
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;;------------------------------------------------------------------------------
;; Autocomplete: irony
(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

(use-package company-irony
  :ensure t
  :config
  ;; (optional) adds CC special commands to `company-begin-commands' in order to
  ;; trigger completion at interesting places, such as after scope operator
  ;;     std::|
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

;; On first install, run M-x irony-install-server
;; To set compilation database, use M-x irony-cdb-json-add-compile-commands-path
;; Use cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON to generate compile_commands.json
;; The compilation db list is in ~/.emacs.d/irony/cdb-json-projects

;;------------------------------------------------------------------------------
;; Autocomplete: company
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (add-to-list 'company-backends 'company-irony))

(use-package company-ghc
  :ensure t)

(use-package company-ghci
  :ensure t)

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

;;------------------------------------------------------------------------------
;; Autocomplete: auto-complete
;; (use-package auto-complete
;;   :ensure t
;;   :defer 1
;;   :config
;;   (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;   (ac-config-default)
;;   (ac-set-trigger-key "<tab>"))

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
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key [tab] 'tab-indent-or-complete)

;;------------------------------------------------------------------------------
;; Basic offset = 2
(setq-default c-basic-offset 2)
(setq lua-indent-level 2)
(setq js-indent-level 2)
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

;;------------------------------------------------------------------------------
;; Compilation

;; Compilation
(global-set-key [\M-up] 'previous-error)
(global-set-key [\M-down] 'next-error)
(setq compilation-scroll-output t)

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

;; inspired by jds-find-tags-file in http://www.emacswiki.org/emacs/EmacsTags
(defun find-sconstruct ()
  "recursively searches upwards from buffer's current dir for file named SConstruct and returns that dir. Or nil if not found or if buffer is not visiting a file"
  (cl-labels
      ((find-sconstruct-r (path)
                          (let* ((parent (file-name-directory path))
                                 (possible-file (concat parent "SConstruct")))
                            (cond
                             ((file-exists-p possible-file)
                              (throw 'found-it possible-file))
                             ((string= "/SConstruct" possible-file)
                              (error "No SConstruct found"))
                             (t (find-sconstruct-r (directory-file-name parent)))))))
    (if (buffer-file-name)
        (catch 'found-it
          (find-sconstruct-r (buffer-file-name)))
      (error "Buffer is not visiting a file"))))

(defun project-root ()
  (file-name-directory (find-sconstruct)))

(if (eq system-type 'cygwin)
    (setq compile-command '(concat "cd " (project-root) " && /usr/local/bin/scons"))
  (setq compile-command '(concat "cd " (project-root) " && scons")))

(setq compilation-read-command nil)

;;------------------------------------------------------------------------------
;; CEDET

(use-package semantic
  :config
  (mapc (lambda (m) (add-to-list 'semantic-default-submodes m))
      '(global-semantic-mru-bookmark-mode
        global-semanticdb-minor-mode
        global-semantic-idle-scheduler-mode
        global-semantic-highlight-func-mode
        global-semantic-idle-summary-mode
        ))
  (semantic-mode 1)
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)
  )

;; customisation of modes
(defun my-c-mode-cedet-hook ()
  (require 'eassist)
  ;(local-set-key "\C-c#" 'semantic-decoration-include-visit)
  ;(local-set-key "\C-x\C-h" 'eassist-switch-h-cpp)
  (local-set-key "\C-cm" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref))

(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;;------------------------------------------------------------------------------
;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  :bind
  (("\C-xf" . projectile-find-file)
   ("\C-x\C-g" . projectile-grep)
   ("\C-c#" . projectile-find-file-dwim)
   ("\C-x\C-h" . projectile-find-other-file)))

;;------------------------------------------------------------------------------
;; Python mode
(add-hook 'python-mode-hook (lambda ()
                          (flycheck-select-checker 'python-flake8)
                          (flycheck-mode)))

;;------------------------------------------------------------------------------
;; Javascript
(use-package js2-mode
  :ensure t
  :defer 5)
(use-package json-mode
  :ensure t
  :defer 5)

(if (eq system-type 'gnu/linux)
  (add-hook 'js2-mode-hook (lambda ()
                            (flycheck-select-checker 'javascript-jslint-reporter)
                            (flycheck-mode))))

;;------------------------------------------------------------------------------
;; XML
(use-package nxml-mode
  :ensure t
  :defer 5)

;;------------------------------------------------------------------------------
;; Protobufs
;; there is a bug in protobuf mode that requires cl
(require 'cl)
(use-package protobuf-mode
  :ensure t
  :defer 1)

;;------------------------------------------------------------------------------
;; Haskell mode
(use-package haskell-mode
  :ensure t
  :defer 1)
(use-package flycheck-haskell
  :ensure t
  :defer 1)

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "M-k") 'haskell-compile))
(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "M-k") 'haskell-compile))
(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c v c") 'haskell-cabal-visit-file))
(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    ;;(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-l") 'inferior-haskell-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

;;------------------------------------------------------------------------------
;; Git interactions

;; git-gutter-fringe
(use-package git-gutter-fringe+
  :ensure t
  :defer 5
  :config
  (setq git-gutter-fr+-side 'right-fringe)
  (global-git-gutter+-mode 1)
  :diminish git-gutter+-mode)

;; mo-git-blame
(use-package mo-git-blame
  :ensure t
  :bind
  (("\C-cb" . mo-git-blame-current)))

;; magit
(use-package magit
  :ensure t
  :bind
  (("\C-cg" . magit-status)))

(setq magit-last-seen-setup-instructions "1.4.0")

;;------------------------------------------------------------------------------
;; Org-mode
(use-package org
  :ensure org-plus-contrib
  :commands (org-mode)
  :mode ("\\.org\\'" . org-mode)
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
  (setq org-beamer-outline-frame-title "Contents"))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-ci" 'org-iswitchb)

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-latex-packages-alist '("" "minted" nil))
            (define-key org-mode-map "\M-Q" 'toggle-truncate-lines)
            (require 'ox-latex)
            (require 'ox-beamer)
            (add-to-list 'org-beamer-environments-extra
             '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))))

;;------------------------------------------------------------------------------
;; Tags
(defun my/find-tags-file-r (path)
  "find the tags file from the parent directories"
  (let* ((parent (file-name-directory path))
         (possible-tags-file (concat parent ".git/TAGS")))
    (cond
     ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
     ((string= "/.git/TAGS" possible-tags-file) (error "no tags file found"))
     (t (my/find-tags-file-r (directory-file-name parent))))))

(defun my/find-tags-file ()
  "recursively searches each parent directory for a file named
'TAGS' in the bare .git repo and returns the path to that file or
nil if a tags file is not found. Returns nil if the buffer is not
visiting a file"
  (if (buffer-file-name)
      (catch 'found-it
        (my/find-tags-file-r (buffer-file-name)))
    (error "buffer is not visiting a file")))

(defun my/set-tags-file-path ()
  "calls `my/find-tags-file' to recursively search up the
directory tree to find a file named '.git/TAGS'. If found, set
'tags-table-list' with that path as an argument otherwise raises
an error."
  (interactive)
  (condition-case nil
      (add-to-list 'tags-table-list (my/find-tags-file))
    (error nil)))

;; find the TAGS file after opening the source file
(add-hook 'find-file-hook
          '(lambda () (my/set-tags-file-path)))

(use-package etags-select
  :ensure t)

(use-package etags-table
  :ensure t)

(setq tags-revert-without-query 1
      etags-table-search-up-depth 10)
(global-set-key "\M-." 'etags-select-find-tag-at-point)

;; prevent prompt on opening large TAGS file
(setq large-file-warning-threshold 100000000)

;;------------------------------------------------------------------------------
;; Other lesser-used modes

;; vlfi
(use-package vlf
  :ensure t
  :defer 5)

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
(global-set-key (kbd "C-x C-S-e") 'eval-and-replace)

;;------------------------------------------------------------------------------
;; Insert current time/date
(defvar current-date-time-format "%a %b %d %H:%M:%S %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n")
  )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  (insert "\n")
  )

(global-set-key "\C-c\C-d" 'insert-current-date-time)
(global-set-key "\C-c\C-t" 'insert-current-time)

;;------------------------------------------------------------------------------
;; Insert generated UUIDs
(random t)

(defun insert-random-uuid ()
  "Insert a random universally unique identifier (UUID).
A UUID is a 128-bit (16 byte) number formatted in a certain way.
Example of a UUID: 1df63142-a513-X850-Y1a3-535fc3520c3d
Where X is 4 and Y is one of {8,9,a,b}."
  (interactive)
  (insert
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
           (random (expt 16 6)))))

(global-set-key "\C-cu" 'insert-random-uuid)

(defun random-xcode-uuid-string ()
   (format "%04X%04X%04X%04X%04X%04X"
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))))

(defun insert-random-xcode-uuid ()
  "Insert a random universally unique identifier (UUID) suitable for use in XCode projects.
An XCode UUID is a 96-bit (12 byte) number formatted as a hex string.
Example of an XCode UUID: a513b85041a3535fc3520c3d."
  (interactive)
  (insert (random-xcode-uuid-string)))

(global-set-key "\C-cx" 'insert-random-xcode-uuid)

(defun insert-xcode-header-template ()
  (interactive)
  (let ((build-file-uuid (random-xcode-uuid-string))
        (file-ref-uuid (random-xcode-uuid-string)))
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
  (let ((build-file-uuid (random-xcode-uuid-string))
        (file-ref-uuid (random-xcode-uuid-string)))
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
;; Start server
(require 'server)
(server-start)

;;------------------------------------------------------------------------------
;; Size the frame
(setq default-frame-height (frame-height))
(setq default-frame-alist
  (append
    `((width . ,column-wrap-hard)
      (height . ,default-frame-height))
    default-frame-alist))

(size-frame-default)

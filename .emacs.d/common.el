;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Hide minor modes
(use-package diminish
  :ensure t)

;;------------------------------------------------------------------------------
;; UTF8 defaults
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8
      default-file-name-coding-system 'utf-8
      locale-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(setq default-input-method nil)

;;------------------------------------------------------------------------------
;; Clean up display
(tool-bar-mode -1)
(menu-bar-mode -1)
(display-battery-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
      initial-scratch-message ""
      visible-bell 1
      x-stretch-cursor t)

;; Display defaults
(setq column-wrap-soft 80
      column-wrap-hard 100)
(setq sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Keep whitespace clean
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
;; Autosaves/backups
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/autosaves" t)
(setq backup-directory-alist
      `((".*" . , "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/autosaves/" t)))
(setq create-lockfiles nil)

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
;; More usable defaults
(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq load-prefer-newer t
      apropos-do-all t)
(diminish 'abbrev-mode)

;;------------------------------------------------------------------------------
;; Smooth scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(setq mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;;------------------------------------------------------------------------------
;; Prevent prompt on opening large TAGS file
(setq large-file-warning-threshold 100000000)


;;------------------------------------------------------------------------------
;; Better process interaction
(setq read-process-output-max (* 2 1024 1024))

;;------------------------------------------------------------------------------
;; vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  (require 'vertico-multiform)
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package orderless
  :ensure t
  :config
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)
        completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))))
  :after vertico)

;;------------------------------------------------------------------------------
;; Undo
(use-package vundo
  :ensure t
  :bind (("C-x _" . vundo)))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
        undo-tree-auto-save-history nil)
  :diminish undo-tree-mode
  :bind (("C-z" . undo-tree-undo)
         ("C-x u" . undo-tree-visualize)))

;;------------------------------------------------------------------------------
;; Google-this
(use-package google-this
  :ensure t
  :commands (google-this-parse-and-search-string)
  :config
  (setq google-this-browse-url-function 'eww-browse-url)
  :bind
  (("C-c <f1>" . duckduckgo-this-cpp-reference)))

(defun duckduckgo-this-lucky-search-url ()
  "Return the url for a feeling-ducky duckduckgo search."
  (format "https://duckduckgo.com/?q=\\%%s"))

(defun duckduckgo-this-cpp-reference ()
  "Visit the most probable cppreference.com page for this word."
  (interactive)
  (google-this-parse-and-search-string
   (concat (thing-at-point 'symbol) " site:cppreference.com")
   nil (duckduckgo-this-lucky-search-url)))

;;------------------------------------------------------------------------------
;; Avy
(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-word-or-subword-1)
         ("C-," . avy-pop-mark)
         ("C-j" . avy-goto-char-timer)))

(use-package ace-window
  :ensure t
  :bind ("C-c w" . ace-window))

;;------------------------------------------------------------------------------
;; smart-scan: use M-n and M-p to jump to next/prev thing at point
;; remove M-' from smartscan keymap: it's used by surround
(defvar smartscan-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "M-n") 'smartscan-symbol-go-forward)
    (define-key m (kbd "M-p") 'smartscan-symbol-go-backward)
    m)
  "Keymap for `smartscan'.")

(use-package smartscan
  :ensure t
  :config (global-smartscan-mode t))

;;------------------------------------------------------------------------------
;; use surround for changing delimiters
(use-package surround
  :ensure t
  :bind-keymap ("M-'" . surround-keymap)
  :bind (:map surround-keymap
              ("M-'" . smartscan-symbol-replace)))

;;------------------------------------------------------------------------------
;; visual-regexp-steroids: better regexp searching
(use-package visual-regexp-steroids
  :ensure t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-r" . vr/isearch-backward)
         ("C-s" . vr/isearch-forward)))

;; use case-insensitive searching in general
(setq case-fold-search t)

;;------------------------------------------------------------------------------
;; make shell files executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;------------------------------------------------------------------------------
;; cycle CamelCase, snake_case, kebab-case etc
(use-package string-inflection
  :ensure t
  :bind (("C-c -" . string-inflection-all-cycle)
         ("C-c _" . string-inflection-toggle)))

;;------------------------------------------------------------------------------
;; better zap-to-char
(use-package zzz-to-char
  :ensure t
  :bind (("M-z" . zzz-up-to-char)))

;;------------------------------------------------------------------------------
;; browse kill-ring
(use-package browse-kill-ring
  :ensure t
  :bind (("C-M-y" . browse-kill-ring)))

;;------------------------------------------------------------------------------
;; symbol-overlay
(use-package symbol-overlay
  :ensure t
  :bind (("<f3>" . symbol-overlay-put)))

;;------------------------------------------------------------------------------
;; simple modeline functionality
(defun my-truncate-buffer-name (buf-name)
  (let ((len (length buf-name)))
    (cond ((> len 30)
           (concat "..."
                   (substring buf-name (- len 27) len)))
          (t buf-name))))

(setq auto-revert-check-vc-info t)

;;------------------------------------------------------------------------------
;; keep track of the cursor
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;;------------------------------------------------------------------------------
;; consult extras
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ;;("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ;;("M-#" . consult-register-load)
         ;;("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;;("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)              ;; Alternative: consult-flymake
         ("C-o" . consult-goto-line)               ;; orig. (custom) goto-line
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-m" . consult-imenu)                   ;; orig. back-to-indentation
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

(use-package consult-flycheck
  :ensure t
  :after consult)

(use-package consult-dir
  :ensure t
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;------------------------------------------------------------------------------
;; extras in the minibuffer
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (;;("M-a" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-a" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;;------------------------------------------------------------------------------
;; corfu for completion
(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))
(define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

(use-package corfu-terminal
  :ensure t
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  :after corfu)

;;------------------------------------------------------------------------------
;; powerthesaurus
(use-package powerthesaurus
  :ensure t
  :bind
  (("C-c <f2>" . powerthesaurus-lookup-dwim)))

;;------------------------------------------------------------------------------
;; embark
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;------------------------------------------------------------------------------
;; time and world clock
(use-package time
  :ensure t
  :custom
  (world-clock-time-format "%a %e %b %T %Z")
  (display-time-day-and-date t)
  (display-time-default-load-average nil)
  (zoneinfo-style-world-list
   '(("America/Los_Angeles" "Folsom")
     ("America/Phoenix" "Chandler")
     ("America/Denver" "Denver")
     ("Asia/Kolkata" "Bangalore")
     ("Asia/Singapore" "Penang")
     ("Australia/Brisbane" "Brisbane"))))

;;------------------------------------------------------------------------------
;; increment numbers in region
(defun replace-numbers-in-region-with-increments ()
  "Replace numbers in a given region with numbers incrementing from zero."
  (interactive)
  (replace-regexp "[0-9]+"
                  '((lambda (x n) (number-to-string n)) . 0)
                  nil (region-beginning) (region-end)))
(bind-key "C-S-#" 'replace-numbers-in-region-with-increments)

;;------------------------------------------------------------------------------
;; fuzzy find
(use-package fzf
  :ensure t
  :bind
  (("C-c z f" . fzf-find-file)
   ("C-c z g" . fzf-grep)
   ("C-c z z" . fzf))
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        fzf/grep-command "rg --no-heading -nH"
        ;; fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;;------------------------------------------------------------------------------
;; cycle accented characters at point
(use-package cyclekey
  :load-path (lambda () (concat dotfile-dir ".emacs.d/site-lisp/"))
  :ensure nil
  :init
  (setq cyclekey-languages '("French" "German" "Spanish")
        cyclekey-save-languages nil)
  :config
  (cyclekey-init)
  :bind ("M-o" . cyclekey-cycle))

;;------------------------------------------------------------------------------
;; Prettier modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  (setq doom-modeline-minor-modes t))

;;------------------------------------------------------------------------------
;; minions: minor modes in modeline
(use-package minions
  :ensure t
  :config (minions-mode 1))

;;------------------------------------------------------------------------------
;; Writable grep buffer
(use-package wgrep
  :ensure t)

;;------------------------------------------------------------------------------
;; Lookup words
(setopt dictionary-server "dict.org")
(bind-key "C-c <f3>" 'dictionary-lookup-definition)

;;------------------------------------------------------------------------------
;; Bind extra help keys
(use-package help
  :bind
  (:map help-map
        ("=" . describe-char)
        ("j" . describe-face)
        ("-" . describe-keymap)))

;;------------------------------------------------------------------------------
;; Use nerd fonts
(use-package nerd-icons
  :ensure t
  :custom (nerd-icons-font-family "BerkeleyMono Nerd Font"))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :custom
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;------------------------------------------------------------------------------
;; treemacs: tree interface for opening files
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-default-visit-action 'treemacs-visit-node-close-treemacs
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-project-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("<f12>"     . treemacs-add-and-display-current-project-exclusively)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
        ("<f12>"     . treemacs-quit)
        ("<left>"    . treemacs-collapse-parent-node)
        ("<right>"   . treemacs-RET-action)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

;;------------------------------------------------------------------------------
;; cape: completion at point
(use-package cape
  :ensure t
  :bind (("C-c x" . cape-prefix-map)
         ("C-/" . completion-at-point))
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; prevent undo-tree from overriding this keybind
(eval-after-load 'undo-tree
  '(define-key undo-tree-map (kbd "C-/") nil))

;;------------------------------------------------------------------------------
;; keycast: help people who are pairing/watching
(use-package keycast
  :ensure t
  :config
  (defun my/toggle-keycast()
    (interactive)
    (if (member '("" keycast-mode-line " ") global-mode-string)
        (progn (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))
               (remove-hook 'pre-command-hook 'keycast--update)
               (message "Keycast OFF"))
      (add-to-list 'global-mode-string '("" keycast-mode-line " "))
      (add-hook 'pre-command-hook 'keycast--update t)
      (message "Keycast ON")))
  :bind ("C-c t k" . my/toggle-keycast))

;;------------------------------------------------------------------------------
;;; A Help Transient on C-S-h
(use-package helpful
  :ensure t
  :bind ("C-h f" . #'helpful-callable)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key)
  ("C-h x" . #'helpful-command))

(transient-define-prefix hrm-help-transient ()
  "Help commands that I use. A subset of C-h with others thrown in."
  ["Help Commands"
   ["Mode & Bindings"
    ("m" "Mode" describe-mode)
    ("M" "Minor Modes" consult-minor-mode-menu)
    ("b" "Major Bindings" which-key-show-full-major-mode)
    ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
    ("d" "Descbinds" describe-bindings) ; or embark-bindings
    ("t" "Top Bindings  " which-key-show-top-level)
    ]
   ["Describe"
    ("C" "Command" helpful-command)
    ("f" "Function" helpful-callable)
    ("v" "Variable " helpful-variable)
    ("k" "Key" helpful-key)
    ("s" "Symbol" helpful-symbol)
    ("l" "Library" apropos-library)
    ]
   ["Info on"
    ("C-c" "Command" Info-goto-emacs-command-node)
    ("C-f" "Function" info-lookup-symbol)
    ("C-v" "Variable" info-lookup-symbol) ; fails if transient-detect-key-conflicts
    ("C-k" "Key" Info-goto-emacs-key-command-node)
    ("C-s" "Symbol" info-lookup-symbol)
    ]
   ["Goto Source"
    ""
    ("F" "Function" find-function-other-frame)
    ("V" "Variable" find-variable-other-frame)
    ("K" "Key" find-function-on-key-other-frame)
    ""
    ("L" "Library" find-library-other-frame)
    ]
   ["Apropos"
    ("ac" "Command" apropos-command)
    ("af" "Function" apropos-function)
    ("av" "Variable" apropos-variable)
    ("aV" "Value" apropos-value)
    ("aL" "Local Value" apropos-local-value)
    ("ad" "Documentation" apropos-documentation)
    ]
   ]
  [
   ["Internals"
    ("I" "Input Method" describe-input-method)
    ("G" "Language Env" describe-language-environment)
    ("S" "Syntax" describe-syntax)
    ("T" "Categories" describe-categories)
    ("O" "Coding System" describe-coding-system)
    ("o" "Coding Briefly" describe-current-coding-system-briefly)
    ("T" "Display Table" describe-current-display-table)
    ("e" "Echo Messages" view-echo-area-messages)
    ("H" "Lossage" view-lossage)
    ]
   ["Describe"
    ("." "At Point" helpful-at-point)
    ("c" "Key Short" describe-key-briefly)
    ("p" "Key Map" describe-keymap)
    ("A" "Face" describe-face)
    ("i" "Icon" describe-icon)
    ("w" "Where Is" where-is)
    ("=" "Position" what-cursor-position)
    ("g" "Shortdoc" shortdoc-display-group)
    ]
   ["Info Manuals"
    ("C-i" "Info" info)
    ("C-4" "Other Window" info-other-window)
    ("C-e" "Emacs" info-emacs-manual)
    ("C-l" "Elisp" info-elisp-manual)
    ("C-r" "Pick Manual" info-display-manual)
    ]
   ["External"
    ("C" "cppreference" duckduckgo-this-cpp-reference)
    ("N" "Man" consult-man)
    ("W" "Dictionary" powerthesaurus-lookup-dwim)
    ]
   ]
  )
(global-set-key (kbd "C-S-h") 'hrm-help-transient)

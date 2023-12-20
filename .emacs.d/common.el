;;------------------------------------------------------------------------------
;; Hide minor modes
(use-package diminish
  :ensure t)

;;------------------------------------------------------------------------------
;; UTF8 defaults
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
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
;; vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

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
  :bind (("C-z" . undo)
         ("C-x u" . vundo)))

;;------------------------------------------------------------------------------
;; Google-this
(use-package google-this
  :ensure t
  :commands (google-this-parse-and-search-string)
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
         ("C-," . avy-pop-mark)))

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

;;------------------------------------------------------------------------------
;; neotree: tree interface for opening files

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
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
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
         ("M-g i" . consult-imenu)
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
         ("M-s m" . consult-multi-occur)
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

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
  :init (doom-modeline-mode 1))

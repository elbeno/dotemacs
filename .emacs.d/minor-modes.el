;;------------------------------------------------------------------------------
;; Show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;;------------------------------------------------------------------------------
;; Syntactic close
(use-package syntactic-close
  :ensure t
  :bind ("M-]" . syntactic-close))

;;------------------------------------------------------------------------------
;; Show column numbers
(column-number-mode)

;;------------------------------------------------------------------------------
;; Auto-revert buffers
(global-auto-revert-mode)
(diminish 'auto-revert-mode)

;;------------------------------------------------------------------------------
;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;------------------------------------------------------------------------------
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
  :hook (prog-mode css-mode)
  :diminish rainbow-mode)

;;------------------------------------------------------------------------------
;; Highlight numbers, quoted things, escape sequences
(use-package highlight-numbers
  :ensure t
  :config
  :hook (prog-mode . highlight-numbers-mode)
  :diminish highlight-numbers-mode)

(use-package highlight-quoted
  :ensure t
  :config
  :hook (prog-mode . highlight-quoted-mode)
  :diminish highlight-quoted-mode)

(use-package highlight-escape-sequences
  :ensure t
  :config
  :hook (prog-mode . hes-mode)
  :diminish hes-mode)

;;------------------------------------------------------------------------------
;; doc hints
(use-package eldoc
  :ensure t
  :hook
  (emacs-lisp-mode . eldoc-mode)
  (lisp-interaction-mode . eldoc-mode)
  (c++-mode . eldoc-mode)
  :diminish eldoc-mode)

;;------------------------------------------------------------------------------
;; comment-dwim-2
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;;------------------------------------------------------------------------------
;; smart tabs
(use-package smart-tab
  :ensure t
  :config
  (global-smart-tab-mode 1)
  :diminish smart-tab-mode)

;;------------------------------------------------------------------------------
;; which-key to give help on keys
(use-package which-key
  :ensure t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode)
  :diminish which-key-mode)

;;------------------------------------------------------------------------------
;; expand-region
(use-package expand-region
  :ensure t
  :bind (("C-'" . er/expand-region)
         ("C-@" . er/contract-region)))
(delete-selection-mode 1)

;;------------------------------------------------------------------------------
;; region-state: show lines/characters selected
(use-package region-state
  :ensure t
  :config
  (region-state-mode)
  :diminish region-state-mode)

;;------------------------------------------------------------------------------
;; prettify symbols in modes that support it
;; unprettify the symbol at point
(global-prettify-symbols-mode)
(setq prettify-symbols-unprettify-at-point 'right-edge)

;;------------------------------------------------------------------------------
;; Bookmarks
(use-package bm
  :ensure t
  :bind (("C-<f2>" . bm-toggle)
         ("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)))

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
;; Projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t
        projectile-use-git-grep t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :bind (("C-x f" . projectile-find-file)
         ("C-c #" . projectile-find-file-dwim)
         ("C-x C-h" . projectile-find-other-file))
  :diminish projectile-mode)
(projectile-mode +1)

;;------------------------------------------------------------------------------
;; RIPGrep
(use-package ripgrep
  :ensure t
  :bind (("C-x g" . projectile-ripgrep)))

;;------------------------------------------------------------------------------
;; Flycheck
(use-package flycheck
  :ensure t
  :hook ((c++-mode . flycheck-mode)
         (python-mode . flycheck-mode))
  :diminish flycheck-mode)

;;------------------------------------------------------------------------------
;; Emojify
(use-package emojify
  :ensure t
  :init (global-emojify-mode)
  :bind (("C-c e" . emojify-insert-emoji))
  :diminish emojify-mode)
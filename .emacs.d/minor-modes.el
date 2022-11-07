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
;; Show column/line numbers
(column-number-mode)
(if (version< emacs-version "26")
    (global-linum-mode)
  (global-display-line-numbers-mode))

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
  :hook
  (prog-mode . rainbow-mode)
  (css-mode . rainbow-mode)
  :diminish rainbow-mode)

;;------------------------------------------------------------------------------
;; Highlight numbers, quoted things, escape sequences
(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode)
  :diminish highlight-numbers-mode)

(use-package highlight-quoted
  :ensure t
  :hook (prog-mode . highlight-quoted-mode)
  :diminish highlight-quoted-mode)

(use-package highlight-escape-sequences
  :ensure t
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
  (which-key-mode)
  :diminish which-key-mode)

;;------------------------------------------------------------------------------
;; expand-region
(use-package expand-region
  :ensure t
  :bind (("C-}" . er/expand-region)
         ("C-{" . er/contract-region))
  :config
  (require 'the-org-mode-expansions)
  (setq expand-region-smart-cursor t
        expand-region-fast-keys-enabled nil))
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
(setq-default fill-column column-wrap-soft
              truncate-lines t)

(setq-default whitespace-line-column column-wrap-soft
              whitespace-style '(face lines-tail))

(unless (> emacs-major-version 27)
  (setq my-post-fill-column-fg (if (display-graphic-p) "gray20" "yellow"))
  (add-hook 'prog-mode-hook (lambda ()
                              (unless (string-match-p (regexp-quote "*temp*") (buffer-name))
                                (whitespace-mode)
                                (set-face-attribute 'whitespace-line nil
                                                    :foreground my-post-fill-column-fg)))))

(when (> emacs-major-version 27)
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))

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
  :hook
  (prog-mode . projectile-mode)
  :diminish projectile-mode)

;;------------------------------------------------------------------------------
;; RIPGrep
(use-package ripgrep
  :ensure t
  :config
  (setq ripgrep-arguments '("--hidden"))
  :bind (("C-x g" . projectile-ripgrep)))

;;------------------------------------------------------------------------------
;; Flycheck
(use-package flycheck
  :ensure t
  :hook sh-mode)

(use-package flycheck-pos-tip
  :ensure t
  :hook flycheck-mode)

;;------------------------------------------------------------------------------
;; Emojify
(use-package emojify
  :ensure t
  :init (global-emojify-mode)
  :bind (("C-c e" . emojify-insert-emoji))
  :diminish emojify-mode)

;;------------------------------------------------------------------------------
;; filladapt
(use-package filladapt
  :ensure t
  :diminish filladapt-mode
  :config
  (setq-default filladapt-mode t))

;;------------------------------------------------------------------------------
;; grugru (rotate text at point)
(use-package grugru
  :ensure t
  :config
  (grugru-highlight-mode)
  (grugru-define-global 'symbol (grugru-metagenerator-keep-case '("yes" "no")))
  (grugru-define-global 'symbol (grugru-metagenerator-keep-case '("true" "false")))
  (grugru-define-global 'symbol (grugru-metagenerator-keep-case '("width" "height")))
  (grugru-define-global 'symbol (grugru-metagenerator-keep-case '("left" "right")))
  (grugru-define-global 'symbol (grugru-metagenerator-keep-case '("top" "bottom")))
  (grugru-define-global 'symbol (grugru-metagenerator-keep-case '("north" "south" "east" "west")))
  (grugru-define-on-major-mode '(c++-mode) 'symbol '("public" "protected" "private"))
  (grugru-define-on-major-mode '(c++-mode) 'symbol '("class" "struct"))
  (grugru-define-on-major-mode '(c++-mode) 'symbol '("static_cast" "dynamic_cast" "reinterpret_cast" "const_cast" "bit_cast"))
  :bind
  (("C-c /" . grugru-forward)
   ("C-c C-/" . grugru-backward))
  :diminish grugru-highlight-mode)

;; -*- lexical-binding: t; -*-
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
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;------------------------------------------------------------------------------
;; Auto-revert buffers
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
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
  (emacs-lisp-mode lisp-interaction-mode c++-mode c++-ts-mode)
  :diminish eldoc-mode)

;;------------------------------------------------------------------------------
;; smart tabs
(use-package smart-tab
  :ensure t
  :config
  (global-smart-tab-mode 1)
  (setq smart-tab-completion-functions-alist nil
        smart-tab-user-provided-completion-function #'completion-at-point)
  :diminish smart-tab-mode)

;;------------------------------------------------------------------------------
;; expand-region
(use-package expand-region
  :ensure t
  :bind (("C-{" . er/expand-region)
         ("C-}" . er/contract-region))
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
  :init
  (setq bm-restore-repository-on-load t)
  :config
  (setq bm-cycle-all-buffers t
        bm-marker 'bm-marker-right
        bm-repository-file (expand-file-name "bm-repository" user-emacs-directory))
  (setq-default bm-buffer-persistence t)
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'bm-after-goto-hook 'org-bookmark-jump-unhide)
  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle)))

;;------------------------------------------------------------------------------
;; Mark the fill column
(setq-default fill-column column-wrap-soft
              truncate-lines t)

(setq-default whitespace-line-column column-wrap-soft
              whitespace-style '(face lines-tail))

(unless (> emacs-major-version 27)
  (add-hook 'prog-mode-hook
            (lambda ()
              (let ((my-post-fill-column-fg
                    (if (my/graphic-mode-p) "gray20" "yellow")))
                (unless (string-match-p (regexp-quote "*temp*") (buffer-name))
                  (whitespace-mode)
                  (set-face-attribute 'whitespace-line nil
                                      :foreground my-post-fill-column-fg))))))

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
         ("C-c o" . projectile-find-other-file))
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
  :hook (flycheck-mode . flycheck-pos-tip-mode))

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
  (when (fboundp 'c++-ts-mode)
    (grugru-define-on-major-mode '(c++-ts-mode) 'symbol '("public" "protected" "private"))
    (grugru-define-on-major-mode '(c++-ts-mode) 'symbol '("class" "struct"))
    (grugru-define-on-major-mode '(c++-ts-mode) 'symbol '("static_cast" "dynamic_cast" "reinterpret_cast" "const_cast" "bit_cast")))
  :bind
  (("C-c /" . grugru-forward)
   ("C-c C-/" . grugru-backward))
  :diminish grugru-highlight-mode)

;;------------------------------------------------------------------------------
;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;;------------------------------------------------------------------------------
;; format-all
(use-package format-all
  :ensure t
  :init
  (defun my/format-all-buffer-setup ()
    (format-all-mode)
    (let ((language (format-all--language-id-buffer)))
      (format-all--set-chain language
                             (format-all--get-default-chain language))))
  :hook ((c++-mode c++-ts-mode cmake-mode) . my/format-all-buffer-setup)
  :bind (("C-c f" . format-all-buffer)))

;;------------------------------------------------------------------------------
;; jinx for spellchecking
(when (locate-file "enchant-2" exec-path)
  (use-package jinx
    :ensure t
    :hook (emacs-startup . global-jinx-mode)
    :bind ([remap ispell-word] . jinx-correct)
    :config (setq jinx-languages "en_GB")))

;;------------------------------------------------------------------------------
;; writegood-mode to avoid lazy writing
(use-package writegood-mode
  :ensure t
  :bind (("C-c C-r m" . writegood-mode)
         ("C-c C-r l" . writegood-grade-level)
         ("C-c C-r e" . writegood-reading-ease)))

;;------------------------------------------------------------------------------
;; zone out
(use-package zone-rainbow
  :ensure t
  :config
  (setq zone-programs '[zone-rainbow]))

(when (locate-file "wn" exec-path)
  (require 'zone-words)
  (setq zone-programs (vconcat [zone-words] zone-programs))
  (zone-when-idle 120))

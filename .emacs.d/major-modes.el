;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

;; Shells
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "/bin/ls")

;; For editing command-line in bash:
;; When $EDITOR="emacsclient -nw", C-x C-e on the command line
;; allows editing the line in emacs
(add-to-list 'auto-mode-alist '("\\`/tmp/bash-fc\.[a-z0-9A-Z]+\\'" . sh-mode))

;;------------------------------------------------------------------------------
;; CMake
(use-package cmake-mode
  :ensure t
  :mode (("\\.cmake\\'" . cmake-mode)
         ("^CMakeLists.txt$" . cmake-mode)))

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

;;------------------------------------------------------------------------------
;; Cucumber/Gherkin
(use-package feature-mode
  :ensure t
  :mode ("\\.feature$" . feature-mode))

;;------------------------------------------------------------------------------
;; Yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" . yaml-mode))

;;------------------------------------------------------------------------------
;; JSON
(use-package json-mode
  :ensure t
  :mode ("\\.json$" . json-mode))

;;------------------------------------------------------------------------------
;; Dirvish
(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-mode-line-height 10)
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  :bind
  (("C-x j" . dirvish-dwim)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   ("<left>" . dired-up-directory)
   ("<right>" . dired-find-file)
   ("?" . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a" . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f" . dirvish-file-info-menu)    ; [f]ile info
   ("o" . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s" . dirvish-quicksort)         ; [s]ort flie list
   ("r" . dirvish-history-jump)      ; [r]ecent visited
   ("l" . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v" . dirvish-vc-menu)           ; [v]ersion control commands
   ("*" . dirvish-mark-menu)
   ("y" . dirvish-yank-menu)
   ("N" . dirvish-narrow)
   ("^" . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

;;------------------------------------------------------------------------------
;; AsciiDoc
(use-package adoc-mode
  :ensure t
  :mode ("\\.adoc$" . adoc-mode))

;;------------------------------------------------------------------------------
;; Graphviz dotfiles
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 2)
  :mode ("\\.dot$" . graphviz-dot-mode))

;;------------------------------------------------------------------------------
;; eat terminal emulator
(use-package eat
  :ensure t)

;;------------------------------------------------------------------------------
;; Mermaid
(use-package mermaid-mode
  :ensure t
  :config
  (setq mermaid-output-format ".png"
        mermaid-flags (concat "-p " (expand-file-name "~/.puppeteerrc.json")))
  :mode ("\\.mmd$" . mermaid-mode))

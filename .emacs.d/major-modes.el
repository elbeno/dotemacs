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
;; Dired
(use-package dired
  :bind
  (("C-x j" . dired-jump))
  (:map dired-mode-map
        ("M-<up>" . jjgr-dired-up-directory)
        ("<return>" . jjgr-dired-find-file)
        ("g" . dired-git-info-mode)
        ("r" . revert-buffer)
        ("b" . jjgr-dired-up-directory)
        ([remap dired-summary] . which-key-show-major-mode))
  :custom
  (dired-find-subdir t "Reuse buffers for opened directories")
  :config
  (setq dired-dwim-target t)
  (defun jjgr-dired-up-directory (&optional other-window)
    "Run Dired on parent directory of current directory, reusing buffer."
    (interactive "P")
    (let* ((dir (dired-current-directory))
           (orig (current-buffer))
           (up (file-name-directory (directory-file-name dir))))
      (or (dired-goto-file (directory-file-name dir))
          ;; Only try dired-goto-subdir if buffer has more than one dir.
          (and (cdr dired-subdir-alist)
               (dired-goto-subdir up))
          (progn
            (kill-buffer orig)
            (dired up)
            (dired-goto-file dir)))))
  (defun jjgr-dired-find-file (&optional prefix)
    "Open file with either operating system defaults or within Emacs."
    (interactive "P")
    (if prefix
        (org-open-file (dired-get-file-for-visit) 'system)
      (dired-find-file)))
  (defun my/dired-config ()
    (diredfl-mode)
    (hl-line-mode)
    (set-face-background hl-line-face "gray13"))
  :hook (dired-mode . my/dired-config))

(use-package diredfl
  :ensure t
  :defer)

(use-package dired-git-info
  :ensure t
  :after dired-mode)

;;------------------------------------------------------------------------------
;; Dirvish
(use-package dirvish
  :ensure t
  :after dired-mode
  :config
  (dirvish-override-dired-mode))

;;------------------------------------------------------------------------------
;; AsciiDoc
(use-package adoc-mode
  :ensure t)

;;------------------------------------------------------------------------------
;; Graphviz dotfiles
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 2))

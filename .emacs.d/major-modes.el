;;------------------------------------------------------------------------------
;; markdown mode
(use-package markdown-mode
  :ensure t)

;;------------------------------------------------------------------------------
;; vlfi
(use-package vlf
  :ensure t)

;; highlight line in dired
(add-hook 'dired-mode-hook 'hl-line-mode)

;; Shells
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "/bin/ls")

;;------------------------------------------------------------------------------
;; CMake
(use-package cmake-mode
  :ensure t)

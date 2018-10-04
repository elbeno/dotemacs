;;------------------------------------------------------------------------------
;; markdown mode
(use-package markdown-mode
  :ensure t)

(use-package markdown-mode+
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

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

;;------------------------------------------------------------------------------
;; Cucumber/Gherkin
(use-package feature-mode
  :ensure t)

;;------------------------------------------------------------------------------
;; Lua
(use-package lua-mode
  :ensure t
  :init
  (setq lua-indent-level 4))

;;------------------------------------------------------------------------------
;; Yaml
(use-package yaml-mode
  :ensure t)

;;------------------------------------------------------------------------------
;; Javascript
(use-package js2-mode
  :ensure t
  :init
  (setq js-indent-level 4
        js2-basic-offset 4)
  :bind (:map js2-mode-map
              ("M-<up>" . js2-prev-error)
              ("M-<down>" . js2-next-error)))

(use-package json-mode
  :ensure t)

;;------------------------------------------------------------------------------
;; Protobufs
(use-package protobuf-mode
  :ensure t
  :init (require 'cl))

;;------------------------------------------------------------------------------
;; Python
(use-package elpy
  :ensure t
  :init
  (setq python-indent-offset 4)
  :bind (:map elpy-mode-map
              ("M-k" . elpy-check))
  :hook (python-mode . elpy-mode))

;;------------------------------------------------------------------------------
;; PDFs
(when (display-graphic-p)
  (use-package pdf-tools
    :ensure t
    :init
    (pdf-tools-install)))

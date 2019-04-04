;;------------------------------------------------------------------------------
;; markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.bs\\'" . markdown-mode))

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
  :ensure t
  :hook (cmake-mode . company-mode))

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

;; cmake-format-on-save
(defcustom my-cmake-format-enabled t
  "If t, run cmake-format on cmake buffers upon saving."
  :group 'cmake-format
  :type 'boolean
  :safe 'booleanp)

(autoload 'cmake-format-buffer "cmake-format"
  "Format the buffer with cmake-format." t)

(defun my-cmake-format-before-save ()
  (interactive)
  (if my-cmake-format-enabled
      (when (eq major-mode 'cmake-mode) (cmake-format-buffer))
    (message "my-cmake-format-enabled is false")))
(add-hook 'before-save-hook 'my-cmake-format-before-save)

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
;; Elixir
(use-package alchemist
  :ensure t)

(use-package elixir-mode
  :ensure t
  :hook (elixir-mode . company-mode))

;;------------------------------------------------------------------------------
;; PDFs
(when (display-graphic-p)
  (use-package pdf-tools
    :ensure t
    :init
    (pdf-tools-install)))

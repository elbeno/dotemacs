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
  (when my-cmake-format-enabled
    (cmake-format-buffer)))
(add-hook 'cmake-mode-hook
          (lambda () (add-hook 'before-save-hook 'my-cmake-format-before-save nil t)))

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
(when (eq system-type 'gnu/linux)
  (when (display-graphic-p)
    (use-package pdf-tools
      :ensure t
      :init
      (pdf-tools-install))))

;;------------------------------------------------------------------------------
;; Dired
(use-package dired
  :defer t
  :bind
  (:map dired-mode-map
        ("M-<up>" . jjgr-dired-up-directory)
        ("\r" . jjgr-dired-find-file))
  :custom
  (dired-find-subdir t "Reuse buffers for opened directories")
  :config
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
  )

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode 1))

(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; highlight line in dired
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'hl-line-mode-hook (lambda () (set-face-background hl-line-face "gray13")))

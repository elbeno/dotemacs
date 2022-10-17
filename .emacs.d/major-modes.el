;;------------------------------------------------------------------------------
;; company
(use-package company
  :ensure t
  :hook prog-mode)

(when (display-graphic-p)
  (use-package company-box
    :ensure t
    :hook company-mode))

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
(defcustom my-cmake-format-enabled t
  "If t, run cmake-format on cmake buffers upon saving."
  :group 'cmake-format
  :type 'boolean
  :safe 'booleanp)

(autoload 'cmake-format-buffer "cmake-format"
  "Format the buffer with cmake-format." t)

(use-package cmake-mode
  :ensure t
  :config
  (defun my/config-cmake-format ()
    (when my-cmake-format-enabled
      (add-hook 'before-save-hook 'cmake-format-buffer nil t))
    (bind-keys :map cmake-mode-map
               ("C-c f" . cmake-format-buffer))
    (company-mode))
  :mode (("\\.cmake\\'" . cmake-mode)
         ("^CMakeLists.txt$" . cmake-mode))
  :hook (cmake-mode . my/config-cmake-format))

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

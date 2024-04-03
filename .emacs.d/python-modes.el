;;------------------------------------------------------------------------------
;; toggle strings between f-strings and otherwise
(defun toggle-string-to-fstring ()
  "Toggle between string and fstring at point"
  (interactive)
  (when (nth 3 (syntax-ppss))
    (save-excursion
      (goto-char (nth 8 (syntax-ppss)))
      (if (eq (char-before) ?f)
          (delete-char -1)
        (insert "f")))))

;;------------------------------------------------------------------------------
;; manage python imports
(unless (> emacs-major-version 28)
(use-package py-isort
  :ensure t
  :commands (py-isort-before-save)))

;;------------------------------------------------------------------------------
;; elpy
(unless (> emacs-major-version 28)
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (defun my/elpy-mode ()
    (add-hook 'before-save-hook 'py-isort-before-save nil t)
    (add-hook 'before-save-hook 'elpy-black-fix-code nil t)
    (setq python-shell-interpreter "python3"
          python-indent-offset 4
          elpy-rpc-python-command "python3"
          elpy-rpc-timeout 10)
    (setq flycheck-python-flake8-executable "python3"
          flycheck-python-pycompile-executable "python3"
          flycheck-python-pylint-executable "python3")
    (flycheck-mode)
    (flycheck-select-checker 'python-flake8)
    (bind-keys :map flycheck-mode-map
               ("M-<down>" . flycheck-next-error)
               ("M-<up>" . flycheck-previous-error))
    (elpy-mode))
  :bind (:map elpy-mode-map
              ("M-k" . elpy-check)
              ("C-c f" . elpy-black-fix-code)
              ("C-c s" . toggle-string-to-fstring)
              ("M-<up>" . nil)
              ("M-<down>" . nil))
  :hook ((python-mode . my/elpy-mode)
         (python-ts-mode . my/elpy-mode))))

;;------------------------------------------------------------------------------
;; eglot
(when (> emacs-major-version 28)
(use-package eglot
  :ensure t
  :config
  (defun my/eglot-organize-imports () (interactive)
	 (eglot-code-actions nil nil "source.organizeImports" t))
  (defun my/python-eglot-config ()
    (eglot-ensure)
    (add-hook 'before-save-hook 'eglot-format-buffer nil t)
    (add-hook 'before-save-hook 'my/eglot-organize-imports nil t))
  (fset #'jsonrpc--log-event #'ignore)
  :bind (:map eglot-mode-map
              ("C-c f" . eglot-format)
              ("C-c i" . my/eglot-organize-imports)
              ("C-c r" . eglot-rename)
              ("C-c s" . toggle-string-to-fstring)
              ("C-<return>" . eglot-code-actions)
              ("M-<up>" . flymake-goto-prev-error)
              ("M-<down>" . flymake-goto-next-error))
  :hook ((python-mode . my/python-eglot-config)
         (python-ts-mode . my/python-eglot-config))))

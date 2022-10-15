;;------------------------------------------------------------------------------
;; manage python imports
(use-package py-isort
  :ensure t
  :commands (py-isort-before-save))

;;------------------------------------------------------------------------------
;; elpy
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
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
  (defun toggle-string-to-fstring ()
    "Toggle between string and fstring at point"
    (interactive)
    (when (nth 3 (syntax-ppss))
      (save-excursion
        (goto-char (nth 8 (syntax-ppss)))
        (if (eq (char-before) ?f)
            (delete-char -1)
          (insert "f")))))
  :bind (:map elpy-mode-map
              ("M-k" . elpy-check)
              ("C-c f" . elpy-black-fix-code)
              ("C-c s" . toggle-string-to-fstring)
              ("M-<up>" . nil)
              ("M-<down>" . nil))
  :hook (python-mode . my/elpy-mode))

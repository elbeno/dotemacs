;; -*- lexical-binding: t; -*-
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
;; eglot
(use-package eglot
  :ensure t
  :config
  (defun my/eglot-organize-imports () (interactive)
	 (eglot-code-actions (point-min) (point-max) "source.organizeImports" t))
  (defun my/python-eglot-config ()
    (eglot-ensure)
    (add-hook 'before-save-hook 'eglot-format-buffer nil t)
    (add-hook 'before-save-hook 'my/eglot-organize-imports nil t))
  (fset #'jsonrpc--log-event #'ignore)
  :bind (:map eglot-mode-map
              ("C-c F" . eglot-format)
              ("C-c i" . my/eglot-organize-imports)
              ("C-c r" . eglot-rename)
              ("C-c s" . toggle-string-to-fstring)
              ("C-<return>" . eglot-code-actions)
              ("M-<up>" . flymake-goto-prev-error)
              ("M-<down>" . flymake-goto-next-error))
  :hook ((python-mode . my/python-eglot-config)
         (python-ts-mode . my/python-eglot-config)))

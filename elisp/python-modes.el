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
  :commands (eglot eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 0)
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0)
  (eglot-inlay-hints-mode -1)
  :config
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("ty" "server")))
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l d" . eldoc)
              ("C-c l f" . eglot-format)
              ("C-c l h" . eglot-inlay-hints-mode)
              ("C-c l i" . eglot-code-action-organize-imports)
              ("C-c l l" . org-store-link)
              ("C-c l q" . eglot-shutdown-all)
              ("C-c l r" . eglot-rename)
              ("M-<up>" . flymake-goto-prev-error)
              ("M-<down>" . flymake-goto-next-error)
              ("C-<return>" . eglot-code-action-quickfix))
  :hook ((python-ts-mode . eglot-ensure)))

;; inlay hints off by default
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

;;------------------------------------------------------------------------------
;; Use ruff with apheleia
(with-eval-after-load 'apheleia
  (when (executable-find "ruff")
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))))

;; Use ruff with flymake
(when (executable-find "ruff")
  (use-package flymake-ruff
    :ensure t
    :hook (eglot-managed-mode . flymake-ruff-load)))

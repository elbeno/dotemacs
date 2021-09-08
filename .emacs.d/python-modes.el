;;------------------------------------------------------------------------------
;; python interpreter
(setq python-shell-interpreter "python3")

;;------------------------------------------------------------------------------
;; flycheck
(setq flycheck-python-flake8-executable "python3"
      flycheck-python-pycompile-executable "python3"
      flycheck-python-pylint-executable "python3")

(add-hook 'python-mode-hook (lambda ()
                              (flycheck-select-checker 'python-flake8)))

;;------------------------------------------------------------------------------
;; make a string into an fstring or vice versa
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
(use-package py-isort
  :ensure t)

(add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      'py-isort-before-save nil t)))

;;------------------------------------------------------------------------------
;; elpy
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (setq python-indent-offset 4
        elpy-rpc-python-command "python3"
        elpy-rpc-timeout 10)
  :bind (:map elpy-mode-map
              ("M-k" . elpy-check)
              ("C-c f" . elpy-black-fix-code)
              ("C-c s" . toggle-string-to-fstring)
              ("M-<up>" . nil)
              ("M-<down>" . nil)))

(add-hook 'elpy-mode-hook (lambda ()
                            (add-hook 'before-save-hook
                                      'elpy-black-fix-code nil t)))

;;------------------------------------------------------------------------------
;; jupyter
(defun my-ein-keybindings ()
  (bind-keys :map ein:notebook-mode-map
             ("C-c M-l" . ein:worksheet-clear-all-output)))

(use-package ein
  :ensure t
  :config
  (setq ein:notebook-autosave-frequency 0)
  :hook (ein:notebook-mode . my-ein-keybindings))

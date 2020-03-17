;;------------------------------------------------------------------------------
;; python interpreter
(setq python-shell-interpreter "python3")

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

(add-hook 'python-mode-hook
          (lambda () (bind-keys :map python-mode-map
                           ("C-c f" . toggle-string-to-fstring))))

;;------------------------------------------------------------------------------
;; elpy
(use-package elpy
  :ensure t
  :init
  (setq python-indent-offset 4
        elpy-rpc-python-command "python3")
  :bind (:map elpy-mode-map
              ("M-k" . elpy-check))
  :hook (python-mode . elpy-mode))

;;------------------------------------------------------------------------------
;; pyautopep8
(use-package py-autopep8
  :ensure t
  :config
  (setq py-autopep8-options (list "--global-config=~/.config/flake8")))

;;------------------------------------------------------------------------------
;; manage python imports
(use-package pyimport
  :ensure t)

(use-package importmagic
  :ensure t)

(use-package pyimpsort
  :ensure t)

;;------------------------------------------------------------------------------
;; on save: fix imports, sort them, remove unused, then pep8
(defun my-python-before-save-hook ()
  (save-excursion
    (importmagic-fix-imports)
    (pyimpsort-remove-unused)
    (pyimpsort-buffer)
    (py-autopep8-buffer)))

(add-hook 'elpy-mode-hook
          (lambda () (importmagic-mode)
            (add-hook 'before-save-hook 'my-python-before-save-hook t 'local)))

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

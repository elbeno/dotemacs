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
                           ("C-c s" . toggle-string-to-fstring))))

;;------------------------------------------------------------------------------
;; elpy
(use-package elpy
  :ensure t
  :init
  (setq python-indent-offset 4
        elpy-rpc-python-command "python3"
        elpy-rpc-timeout 10)
  :bind (:map elpy-mode-map
              ("M-k" . elpy-check)
              ("C-c f" . elpy-black-fix-code))
  :hook (python-mode . elpy-mode))

;;------------------------------------------------------------------------------
;; manage python imports
(use-package pyimpsort
  :ensure t)

;;------------------------------------------------------------------------------
;; on save: remove unused imports, sort them, then format with black
(defun my-python-before-save-hook ()
  (save-excursion
    (elpy-black-fix-code)
    (pyimpsort-buffer)))

(add-hook 'elpy-mode-hook
          (lambda () (add-hook 'before-save-hook 'my-python-before-save-hook t 'local)
            (define-key elpy-mode-map (kbd "<M-up>") nil)
            (define-key elpy-mode-map (kbd "<M-down>") nil)))

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

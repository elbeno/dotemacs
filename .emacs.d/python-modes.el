;;------------------------------------------------------------------------------
;; python interpreter
(setq python-shell-interpreter "python3")

;;------------------------------------------------------------------------------
;; elpy
(use-package elpy
  :ensure t
  :init
  (setq python-indent-offset 4
        elpy-rpc-python-command "python3")
  :bind (:map elpy-mode-map
              ("M-k" . elpy-check)
              ("M-<down>" . next-error)
              ("M-<up>" . previous-error))
  :hook (python-mode . elpy-mode))

;;------------------------------------------------------------------------------
;; pyautopep8
(use-package py-autopep8
  :ensure t
  :config
  (setq py-autopep8-options (list "--global-config=~/.config/flake8"))
  :hook (elpy-mode . py-autopep8-enable-on-save))



;;------------------------------------------------------------------------------
;; manage python imports
(use-package pyimport
  :ensure t
  :hook
  (elpy-mode . (lambda () (add-hook 'before-save-hook 'pyimport-remove-unused))))

(use-package importmagic
  :ensure t
  :hook
  (elpy-mode . (lambda ()
                 (importmagic-mode)
                 (add-hook 'before-save-hook 'importmagic-fix-imports))))

(use-package pyimpsort
  :ensure t
  :hook
  (elpy-mode . (lambda () (add-hook 'before-save-hook 'pyimpsort-buffer))))


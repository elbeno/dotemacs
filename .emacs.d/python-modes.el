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
  (setq py-autopep8-options (list "--global-config=~/.config/flake8")))

;;------------------------------------------------------------------------------
;; manage python imports
(use-package pyimport
  :ensure t)

(use-package importmagic
  :ensure t)

(use-package pyimpsort
  :ensure t)

;; advise pyimpsort-buffer: play nicely with fci-mode
(defun my-pyimpsort-buffer (func &rest args)
  (save-excursion
    (turn-off-fci-mode)
    (apply func args)
    (turn-on-fci-mode)))
(advice-add #'pyimpsort-buffer :around #'my-pyimpsort-buffer)

;;------------------------------------------------------------------------------
;; on save: fix imports, sort them, remove unused, then pep8
(add-hook 'elpy-mode-hook
          (lambda () (importmagic-mode)
            (add-hook 'before-save-hook 'importmagic-fix-imports t t)
            (add-hook 'before-save-hook 'pyimpsort-buffer t t)
            (add-hook 'before-save-hook 'pyimport-remove-unused t t)
            (add-hook 'before-save-hook 'py-autopep8-buffer t t)))

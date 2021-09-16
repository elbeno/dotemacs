;;------------------------------------------------------------------------------
;; Git interactions
;; modes
(use-package gitconfig-mode :ensure t)
(use-package gitignore-mode :ensure t)
(use-package git-commit :ensure t)

;; magit
(use-package magit
  :ensure t
  :bind
  (("C-c g" . magit-status)
   ("C-c C-b" . magit-blame))
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"))

;; git time machine
(use-package git-timemachine
  :ensure t
  :bind
  (("C-c h" . git-timemachine-toggle)))

;; git messenger
(use-package git-messenger
  :ensure t
  :bind
  (("C-c b" . git-messenger:popup-message))
  :config
  (bind-key "m" 'git-messenger:copy-message git-messenger-map)
  ;; Enable magit-commit-mode after typing 's', 'S', 'd'
  (add-hook 'git-messenger:popup-buffer-hook #'magit-commit-mode))

;; on-the-fly diff highlighting
(use-package diff-hl
  :ensure t
  :config
  (setq diff-hl-side 'right)
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;; prevent annoying reverts during conflict editing
(setq auto-revert-vc-info nil)

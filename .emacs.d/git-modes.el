;;------------------------------------------------------------------------------
;; Git interactions

;; magit
(use-package magit
  :ensure t
  :bind
  (("C-c g" . magit-status)
   ("C-c B" . magit-blame)))

(use-package git-commit
  :ensure t
  :after magit)

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
  (diff-hl-flydiff-mode)
  :hook
  (prog-mode . diff-hl-mode))

;; prevent annoying reverts during conflict editing
(setq auto-revert-vc-info nil)

;; get url links
(use-package git-link
  :ensure t
  :bind
  (("C-c u" . git-link)))

;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Fix terminal keys
(if (getenv "TMUX")
    (use-package term-keys
      :ensure t
      :config
      (term-keys-mode t)
      :diminish term-keys-mode)
  (use-package kkp
    :ensure t
    :config
    (global-kkp-mode +1)))

;;------------------------------------------------------------------------------
;; Use system clipboard
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

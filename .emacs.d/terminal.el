;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Fix terminal keys
(use-package term-keys
  :ensure t
  :config
  (term-keys-mode t)
  :diminish term-keys-mode)

;;------------------------------------------------------------------------------
;; Use system clipboard
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

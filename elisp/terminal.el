;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Fix terminal keys
(use-package term-keys
  :ensure t
  :init (my/vc-install "CyberShadow/term-keys")
  :config
  (term-keys-mode t)
  :diminish term-keys-mode)

;;------------------------------------------------------------------------------
;; Use system clipboard
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;;------------------------------------------------------------------------------
;; Tmux control
(use-package tmux-control
  :init (my/vc-install "csheaff/tmux-control")
  :custom
  ;; Connection defaults for `M-x tmux-control-connect' — these are examples;
  ;; set them to your own host / socket / session.
  (tmux-control-default-host nil)          ; an SSH host alias, or nil for local
  (tmux-control-default-socket-name "elbeno")
  (tmux-control-default-session "emacs"))

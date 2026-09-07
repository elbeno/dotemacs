;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Fix terminal keys
(use-package term-keys
  :ensure t
  :vc (:url "https://github.com/CyberShadow/term-keys.git"
            :vc-backend Git
            :rev :newest)
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
  :vc (:url "https://github.com/csheaff/tmux-control.git"
            :vc-backend Git
            :rev :newest)
  :custom
  ;; Connection defaults for `M-x tmux-control-connect' — these are examples;
  ;; set them to your own host / socket / session.
  (tmux-control-default-host nil)          ; an SSH host alias, or nil for local
  (tmux-control-default-socket-name "elbeno")
  (tmux-control-default-session "emacs"))

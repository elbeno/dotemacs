;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; smooth scrolling and resizing
(setq pixel-scroll-mode t
      frame-resize-pixelwise t)

;;------------------------------------------------------------------------------
;; font: Berkeley Mono
(custom-set-faces
 '(fixed-pitch ((t (:family "BerkeleyMono Nerd Font")))))
(custom-set-faces
 '(org-document-title ((t (:family "BerkeleyMono Nerd Font" :height 1.0)))))

(defun my/new-frame-setup ()
  (set-frame-font "BerkeleyMono Nerd Font-11"))

(unless (daemonp) (my/new-frame-setup))
(add-hook 'server-after-make-frame-hook 'my/new-frame-setup)

;;------------------------------------------------------------------------------
;; All the icons!
(use-package all-the-icons
  :ensure t
  :config
  (unless (file-exists-p
           (concat (getenv "HOME") "/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts t)))

;;------------------------------------------------------------------------------
;; Display file info
(use-package file-info
  :ensure t
  :bind ("C-c <f3>" . 'file-info-show)
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-frame-center
                                               :internal-border-width 2
                                               :internal-border-color "#61AFEF"
                                               :left-fringe 16
                                               :right-fringe 16)))

;;------------------------------------------------------------------------------
;; Ultrascroll
(use-package ultra-scroll
  :ensure t
  :init
  (setq scroll-conservatively 3
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

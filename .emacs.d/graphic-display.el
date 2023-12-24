;;------------------------------------------------------------------------------
;; smooth scrolling
(setq pixel-scroll-mode t)

;;------------------------------------------------------------------------------
;; font: Inconsolata
(set-frame-font "Berkeley Mono-11")

(custom-set-faces
 '(fixed-pitch ((t (:family "Berkeley Mono")))))
(custom-set-faces
 '(org-document-title ((t (:family "Berkeley Mono" :height 1.0)))))

;;------------------------------------------------------------------------------
(use-package dash
  :ensure t)

;;------------------------------------------------------------------------------
;; Sizing/docking
(setq frame-resize-pixelwise t)

(defun monitor-width (monitor)
  (nth 3 (assq 'geometry monitor)))

(defun frame-max-height (&optional frame)
  (interactive)
  (set-frame-parameter frame 'fullscreen 'fullheight))

(defun dock-frame-left (&optional frame monitor)
  (interactive)
  (setq frame (or frame (selected-frame)))
  (setq monitor (or monitor (frame-monitor-attributes frame)))
  (let* ((monitor-list (-take-while
                        (lambda (x) (not (equal monitor x)))
                        (display-monitor-attributes-list)))
         (widths (mapcar #'monitor-width monitor-list))
         (x (apply '+ widths)))
    (set-frame-parameter frame 'left x)))

(defun dock-frame-right (&optional frame monitor)
  (interactive)
  (setq frame (or frame (selected-frame)))
  (setq monitor (or monitor (frame-monitor-attributes frame)))
  (let* ((monitor-list (-take-while
                        (lambda (x) (not (equal monitor x)))
                        (display-monitor-attributes-list)))
         (widths (mapcar #'monitor-width monitor-list))
         (x (+ (apply '+ widths) (monitor-width monitor))))
    (set-frame-parameter frame 'left (- x (frame-pixel-width frame)))))

(defun size-frame-default ()
  (set-frame-parameter nil 'width column-wrap-hard)
  (frame-max-height))

(bind-key "C-S-<f11>" 'frame-max-height)
(bind-key "C-<f11>" 'dock-frame-left)
(bind-key "C-<f12>" 'dock-frame-right)

;;------------------------------------------------------------------------------
;; Frame opacity
(defun sanityinc/adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(bind-key "M-C-8" (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(bind-key "M-C-9" (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(bind-key "M-C-0" (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;;------------------------------------------------------------------------------
;; All the icons!
(use-package all-the-icons
  :ensure t
  :config
  (unless (file-exists-p
           (concat (getenv "HOME") "/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :ensure t
  :hook dired-mode
  :after all-the-icons)

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

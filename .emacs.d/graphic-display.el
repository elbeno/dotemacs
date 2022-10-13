;;------------------------------------------------------------------------------
;; font: Inconsolata
(setq my-font-family "Inconsolata")
(setq my-font-height 100)
(setq my-font-weight 'normal)
(setq my-font-width 'normal)

(when (eq system-type 'darwin)
  (setq my-font-height 120))

(set-face-attribute 'default nil
                    :family my-font-family
                    :height my-font-height
                    :weight my-font-weight
                    :width my-font-width)

(set-fontset-font "fontset-default"
                  '(#x0100 . #xffff)
                  (font-spec :family "DejaVu Sans Mono"
                             :height my-font-height
                             :weight 'normal
                             :width 'normal))

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

;;------------------------------------------------------------------------------
;; Prettier modeline
(setq my/read-only-indicator " 🔑 ")
(setq my/modified-indicator " ⭐ ")

(setq-default mode-line-format (list
  '((:eval
     (cond
      (buffer-read-only
       (propertize my/read-only-indicator 'face '(:foreground "red" :weight 'bold)))
      ((buffer-modified-p)
       (propertize my/modified-indicator 'face '(:foreground "yellow")))
      ((not (buffer-modified-p))
       (propertize my/modified-indicator 'face '(:foreground "black"))))))
  '(:eval (propertize (all-the-icons-icon-for-mode major-mode :height (/ all-the-icons-scale-factor 1.4) :v-adjust -0.03)))
  '(:eval
    (format " %s " (my-truncate-buffer-name (buffer-file-name))))
  'mode-line-position
  "["
  'mode-name
  "] "
  '(:eval
    (if vc-mode
        (let* ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
               (face (cond ((string-match "^ -" noback) 'mode-line-vc)
                           ((string-match "^ [:@]" noback) 'mode-line-vc-edit)
                           ((string-match "^ [!\\?]" noback) 'mode-line-vc-modified))))
          (format "[ %s]" (substring noback 2)))))))

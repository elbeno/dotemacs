;;------------------------------------------------------------------------------
;; Prettier modeline
(setq my/read-only-indicator " R ")
(setq my/modified-indicator " * ")

(setq-default mode-line-format (list
  '((:eval
     (cond
      (buffer-read-only
       (propertize my/read-only-indicator 'face '(:foreground "red" :weight 'bold)))
      ((buffer-modified-p)
       (propertize my/modified-indicator 'face '(:foreground "yellow")))
      ((not (buffer-modified-p))
       (propertize my/modified-indicator 'face '(:foreground "black"))))))
  '(:eval
    (format " %s " (my-truncate-buffer-name (or (buffer-file-name) (buffer-name)))))
  'mode-line-position
  "[" 'mode-name "] "))

;;------------------------------------------------------------------------------
;; Fix terminal keys
(use-package term-keys
  :ensure t
  :config
  (term-keys-mode t)
  :diminish term-keys-mode)

;;------------------------------------------------------------------------------
;; Use system clipboard
(use-package xclip
  :ensure t
  :init (xclip-mode 1))

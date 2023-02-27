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
          (format "[%s]" (substring noback 2)))))))


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

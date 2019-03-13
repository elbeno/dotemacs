(bind-key "C-z" 'undo)
(bind-key "C-o" 'goto-line)
(bind-key "M-r" 'replace-string)
(bind-key "M-k" 'compile)
(bind-key "M-SPC" 'cycle-spacing)

;; old-style searching
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

;; Action of home key
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(bind-key "<home>" 'beginning-of-line-or-indentation)

;; Turn off insert
(defun do-nothing () (interactive))
(bind-key "<insert>" 'do-nothing)
(bind-key "<insertchar>" 'do-nothing)

;; Kill-ring menu
(defun popup-kill-ring-menu ()
  "Show the kill ring in a popup menu."
  (interactive)
  (popup-menu 'yank-menu))
(bind-key "C-c y" 'popup-kill-ring-menu)

;; Cycle buffers/windows with F5-F8
(bind-key "<f5>" 'next-multiframe-window)
(bind-key "<f6>" 'previous-multiframe-window)
(bind-key "<f7>" 'previous-buffer)
(bind-key "<f8>" 'next-buffer)

;; Moving windows
(bind-key "C-c <left>"  'windmove-left)
(bind-key "C-c <right>" 'windmove-right)
(bind-key "C-c <up>"    'windmove-up)
(bind-key "C-c <down>"  'windmove-down)

;; Highlight symbols
(bind-key "<f3>" 'highlight-symbol-at-point)
(bind-key "S-<f3>" 'hi-lock-mode)

;; Use hippie-expand instead of dabbrev-expand
(bind-key "M-/" 'hippie-expand)

;; Use kill-this-buffer
(defun bjm/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(bind-key "C-x k" 'bjm/kill-this-buffer)

;; quick-calc on C-=
(bind-key "C-=" 'quick-calc)

;; rotate-text on C-c /
(bind-key "C-c /" 'rotate-text)
(bind-key "C-c C-/" 'rotate-text-backward)

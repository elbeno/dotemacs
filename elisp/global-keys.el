;; -*- lexical-binding: t; -*-
;; (bind-key "C-o" 'goto-line) ;; replaced by consult
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
  (when this-command-keys-shift-translated
      (unless mark-active (push-mark nil t t)))
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line))
  (when this-command-keys-shift-translated
    (setq transient-mark-mode (cons 'only transient-mark-mode))))
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

;; Use hippie-expand instead of dabbrev-expand
(bind-key "M-/" 'hippie-expand)

;; Don't ask for which buffer to kill
(bind-key "C-x k" 'kill-current-buffer)

;; quick-calc on C-=
(bind-key "C-=" 'quick-calc)

;; comments
(defun my/comment-dwim ()
  "Comment region if active, else comment line.

This avoids the excess region commenting of `comment-line' while also avoiding the weird single-line
behavior of `comment-dwim'."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (call-interactively #'comment-or-uncomment-region)
      (call-interactively #'comment-line))))
(bind-key "M-;" 'my/comment-dwim)

;; wrap selection in delimiters
(defvar insert-pair-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] #'insert-pair)
    map))
(bind-key "C-(" insert-pair-map)

;; C-a my attention key in tmux, so remap emacs' C-a to C-b
;; C-b is normally bound to backward-char
(bind-key "C-b" 'move-beginning-of-line)

(when (> emacs-major-version 28)
  (bind-key "C-c d" 'duplicate-dwim))

;; prevent accidental suspend
(global-unset-key (kbd "C-x C-z"))

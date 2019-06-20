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

;; xterm with the resource ?.VT100.modifyOtherKeys: 1
;; GNU Emacs >=24.4 sets xterm in this mode and define
;; some of the escape sequences but not all of them.
(defun character-apply-modifiers (c &rest modifiers)
  "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (meta control shift).
Return an event vector."
  (if (memq 'control modifiers) (setq c (if (or (and (<= ?@ c) (<= c ?_))
                                                (and (<= ?a c) (<= c ?z)))
                                            (logand c ?\x1f)
                                          (logior (lsh 1 26) c))))
  (if (memq 'meta modifiers) (setq c (logior (lsh 1 27) c)))
  (if (memq 'shift modifiers) (setq c (logior (lsh 1 25) c)))
  (vector c))

(defun my-eval-after-load-xterm ()
  (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
    (let ((c 32))
      (while (<= c 126)
        (mapc (lambda (x)
                (define-key xterm-function-map (format (car x) c)
                  (apply 'character-apply-modifiers c (cdr x))))
              '(;; with ?.VT100.formatOtherKeys: 0
                ("\e\[27;3;%d~" meta)
                ("\e\[27;5;%d~" control)
                ("\e\[27;6;%d~" control shift)
                ("\e\[27;7;%d~" control meta)
                ("\e\[27;8;%d~" control meta shift)
                ;; with ?.VT100.formatOtherKeys: 1
                ("\e\[%d;3u" meta)
                ("\e\[%d;5u" control)
                ("\e\[%d;6u" control shift)
                ("\e\[%d;7u" control meta)
                ("\e\[%d;8u" control meta shift)))
        (setq c (1+ c))))))

(eval-after-load "xterm" '(my-eval-after-load-xterm))

;; Make navigation keys behave on Mac like they do on Windows/Linux
(when (eq system-type 'darwin)
  (bind-key "<end>" 'move-end-of-line))

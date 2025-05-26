;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Disable themes completely before applying a new one
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable all theme effects before enabling new ones."
  (mapc #'disable-theme custom-enabled-themes))

;;------------------------------------------------------------------------------
;; Nice theme
(defun load-theme-cyberpunk ()
  (interactive)
  (load-theme 'cyberpunk t))

(use-package cyberpunk-theme
  :ensure t
  :bind (("C-c t c" . load-theme-cyberpunk)))
(load-theme-cyberpunk)

;;------------------------------------------------------------------------------
;; Highlight FIXME/TODO/etc
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

;;------------------------------------------------------------------------------
;; Special types of comments
(defface font-lock-comment-strike
  '((t (:strike-through t)))
  "For strike-through comments")

(defface font-lock-comment-important
  '((t (:foreground "#00ff00")))
  "For important")

(defface font-lock-comment-todo
  '((t (:foreground "#ff0000")))
  "For todo comments")

(defun add-custom-keyw()
  "adds a few special keywords"
  (font-lock-add-keywords
   nil
   '(("\\s<+x[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-strike prepend)
     ("\\s<+t[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-todo prepend)
     ("\\s<+i[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-important prepend))))

(add-hook 'prog-mode-hook #'add-custom-keyw)

(defun add-custom-keyw-cpp()
  "adds a few special keywords"
  (font-lock-add-keywords
   nil
   '(("//+x[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-strike prepend)
     ("//+t[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-todo prepend)
     ("//+i[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-important prepend))))

(add-hook 'c++-mode-hook #'add-custom-keyw-cpp)

;;------------------------------------------------------------------------------
;; Diff mode colors
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

;;------------------------------------------------------------------------------
;; modus themes
(defun load-theme-modus-operandi ()
  (interactive)
  (load-theme 'modus-operandi t))
(defun load-theme-modus-vivendi ()
  (interactive)
  (load-theme 'modus-vivendi t))

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (fringe bg-inactive)
          (bg-region bg-active)
          (fg-region unspecified))
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-prompts '(bold intense)
        modus-themes-completions '((matches . (extrabold))
                                   (selection . (semibold text-also)))
        modus-themes-org-blocks 'tinted-background)
  :bind (("C-c t l" . load-theme-modus-operandi)
         ("C-c t d" . load-theme-modus-vivendi)
         ("C-c t o" . load-theme-modus-operandi)
         ("C-c t v" . load-theme-modus-vivendi)
         ("C-c t t" . modus-themes-toggle)))

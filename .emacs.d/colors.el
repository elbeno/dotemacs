;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Nice themes
(defun load-theme-cyberpunk ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'cyberpunk t))

(use-package cyberpunk-theme
  :ensure t
  :bind (("C-c t c" . load-theme-cyberpunk)))

(defun load-theme-tomorrow-night-deepblue ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'tomorrow-night-deepblue t))

(use-package tomorrow-night-deepblue-theme
  :ensure t
  :bind (("C-c t b" . load-theme-tomorrow-night-deepblue)))

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
  (require 'modus-themes)
  (modus-themes-load-theme 'modus-operandi))
(defun load-theme-modus-vivendi ()
  (interactive)
  (require 'modus-themes)
  (modus-themes-load-theme 'modus-vivendi))

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
         ("C-c t m" . modus-themes-select)))

;;------------------------------------------------------------------------------
;; ef themes
(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-to-toggle '(ef-summer ef-winter))
  :bind (("C-c t e" . ef-themes-select)))

;;------------------------------------------------------------------------------
;; doom themes
(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  :bind (("C-c t D" . my/doom-themes-select)))

;;------------------------------------------------------------------------------
;; doom themes selection, piggybacking on ef-themes selection
(defconst my/doom-themes
  '(doom-1337
    doom-acario-dark
    doom-acario-light
    doom-ayu-dark
    doom-ayu-light
    doom-ayu-mirage
    doom-badger
    doom-challenger-deep
    doom-city-lights
    doom-dark+
    doom-dracula
    doom-earl-grey
    doom-ephemeral
    doom-fairy-floss
    doom-feather-dark
    doom-feather-light
    doom-flatwhite
    doom-gruvbox
    doom-gruvbox-light
    doom-henna
    doom-homage-black
    doom-homage-white
    doom-horizon
    doom-Iosvkem
    doom-ir-black
    doom-lantern
    doom-laserwave
    doom-manegarm
    doom-material
    doom-material-dark
    doom-meltbus
    doom-miramare
    doom-molokai
    doom-monokai-classic
    doom-monokai-machine
    doom-monokai-octagon
    doom-monokai-pro
    doom-monokai-ristretto
    doom-monokai-spectrum
    doom-moonlight
    doom-nord
    doom-nord-aurora
    doom-nord-light
    doom-nova
    doom-oceanic-next
    doom-oksolar-dark
    doom-oksolar-light
    doom-old-hope
    doom-one
    doom-one-light
    doom-opera
    doom-opera-light
    doom-outrun-electric
    doom-palenight
    doom-peacock
    doom-pine
    doom-plain
    doom-plain-dark
    doom-rouge
    doom-shades-of-purple
    doom-snazzy
    doom-solarized-dark
    doom-solarized-dark-high-contrast
    doom-solarized-light
    doom-sourcerer
    doom-spacegrey
    doom-tokyo-night
    doom-tomorrow-day
    doom-tomorrow-night
    doom-vibrant
    doom-wilmersdorf
    doom-xcode
    doom-zenburn)
  "List of doom themes.")

(defun my/doom-themes--enable-themes ()
  "Enable all doom themes."
  (mapc
   (lambda (theme)
     (unless (memq theme custom-known-themes)
       (load-theme theme :no-confirm :no-enable)))
   my/doom-themes))

(defun my/doom-themes--load ()
  "Return all the known doom themes."
  (ef-themes--completion-table 'theme (my/doom-themes--enable-themes)))

(defun my/doom-themes--select-prompt (&optional prompt)
  "Minibuffer prompt for `doom-themes-select'.
With optional PROMPT string, use it.  Else use a generic prompt."
  (require 'ef-themes)
  (let* ((themes (my/doom-themes--load))
         (completion-extra-properties `(:annotation-function ,#'ef-themes--annotate-theme)))
    (intern
     (completing-read
      (or prompt "Select Doom Theme: ")
      themes
      nil t nil
      'my/doom-themes--select-theme-history))))

(defvar my/doom-themes--select-theme-history nil
  "Minibuffer history of `my/doom-themes--select-prompt'.")

(defun my/doom-themes-select (theme)
  "Load a doom THEME using minibuffer completion.
When called from Lisp, THEME is the symbol of a theme."
  (interactive (list (my/doom-themes--select-prompt nil)))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

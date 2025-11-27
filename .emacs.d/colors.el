;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Nice themes
(defun my/load-theme (theme)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t))

(defun load-theme-cyberpunk ()
  (interactive)
  (my/load-theme 'cyberpunk))

(use-package cyberpunk-theme
  :ensure t
  :bind (("C-c t c" . load-theme-cyberpunk)))

(defun load-theme-tomorrow-night-deepblue ()
  (interactive)
  (my/load-theme 'tomorrow-night-deepblue))

(use-package tomorrow-night-deepblue-theme
  :ensure t
  :bind (("C-c t b" . load-theme-tomorrow-night-deepblue)))

(defun load-theme-hercules ()
  (interactive)
  (my/load-theme 'hercules))

(use-package hercules-theme
  :init (my/vc-install "0xcefaedfe/hercules-theme")
  :bind (("C-c t h" . load-theme-hercules)))

(defun load-theme-tron-legacy ()
  (interactive)
  (my/load-theme 'tron-legacy))

(use-package tron-legacy-theme
  :ensure t
  :config
  (setq tron-legacy-theme-vivid-cursor t
        tron-legacy-theme-softer-bg t
        tron-legacy-theme-dark-fg-bright-comments t)
  :bind (("C-c t r" . load-theme-tron-legacy)))

(defun load-theme-poet-dark ()
  (interactive)
  (my/load-theme 'poet-dark))

(use-package poet-theme
  :ensure t
  :bind (("C-c t p" . load-theme-poet-dark)))

(defun load-theme-zenburn ()
  (interactive)
  (my/load-theme 'zenburn))

(use-package zenburn-theme
  :ensure t
  :bind (("C-c t z" . load-theme-zenburn)))

(defun load-theme-moe-dark ()
  (interactive)
  (my/load-theme 'moe-dark))

(use-package moe-theme
  :ensure t
  :bind (("C-c t m" . load-theme-moe-dark)))

(defun load-theme-ample ()
  (interactive)
  (my/load-theme 'ample))

(use-package ample-theme
  :ensure t
  :bind (("C-c t a" . load-theme-ample)))

(defun load-theme-miasma ()
  (interactive)
  (my/load-theme 'miasma))

(use-package miasma-theme
  :init (my/vc-install "daut/miasma-theme.el")
  :bind (("C-c t s" . load-theme-miasma)))

;; default theme
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
  :bind (("C-c t v" . load-theme-modus-vivendi)
         ("C-c t o" . load-theme-modus-operandi)
         ("C-c t M" . modus-themes-select)))

;;------------------------------------------------------------------------------
;; ef themes
(use-package ef-themes
  :ensure t
  :bind (("C-c t E" . ef-themes-select)))

;; themes selection, piggybacking on ef-themes selection
(defun my/themes--select-prompt (themes history &optional prompt)
  "Minibuffer prompt for `themes-select'.
With optional PROMPT string, use it.  Else use a generic prompt."
  (require 'ef-themes)
  (let* ((completion-extra-properties `(:annotation-function ,#'ef-themes--annotate-theme)))
    (intern
     (completing-read
      (or prompt "Select Theme: ")
      themes
      nil t nil
      'my/solarized-themes--select-theme-history))))

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
  ;; (doom-themes-neotree-config)
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

(defvar my/doom-themes--select-theme-history nil
  "Minibuffer history of `my/doom-themes--select-prompt'.")

(defun my/doom-themes--select-prompt ()
  "Minibuffer prompt for `doom-themes-select'.
With optional PROMPT string, use it.  Else use a generic prompt."
  (my/themes--select-prompt my/doom-themes
                            my/doom-themes--select-theme-history
                            "Select Doom Theme: "))

(defun my/doom-themes-select (theme)
  "Load a doom THEME using minibuffer completion.
When called from Lisp, THEME is the symbol of a theme."
  (interactive (list (my/doom-themes--select-prompt)))
  (my/load-theme theme))

;;------------------------------------------------------------------------------
;; solaire mode: distinguish "real" buffers
(use-package solaire-mode
  :ensure t
  :init (solaire-global-mode +1))

;;------------------------------------------------------------------------------
;; kaolin themes
(use-package kaolin-themes
  :ensure t
  :bind (("C-c t K" . my/kaolin-themes-select)))

(defconst my/kaolin-themes
  '(kaolin-dark
    kaolin-light
    kaolin-aurora
    kaolin-blossom
    kaolin-breeze
    kaolin-bubblegum
    kaolin-eclipse
    kaolin-galaxy
    kaolin-mono-dark
    kaolin-mono-light
    kaolin-ocean
    kaolin-shiva
    kaolin-temple
    kaolin-valley-dark
    kaolin-valley-light)
  "List of kaolin themes.")

(defvar my/kaolin-themes--select-theme-history nil
  "Minibuffer history of `my/kaolin-themes--select-prompt'.")

(defun my/kaolin-themes--select-prompt ()
  "Minibuffer prompt for `kaolin-themes-select'.
With optional PROMPT string, use it.  Else use a generic prompt."
  (my/themes--select-prompt my/kaolin-themes
                            my/kaolin-themes--select-theme-history
                            "Select Kaolin Theme: "))

(defun my/kaolin-themes-select (theme)
  "Load a kaolin THEME using minibuffer completion.
When called from Lisp, THEME is the symbol of a theme."
  (interactive (list (my/kaolin-themes--select-prompt)))
  (my/load-theme theme))

;;------------------------------------------------------------------------------
;; solarized themes
(use-package solarized-theme
  :ensure t
  :bind (("C-c t S" . my/solarized-themes-select)))

(defconst my/solarized-themes
  '(solarized-dark
    solarized-light
    solarized-gruvbox-dark
    solarized-gruvbox-light
    solarized-selenized-dark
    solarized-selenized-light
    solarized-zenburn)
  "List of solarized themes.")

(defvar my/solarized-themes--select-theme-history nil
  "Minibuffer history of `my/solarized-themes--select-prompt'.")

(defun my/solarized-themes--select-prompt ()
  "Minibuffer prompt for `solarized-themes-select'.
With optional PROMPT string, use it.  Else use a generic prompt."
  (my/themes--select-prompt my/solarized-themes
                            my/solarized-themes--select-theme-history
                            "Select Solarized Theme: "))

(defun my/solarized-themes-select (theme)
  "Load a solarized THEME using minibuffer completion.
When called from Lisp, THEME is the symbol of a theme."
  (interactive (list (my/solarized-themes--select-prompt)))
  (my/load-theme theme))

;;------------------------------------------------------------------------------
;; toggle themes
(defconst my/theme-toggle-list
  '((hercules . hercules-light)
    (hercules-light . hercules)
    (moe-dark . moe-light)
    (moe-light . moe-dark)
    (modus-operandi . modus-vivendi)
    (modus-vivendi . modus-operandi)
    (kaolin-dark . kaolin-light)
    (kaolin-light . kaolin-dark)
    (kaolin-mono-dark . kaolin-mono-light)
    (kaolin-mono-light . kaolin-mono-dark)
    (kaolin-valley-dark . kaolin-valley-light)
    (kaolin-valley-light . kaolin-valley-dark)
    (solarized-dark . solarized-light)
    (solarized-light . solarized-dark)
    (solarized-gruvbox-dark . solarized-gruvbox-light)
    (solarized-gruvbox-light . solarized-gruvbox-dark)
    (solarized-selenized-dark . solarized-selenized-light)
    (solarized-selenized-light . solarized-selenized-dark)
    (doom-nord . doom-nord-light)
    (doom-nord-light . doom-nord)
    (doom-one . doom-one-light)
    (doom-one-light . doom-one)
    (doom-gruvbox . doom-gruvbox-light)
    (doom-gruvbox-light . doom-gruvbox)
    (doom-opera . doom-opera-light)
    (doom-opera-light . doom-opera)
    (doom-ayu-dark . doom-ayu-light)
    (doom-ayu-light . doom-ayu-dark)
    (doom-solarized-dark . doom-solarized-light)
    (doom-solarized-light . doom-solarized-dark)
    (doom-acario-dark . doom-acario-light)
    (doom-acario-light . doom-acario-dark)
    (doom-feather-dark . doom-feather-light)
    (doom-feather-light . doom-feather-dark)
    (doom-oksolar-dark . doom-oksolar-light)
    (doom-oksolar-light . doom-oksolar-dark)
    (doom-material-dark . doom-material)
    (doom-material . doom-material-dark)
    (ef-day . ef-night)
    (ef-night . ef-day)
    (ef-spring . ef-autumn)
    (ef-autumn . ef-spring)
    (ef-summer . ef-winter)
    (ef-winter . ef-summer)
    (ef-dark . ef-light)
    (ef-light . ef-dark)
    (ef-duo-dark . ef-duo-light)
    (ef-duo-light . ef-duo-dark)
    (ef-elea-dark . ef-elea-light)
    (ef-elea-light . ef-elea-dark)
    (ef-trio-dark . ef-trio-light)
    (ef-trio-light . ef-trio-dark)
    (ef-maris-dark . ef-maris-light)
    (ef-maris-light . ef-maris-dark)
    (ef-melissa-dark . ef-melissa-light)
    (ef-melissa-light . ef-melissa-dark)
    (ef-tritanopia-dark . ef-tritanopia-light)
    (ef-tritanopia-light . ef-tritanopia-dark)
    (ef-deuteranopia-dark . ef-deuteranopia-light)
    (ef-deuteranopia-light . ef-deuteranopia-dark)
    (ample . ample-light)
    (ample-light . ample)))

(defun my/toggle-theme ()
  (interactive)
  (let* ((current-theme (car custom-enabled-themes))
         (sister-theme (assoc current-theme my/theme-toggle-list)))
    (when sister-theme
      (my/load-theme (cdr sister-theme)))))

(bind-key "C-c t t" #'my/toggle-theme)

(use-package easy-theme-preview
  :init (my/vc-install "ayys/easy-theme-preview")
  :bind (("C-c t P" . easy-theme-preview)))

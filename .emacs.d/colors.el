;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; Nice themes
(defun my/load-theme (theme)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t))

(defmacro my/theme-package (theme package binding)
  (let* ((theme-name (symbol-name theme))
        (load-func-name (concat "my/load-theme-" theme-name)))
    `(progn
       (defun ,(intern load-func-name) ()
         (interactive)
         (my/load-theme ',theme))
       (use-package ,package
         :ensure t
         :defer t
         :bind ((,binding . ,(intern-soft load-func-name)))))))

(my/theme-package ample ample-theme "C-c t a")
(my/theme-package cyberpunk cyberpunk-theme "C-c t c")
(my/theme-package dracula dracula-theme "C-c t d")
(my/theme-package leuven leuven-theme "C-c t l")
(my/theme-package monokai monokai-theme "C-c t m")
(my/theme-package moe-dark moe-theme "")
(my/theme-package poet-dark poet-theme "C-c t p")
(my/theme-package tomorrow-night-deepblue tomorrow-night-deepblue-theme "C-c t b")
(my/theme-package zenburn zenburn-theme "C-c t z")

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

(defun load-theme-miasma ()
  (interactive)
  (my/load-theme 'miasma))

(use-package miasma-theme
  :init (my/vc-install "daut/miasma-theme.el")
  :bind (("C-c t s" . load-theme-miasma)))

(defun load-theme-fleury ()
  (interactive)
  (my/load-theme 'fleury))

(use-package fleury-theme
  :init (my/vc-install "ShamsParvezArka/fleury-theme.el")
  :bind (("C-c t f" . load-theme-fleury)))

;; default theme
(my/load-theme-cyberpunk)

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

;;------------------------------------------------------------------------------
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
      history))))

(defun cap-first (s)
  (if (> (length s) 0)
      (concat (upcase (substring s 0 1)) (downcase (substring s 1)))
    nil))

(defmacro my/theme-selector (theme)
  (let* ((theme-name (symbol-name theme))
        (var-name (concat "my/" theme-name "-themes"))
        (prompt-func-name (concat "my/" theme-name "-themes--select-prompt"))
        (select-func-name (concat "my/" theme-name "-themes-select"))
        (hist-name (concat "my/" theme-name "-themes--select-theme-history"))
        (prompt (concat "Select " (cap-first theme-name) " Theme: ")))
    `(progn
       (defvar ,(intern hist-name)  nil
         ,(concat "Minibuffer history of `" prompt-func-name "'."))
       (defun ,(intern prompt-func-name) ()
         ,(concat "Minibuffer prompt for `" select-func-name "'.")
         (my/themes--select-prompt ,(intern-soft var-name)
                                   ,(intern-soft hist-name)
                                   ,prompt))
       (defun ,(intern select-func-name) (theme)
         ,(concat "Load a " theme-name " theme using minibuffer completion.")
         (interactive (list (,(intern-soft prompt-func-name))))
         (my/load-theme theme)))))

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

(my/theme-selector doom)

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

(my/theme-selector kaolin)

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

(my/theme-selector solarized)

;;------------------------------------------------------------------------------
;; gruvbox themes
(use-package gruvbox-theme
  :ensure t
  :bind (("C-c t G" . my/gruvbox-themes-select)))

(defconst my/gruvbox-themes
  '(gruvbox-dark-hard
    gruvbox-dark-medium
    gruvbox-dark-soft
    gruvbox-light-hard
    gruvbox-light-medium
    gruvbox-light-soft)
  "List of gruvbox themes.")

(my/theme-selector gruvbox)

;;------------------------------------------------------------------------------
;; tomorrow themes
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :bind (("C-c t T" . my/tomorrow-themes-select)))

(defconst my/tomorrow-themes
  '(sanityinc-tomorrow-blue
    sanityinc-tomorrow-bright
    sanityinc-tomorrow-day
    sanityinc-tomorrow-eighties
    sanityinc-tomorrow-night)
  "List of tomorrow themes.")

(my/theme-selector tomorrow)

;;------------------------------------------------------------------------------
;; alect themes
(use-package alect-themes
  :ensure t
  :config
  (setq alect-display-class '((class color) (min-colors 256)))
  :bind (("C-c t A" . my/alect-themes-select)))

(defconst my/alect-themes
  '(alect-black
    alect-black-alt
    alect-light
    alect-light-alt
    alect-dark
    alect-dark-alt)
  "List of alect themes.")

(my/theme-selector alect)

;;------------------------------------------------------------------------------
;; other themes
(use-package apropospriate-theme
  :ensure t
  :defer t)

(use-package badwolf-theme
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-solarized
  :ensure t
  :defer t)

(use-package darktooth-theme
  :ensure t
  :defer t)

(use-package material-theme
  :ensure t
  :defer t)

(use-package spacemacs-theme
  :ensure t
  :defer t)

(use-package tao-theme
  :ensure t
  :defer t)

(defconst my/other-themes
  '(ample
    apropospriate-dark
    apropospriate-light
    badwolf
    cyberpunk
    darktooth
    darktooth-dark
    darktooth-darker
    dracula
    fleury
    hercules
    material
    material-light
    miasma
    moe-dark
    poet-dark
    sanityinc-solarized-dark
    sanityinc-solarized-light
    spacemacs-dark
    spacemacs-light
    tao-yin
    tao-yang
    tomorrow-night-deepblue
    tron-legacy
    zenburn)
  "List of miscellaneous themes.")

(my/theme-selector other)
(bind-key "C-c t O" 'my/other-themes-select)

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
    (doom-homage-white . doom-homage-black)
    (doom-homage-black . doom-homage-white)
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
    (ample-light . ample)
    (sanityinc-tomorrow-day . sanityinc-tomorrow-night)
    (sanityinc-tomorrow-night . sanityinc-tomorrow-day)
    (sanityinc-solarized-light . sanityinc-solarized-dark)
    (sanityinc-solarized-dark . sanityinc-solarized-light)
    (gruvbox-dark-hard . gruvbox-light-hard)
    (gruvbox-light-hard . gruvbox-dark-hard)
    (gruvbox-dark-medium . gruvbox-light-medium)
    (gruvbox-light-medium . gruvbox-dark-medium)
    (gruvbox-dark-soft . gruvbox-light-soft)
    (gruvbox-light-soft . gruvbox-dark-soft)
    (material . material-light)
    (material-light . material)
    (tao-yin . tao-yang)
    (tao-yang . tao-yin)
    (apropospriate-light . apropospriate-dark)
    (apropospriate-dark . apropospriate-light)
    (alect-light . alect-dark)
    (alect-dark . alect-light)
    (alect-light-alt . alect-dark-alt)
    (alect-dark-alt . alect-light-alt)
    (spacemacs-dark . spacemacs-light)
    (spacemacs-light . spacemacs-dark)))

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

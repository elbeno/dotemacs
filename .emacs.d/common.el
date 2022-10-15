;;------------------------------------------------------------------------------
;; Hide minor modes
(use-package diminish
  :ensure t)

;;------------------------------------------------------------------------------
;; UTF8 defaults
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;------------------------------------------------------------------------------
;; Clean up display
(tool-bar-mode -1)
(menu-bar-mode -1)
(display-battery-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t
      initial-scratch-message ""
      visible-bell 1
      x-stretch-cursor t)

;; Display defaults
(setq column-wrap-soft 80
      column-wrap-hard 100)
(setq sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Keep whitespace clean
(setq require-final-newline t)
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode)
  :diminish
  ws-butler-mode)

;;------------------------------------------------------------------------------
;; Copy/paste stuff
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t)

;;------------------------------------------------------------------------------
;; Autosaves/backups
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/autosaves" t)
(setq backup-directory-alist
      `((".*" . , "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/autosaves/" t)))
(setq create-lockfiles nil)

;; Delete backups older than one month
(defun delete-backups ()
  (interactive)
  (message "Deleting old backup files...")
  (let ((month (* 60 60 24 31))
        (current (float-time (current-time))))
    (dolist (file (directory-files temporary-file-directory t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (nth 5 (file-attributes file))))
                    month))
        (message file)
        (delete-file file)))))
(delete-backups)

;;------------------------------------------------------------------------------
;; More usable defaults
(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq load-prefer-newer t
      apropos-do-all t)
(diminish 'abbrev-mode)

;;------------------------------------------------------------------------------
;; Smooth scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;;------------------------------------------------------------------------------
;; Prevent prompt on opening large TAGS file
(setq large-file-warning-threshold 100000000)

;;------------------------------------------------------------------------------
;; IDO & amx
(use-package ido
  :ensure t
  :preface
  (defvar ido-cur-item               nil)
  (defvar ido-default-item           nil)
  (defvar inherit-input-method       nil)
  (defvar ido-cur-list               nil)
  (defvar ido-context-switch-command nil)
  (defvar ido-cr+-enable-next-call   nil)
  (defvar ido-cr+-replace-completely nil)
  (defvar ido-cr+-debug-mode         nil)
  (defvar ido-require-match          nil))

(use-package flx-ido
  :ensure t
  :config
  (ido-mode 1)
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point nil)
  (setq ido-create-new-buffer 'always)
  (flx-ido-mode 1)
  (setq ido-use-faces t)
  :after ido)

(use-package ido-completing-read+
  :ensure t
  :preface
  (defvar ido-ubiquitous-debug-mode nil)
  :config
  (ido-ubiquitous-mode)
  :after ido)

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-show-count t)
  (ido-vertical-mode 1)
  (set-face-attribute 'ido-vertical-first-match-face nil
                      :background "#e5b7c0")
  (set-face-attribute 'ido-vertical-only-match-face nil
                      :background "#e52b50"
                      :foreground "white")
  (set-face-attribute 'ido-vertical-match-face nil
                      :foreground "#b00000")
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  :after ido)

(use-package amx
  :ensure t
  :config
  (amx-mode)
  :demand)

;;------------------------------------------------------------------------------
;; Undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
        undo-tree-auto-save-history nil)
  :diminish undo-tree-mode
  :bind (("C-z" . undo-tree-undo)
         ("C-x u" . undo-tree-visualize)))

;;------------------------------------------------------------------------------
;; Google-this
(use-package google-this
  :ensure t
  :commands (google-this-parse-and-search-string)
  :bind
  (("C-c <f1>" . duckduckgo-this-cpp-reference)))

(defun duckduckgo-this-lucky-search-url ()
  "Return the url for a feeling-ducky duckduckgo search."
  (format "https://duckduckgo.com/?q=\\%%s"))

(defun duckduckgo-this-cpp-reference ()
  "Visit the most probable cppreference.com page for this word."
  (interactive)
  (google-this-parse-and-search-string
   (concat (thing-at-point 'symbol) " site:cppreference.com")
   nil (duckduckgo-this-lucky-search-url)))

;;------------------------------------------------------------------------------
;; Avy
(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-word-or-subword-1)
         ("C-;" . avy-goto-word-or-subword-1)
         ("C-." . avy-pop-mark)))

(use-package ace-window
  :ensure t
  :bind ("C-c w" . ace-window))

;;------------------------------------------------------------------------------
;; smart-scan: use M-n and M-p to jump to next/prev thing at point
(use-package smartscan
  :ensure t
  :config (global-smartscan-mode t))

;;------------------------------------------------------------------------------
;; visual-regexp-steroids: better regexp searching
(use-package visual-regexp-steroids
  :ensure t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-r" . vr/isearch-backward)
         ("C-s" . vr/isearch-forward)))

;;------------------------------------------------------------------------------
;; neotree: tree interface for opening files

;; When opening a file, or a directory with dired, hide the neotree window. Just
;; using neo-enter-hook doesn't quite do it, because neotree routes all
;; functionality (eg refresh, toggle hidden, etc) through neo-buffer--execute.

(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t
        neo-hidden-regexp-list '("\\.pyc$" "~$" "^#.*#$" "^\\.#\\..*$" "\\.elc$")
        my/neotree-opening-file nil
        my/neotree-entering-dired nil)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (defun neo-hide-on-enter (type path arg)
    (if (or (and (eq my/neotree-opening-file t)
                 (equal type 'file))
            (and (eq my/neotree-entering-dired t)
                 (equal type 'directory)))
        (neotree-hide))
    (setq my/neotree-opening-file nil
          my/neotree-entering-dired nil))
  (defun my/before-neobuffer-execute (arg0 &optional file-fn dir-fn &rest args)
    (when (eq dir-fn 'neo-open-dired)
      (setq my/neotree-entering-dired t))
    (when (or (eq file-fn 'neo-open-file)
              (eq file-fn 'neo-open-file-vertical-split)
              (eq file-fn 'neo-open-file-horizontal-split))
      (setq my/neotree-opening-file t)))
  (advice-add 'neo-buffer--execute :before #'my/before-neobuffer-execute)
  (add-hook 'neo-enter-hook #'neo-hide-on-enter)
  :bind (("<f12>" . neotree-toggle)))

;;------------------------------------------------------------------------------
;; make shell files executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;------------------------------------------------------------------------------
;; cycle CamelCase, snake_case, kebab-case etc
(use-package string-inflection
  :ensure t
  :bind (("C-c -" . string-inflection-all-cycle)
         ("C-c _" . string-inflection-toggle)))

;;------------------------------------------------------------------------------
;; better zap-to-char
(use-package zzz-to-char
  :ensure t
  :bind (("M-z" . zzz-up-to-char)))

;;------------------------------------------------------------------------------
;; browse kill-ring
(use-package browse-kill-ring
  :ensure t
  :bind (("C-M-y" . browse-kill-ring)))

;;------------------------------------------------------------------------------
;; symbol-overlay
(use-package symbol-overlay
  :ensure t
  :bind (("<f3>" . symbol-overlay-put)))

;;------------------------------------------------------------------------------
;; simple modeline
(defun my-truncate-buffer-name (buf-name)
  (let ((len (length buf-name)))
    (cond ((> len 50)
           (concat "..."
                   (substring buf-name (- len 47) len)))
          (t buf-name))))

(setq auto-revert-check-vc-info t)

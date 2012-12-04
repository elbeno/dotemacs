;; this file's directory
(setq dotfile-dir (file-name-directory
                   (or load-file-name (buffer-file-name))))

;; my stuff is in .emacs.d
(add-to-list 'load-path (concat dotfile-dir ".emacs.d/"))
;; 3rd party stuff is in site-lisp
(add-to-list 'load-path (concat dotfile-dir ".emacs.d/site-lisp/"))
;; pakacges are in elpa
(setq package-user-dir (concat dotfile-dir ".emacs.d/elpa/"))
;; extra binaries (if needed) are in site-bin
(add-to-list 'exec-path (concat dotfile-dir ".emacs.d/site-bin/"))

;; load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle)

;; process the pre-package stuff
(org-babel-load-file (concat dotfile-dir ".emacs.d/pre-package-init.org"))

;; initialize package system
(package-initialize)

;; packages installed:

;;caml              js2-mode
;;color-theme       lua-mode
;;column-marker     magit
;;csharp-mode       minimap
;;ecb               mo-git-blame
;;ghc               multiple-cursors
;;git-commit-mode   nlinum
;;gitconfig-mode    nxml-mode
;;gitignore-mode    nyan-mode
;;guess-offset      protobuf-mode
;;haskell-mode      slime
;;highlight-symbol  smart-tab
;;ido-ubiquitous    smex
;;                  undo-tree

;; process the post-package-init stuff (eg. always-load requires)
(org-babel-load-file (concat dotfile-dir ".emacs.d/post-package-init.org"))

;; apply custom variables
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

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

;; useful packages
(setq useful-packages
      '(caml
        color-theme
        csharp-mode
        ghc
        git-commit-mode
        gitconfig-mode
        gitignore-mode
        guess-offset
        ido-ubiquitous
        js2-mode
        lua-mode
        magit
        mo-git-blame
        nlinum
        nxml-mode
        protobuf-mode
        slime
        smart-tab
        smex
        undo-tree))

;; process the post-package-init stuff (eg. always-load requires)
(org-babel-load-file (concat dotfile-dir ".emacs.d/post-package-init.org"))

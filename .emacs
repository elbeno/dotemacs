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

(org-babel-load-file (concat dotfile-dir ".emacs.d/pre-package-init.org"))

;; when using a different init file, this needs to be called manually
(package-initialize)

(org-babel-load-file (concat dotfile-dir ".emacs.d/post-package-init.org"))

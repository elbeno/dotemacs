;; this file's directory
(setq dotfile-dir (file-name-directory
                   (or load-file-name (buffer-file-name))))

;; my stuff is in .emacs.d
(add-to-list 'load-path (concat dotfile-dir ".emacs.d/"))
;; 3rd party stuff is in site-lisp
(add-to-list 'load-path (concat dotfile-dir ".emacs.d/site-lisp/"))

;; load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle)

(org-babel-load-file (concat dotfile-dir ".emacs.d/config.org"))

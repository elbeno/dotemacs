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

;;bm                js2-mode
;;caml              lua-mode
;;color-theme       magit
;;column-marker     minimap
;;csharp-mode       mo-git-blame
;;ecb               multiple-cursors
;;ghc               nlinum
;;git-commit-mode   nxml-mode
;;gitconfig-mode    nyan-mode
;;gitignore-mode    protobuf-mode
;;                  slime
;;haskell-mode      smart-tab
;;highlight-symbol  smex
;;ido-ubiquitous    undo-tree


;; process the post-package-init stuff (eg. always-load requires)
(org-babel-load-file (concat dotfile-dir ".emacs.d/post-package-init.org"))

;; apply custom variables
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;; load any further custom stuff
(defun files-in-below-directory (directory)
  "List the .el files in DIRECTORY and in its sub-directories."
  ;; Although the function will be used non-interactively,
  ;; it will be easier to test if we make it interactive.
  ;; The directory will have a name such as
  ;;  "/usr/local/share/emacs/22.1.1/lisp/"
  (interactive "directory name: ")
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename ends in `.el'
       ;; and if so, append its name to a list.
       ((equal ".el" (substring (car (car current-directory-list)) -3))
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()
          ;; else descend into the directory and repeat the process
          (setq el-files-list
                (append
                 (files-in-below-directory
                  (car (car current-directory-list)))
                 el-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    el-files-list))

(mapcar 'load (files-in-below-directory (concat dotfile-dir ".emacs.d/custom/")))

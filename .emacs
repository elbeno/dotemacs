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

;; for some reason we need to set these ahead of loading org-mode!
(setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                 ("/" italic "<i>" "</i>")
                                 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                 ("=" org-code "<code>" "</code>" verbatim)
                                 ("~" org-verbatim "<code>" "</code>" verbatim)
                                 ("-" (:strike-through t) "<del>" "</del>")
                                 ("@" org-warning "<b>" "</b>"))))
(setq org-export-latex-emphasis-alist (quote
                                       (("*" "\\textbf{%s}" nil)
                                        ("/" "\\emph{%s}" nil)
                                        ("_" "\\underline{%s}" nil)
                                        ("+" "\\texttt{%s}" nil)
                                        ("-" "\\st{%s}" nil)
                                        ("=" "\\verb=%s=" nil)
                                        ("~" "\\verb~%s~" t)
                                        ("@" "\\alert{%s}" nil))))

;; load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle)

;; process the pre-package stuff
(org-babel-load-file (concat dotfile-dir ".emacs.d/pre-package-init.org"))

;; initialize package system
(package-initialize)

;; contents of elpa dir:
;; archives                        js2-mode-20130920.429
;; bm-20121212.2224                json-mode-1.1.0
;; caml-20130904.1012              lua-mode-20130910.922
;; cl-lib-0.3                      magit-20131001.226
;; color-theme-20080305.834        markdown-mode+-20120829.710
;; column-marker-20121128.843      markdown-mode-20130726.2142
;; csharp-mode-20130824.1200       minimap-20110427.1123
;; dash-20130911.1307              mo-git-blame-20130728.1717
;; ecb-20130826.1941               multiple-cursors-20130913.1148
;; epl-20131021.2022               nlinum-1.1
;; flx-20130728.2028               nxml-mode-20041004
;; flx-ido-20130814.2156           nyan-mode-20120710.1922
;; frame-cmds-20130921.1622        pkg-info-20131020.1746
;; frame-fns-20130723.2318         projectile-20131018.1037
;; ghc-20130520.1540               protobuf-mode-20091217.1955
;; git-commit-mode-20130919.341    s-20130905.558
;; gitconfig-mode-20130831.828     slime-20130929.1345
;; gitignore-mode-20130831.828     smart-tab-20130317.1157
;; git-rebase-mode-20130912.1429   smex-20130707.1255
;; haskell-mode-20130902.1113      undo-tree-20130812.1224
;; highlight-symbol-20130628.1552  yasnippet-20130907.1855
;; ido-ubiquitous-20130927.1229

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
(mapcar 'load (files-in-below-directory (concat dotfile-dir ".emacs.d/autoloads/")))

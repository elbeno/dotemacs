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

;; apply custom variables
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;; load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle)

;; process the pre-package stuff
(org-babel-load-file (concat dotfile-dir ".emacs.d/pre-package-init.org"))

;; initialize package system (only once)
(package-initialize)
(setq package-enable-at-startup nil)

;; contents of elpa dir:
;; archives                          git-rebase-mode-20131005.1730
;; auto-complete-20131128.233        haskell-mode-20131129.1536
;; bm-20121212.2224                  highlight-symbol-20131103.2233
;; caml-20130904.1012                ido-ubiquitous-20131009.1047
;; cl-lib-0.3                        js2-mode-20131119.116
;; color-theme-20080305.834          json-mode-20131016.1653
;; column-marker-20121128.843        lua-mode-20131019.959
;; csharp-mode-20130824.1200         magit-20131126.621
;; dash-20131030.2119                markdown-mode+-20120829.710
;; dropdown-list-20120329.1636       markdown-mode-20131005.1155
;; ecb-20131116.1319                 minimap-20110427.1123
;; epl-20131101.1205                 mo-git-blame-20131002.1223
;; etags-select-1.13                 multiple-cursors-20131126.2320
;; etags-table-20130824.1157         nlinum-1.1
;; f-20131130.1927                   nxml-mode-20041004
;; flx-20130728.2028                 nyan-mode-20120710.1922
;; flx-ido-20131030.1243             pkg-info-20131101.1208
;; frame-cmds-20130921.1622          popup-20130708.2245
;; frame-fns-20131028.2235           projectile-20131203.2055
;; fringe-helper-20130519.1641       protobuf-mode-20091217.1955
;; ghc-20131119.1414                 s-20130905.558
;; git-commit-mode-20131124.2132     slime-20131117.759
;; gitconfig-mode-20131009.1430      smart-tab-20130317.1157
;; git-gutter+-20130918.1335         smex-20130707.1255
;; git-gutter-fringe+-20130902.1129  undo-tree-20131119.144
;; gitignore-mode-20130831.828       yasnippet-20131129.2134

;; process the post-package-init stuff (eg. always-load requires)
(org-babel-load-file (concat dotfile-dir ".emacs.d/post-package-init.org"))

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

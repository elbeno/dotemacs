;; debugging
(setq debug-on-error t)

;; this file's true directory
(setq dotfile-dir (file-name-directory
                   (file-chase-links
                    (or load-file-name
                        (buffer-file-name)))))

;; my stuff is in .emacs.d
(add-to-list 'load-path (concat dotfile-dir ".emacs.d/"))
;; 3rd party stuff is in site-lisp
(add-to-list 'load-path (concat dotfile-dir ".emacs.d/site-lisp/"))
;; pakacges are in elpa
(setq package-user-dir (concat dotfile-dir ".emacs.d/elpa/"))
;; extra binaries (if needed) are in site-bin
(add-to-list 'exec-path (concat dotfile-dir ".emacs.d/site-bin/"))

;; apply custom variables
(setq custom-file (concat dotfile-dir ".emacs.d/custom.el"))
(load custom-file)

;; set up package archives
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(defadvice package-compute-transaction
  (before
   package-compute-transaction-reverse (package-list requirements)
   activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements))

;; initialize package system (only once)
(package-initialize)
(setq package-enable-at-startup nil)

;; contents of elpa dir:
;; ace-jump-mode-20130720.1153       git-rebase-mode-20131124.1840
;; archives                          haskell-mode-20131129.1536
;; auto-complete-20131128.233        highlight-symbol-20131103.2236
;; bm-20121212.2224                  ido-ubiquitous-20131009.1047
;; caml-20130904.1012                js2-mode-20131119.116
;; cl-lib-0.3                        json-mode-20131016.1653
;; color-theme-20080305.834          lua-mode-20131019.959
;; column-marker-20121128.843        magit-20131126.621
;; csharp-mode-20130824.1200         markdown-mode+-20120829.710
;; dash-20131030.2119                markdown-mode-20131005.1155
;; dropdown-list-20120329.1636       minimap-20110427.1123
;; ecb-20131116.1319                 mo-git-blame-20131002.1223
;; epl-20131101.1205                 multiple-cursors-20131128.149
;; etags-select-1.13                 nlinum-1.1
;; etags-table-20130824.1157         nxml-mode-20041004
;; f-20131130.1927                   nyan-mode-20131014.1806
;; flx-20131030.1243                 pkg-info-20131101.1208
;; flx-ido-20131030.1243             popup-20130901.2255
;; frame-cmds-20130921.1622          projectile-20131203.2055
;; frame-fns-20131028.2235           protobuf-mode-20091217.1955
;; fringe-helper-20130519.1641       s-20130905.558
;; ghc-20131120.1428                 slime-20131117.759
;; git-commit-mode-20131124.2132     smart-mode-line-20131204.1651
;; gitconfig-mode-20131124.1840      smart-tab-20130317.1157
;; git-gutter+-20130918.1335         smex-20130707.1255
;; git-gutter-fringe+-20130902.1129  undo-tree-20131119.144
;; gitignore-mode-20131124.1840      yasnippet-20131203.1520

;; load up org-mode and org-babel
(require 'org-install)
(require 'ob-tangle)

;; process the main init file
;(load (concat dotfile-dir ".emacs.d/init.el"))
(org-babel-load-file (concat dotfile-dir ".emacs.d/init.org"))

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

;; autoloads are general extra stuff that I've written
(mapcar 'load (files-in-below-directory (concat dotfile-dir ".emacs.d/autoloads/")))

;; custom stuff is per-installation/work private
(mapcar 'load (files-in-below-directory (concat dotfile-dir ".emacs.d/custom/")))

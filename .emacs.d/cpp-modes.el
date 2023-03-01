;;------------------------------------------------------------------------------
;; C++ mode
(setq-default c-basic-offset 2)

;;------------------------------------------------------------------------------
;; LLVM root directory
(defun find-file-recursive (directory filename)
  (if (and directory (file-directory-p directory))
      (let ((found-files
             (directory-files-recursively directory
                                          (concat "^" filename "$"))))
        (if found-files
            (car found-files)
          nil))
    nil))

(defun apply-macro (macro arg-list)
  (eval
   `(,macro ,@(cl-loop for arg in arg-list
                    collect `(quote ,arg)))))

(defun find-file-first-dir (directories filename)
  (apply-macro 'or (append (mapcar (lambda (dir) (find-file-recursive dir filename))
                                   directories)
                           (list (executable-find filename)))))

(defcustom my-cpp-llvm-bin-marker "clangd"
  "The file to use to detect the llvm bin directory."
  :group 'my-cpp-config
  :type 'string
  :safe 'stringp)

(defcustom my-cpp-llvm-roots '("/usr/local/llvm/")
  "The paths to search for llvm binaries."
  :group 'my-cpp-config
  :type 'list
  :safe 'listp)

(defun find-llvm-root ()
  (let ((llvm-bin (find-file-first-dir
                    my-cpp-llvm-roots my-cpp-llvm-bin-marker)))
    (when llvm-bin
      (string-remove-suffix "bin/"
                            (file-name-directory (file-truename llvm-bin))))))

(defun find-exe (root filename)
  (or (find-file-recursive root filename)
      (executable-find filename)))

;;------------------------------------------------------------------------------
;; syntax highlighting
(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

;;------------------------------------------------------------------------------
;; clang-format
(defcustom my-clang-format-enabled t
  "If t, run clang-format on cpp buffers upon saving."
  :group 'clang-format
  :type 'boolean
  :safe 'booleanp)

(use-package clang-format
  :ensure t
  :config
  (setq clang-format-executable (find-exe (find-llvm-root) "clang-format"))
  (defun my/config-clang-format ()
    (when my-clang-format-enabled
      (add-hook 'before-save-hook 'clang-format-buffer nil t))
    (bind-keys :map c++-mode-map
               ("C-c f" . clang-format-buffer)))
  :hook (c++-mode . my/config-clang-format))

;; clang-format files are YAML
(add-to-list 'auto-mode-alist '("\\.clang-format\\'" . yaml-mode))

;;------------------------------------------------------------------------------
;; Auto insertion of headers
(autoload 'cpp-auto-include/namespace-qualify-file "cpp-auto-include"
  "Explicitly qualify uses of the standard library with their namespace(s)." t)
(autoload 'cpp-auto-include/ensure-includes-for-file "cpp-auto-include"
  "Auto-insert #include line(s) required for the current buffer." t)
(autoload 'cpp-auto-include/ensure-includes-for-current-line "cpp-auto-include"
  "Auto-insert #include line(s) required for the current line." t)
(eval-after-load 'cc-mode
  '(bind-keys :map c++-mode-map
              ("C-c n" . cpp-auto-include/namespace-qualify-file)
              ("C-c i" . cpp-auto-include/ensure-includes-for-file)
              ("C-c o" . cpp-auto-include/ensure-includes-for-current-line)))

;;------------------------------------------------------------------------------
;; Transposing arguments
(autoload 'c-transpose-args "c-transpose-args"
  "Transpose function arguments." t)
(autoload 'c-transpose-args-forward "c-transpose-args"
  "Transpose function arguments forward." t)
(autoload 'c-transpose-args-backward "c-transpose-args"
  "Transpose function arguments backward." t)
(eval-after-load 'cc-mode
  '(bind-keys :map c++-mode-map
              ("C-M-t" . c-transpose-args)))

;;------------------------------------------------------------------------------
;; Toggling include quote styles
(autoload 'c-toggle-include-quotes "c-change-brackets"
  "Toggle between angled includes and quoted includes." t)
(eval-after-load 'cc-mode
  '(bind-keys :map c++-mode-map
              ("C-c q" . c-toggle-include-quotes)))

;;------------------------------------------------------------------------------
;; indentation rules
(defun indentation-c-mode-hook ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'member-init-cont '-)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open 0))
(add-hook 'c-mode-common-hook 'indentation-c-mode-hook)

;;------------------------------------------------------------------------------
;; Align Boost.SML tables
(autoload 'find-and-align-boost-sml "boost-sml"
  "Find and align Boost.SML tables." t)

(eval-after-load 'cc-mode
  '(bind-keys :map c++-mode-map
              ("C-<tab>" . align)
              ("C-]" . find-and-align-boost-sml)))

;;------------------------------------------------------------------------------
;; lsp + clangd
(use-package flycheck-clang-tidy
  :ensure t
  :config
  (setq flycheck-clang-tidy-executable (find-exe (find-llvm-root) "clang-tidy"))
  :defer)

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-completion-provider :none)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (defun my/config-lsp-mode ()
    ;; Start lsp mode etc unless we're in a temp buffer
    ;; (don't do it when exporting org-mode blocks)
    (unless (string-match-p (regexp-quote "*temp*") (buffer-name))
      (require 'lsp-clangd)
      (lsp)
      (require 'lsp-diagnostics)
      (lsp-diagnostics-flycheck-enable)
      (flycheck-clang-tidy-setup)
      (flycheck-add-next-checker 'lsp 'c/c++-clang-tidy)
      (bind-keys :map flycheck-mode-map
                 ("M-<down>" . flycheck-next-error)
                 ("M-<up>" . flycheck-previous-error))))
  :config
  (setq lsp-enable-indentation nil
        lsp-auto-guess-root t
        lsp-clangd-binary-path (find-exe (find-llvm-root) "clangd")
        lsp-prefer-flymake nil)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (c++-mode . my/config-lsp-mode)
         (lsp-completion-mode . my/lsp-mode-setup-completion)))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-<return>") #'lsp-ui-sideline-apply-code-actions)
  (define-key lsp-ui-mode-map (kbd "C-c m") #'lsp-ui-imenu)
  (defun my/config-lsp-ui-mode ()
      (lsp-enable-imenu)
      (lsp-ui-mode))
  :hook (lsp-mode . my/config-lsp-ui-mode))

;;------------------------------------------------------------------------------
;; building & error navigation
(setq compilation-scroll-output t)

;; Remove compilation window on success
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "1 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No compilation errors!")))))

(eval-after-load 'cc-mode
  '(bind-keys :map c++-mode-map
              ("M-k" . projectile-compile-project)))

;; make compilation buffers support ANSI colours
(defun colorize-compilation-buffer ()
  (require 'ansi-color)
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;------------------------------------------------------------------------------
;; Debugging

(setq gdb-many-windows t
      gdb-show-main t)

(defun gdb-run-or-cont (arg)
  "Run or continue program with numeric argument ARG."
  (interactive "p")
  (when (boundp 'gdb-thread-number)
    (if (eq gdb-thread-number nil)
        (gud-run arg)
      (gud-cont arg))))

(use-package gud
  :bind (("C-x C-a <f5>" . gdb-run-or-cont)))

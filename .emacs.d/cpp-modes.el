;;------------------------------------------------------------------------------
;; C++ mode
(setq-default c-basic-offset 2)

;;------------------------------------------------------------------------------
;; LLVM root directory
(defun find-file-recursive (directory filename)
  (if (and directory (file-directory-p directory))
      (let ((found-clang-formats
             (directory-files-recursively directory
                                          (concat "^" filename "$"))))
        (if found-clang-formats
            (car found-clang-formats)
          nil))
    nil))

(defun apply-macro (macro arg-list)
  (eval
   `(,macro ,@(cl-loop for arg in arg-list
                    collect `(quote ,arg)))))

(defun find-file-first-dir (directories filename)
  (apply-macro 'or (append (mapcar (lambda (dir) (find-file-recursive dir filename))
                                   possible-roots)
                           (list (executable-find filename)))))

(defun find-llvm-root (possible-roots)
  (let ((clang-exe (find-file-first-dir possible-roots "clangd")))
    (when clang-exe (file-truename clang-exe))))

(setq-default llvm-roots '("/usr/local/llvm/"))
(setq-default llvm-root
              (let ((root (find-llvm-root llvm-roots)))
                (when root
                  (string-remove-suffix "bin/" (file-name-directory root)))))

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
(setq clang-format-executable (find-exe llvm-root "clang-format"))

(use-package clang-format
  :ensure t)

(eval-after-load 'cc-mode
  '(bind-keys :map c++-mode-map
              ("C-c f" . clang-format)))

;; clang-format-on-save
(defcustom my-clang-format-enabled t
  "If t, run clang-format on cpp buffers upon saving."
  :group 'clang-format
  :type 'boolean
  :safe 'booleanp)

(defun my-clang-format-before-save ()
  (interactive)
  (when my-clang-format-enabled
    (clang-format-buffer)))

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
              ("C-c q" . cpp-auto-include/namespace-qualify-file)
              ("C-c i" . cpp-auto-include/ensure-includes-for-file)
              ("C-c o" . cpp-auto-include/ensure-includes-for-current-line)))

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
;; lsp + clangd + company

(when (display-graphic-p)
  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode)))

(setq company-clang-executable (find-exe llvm-root "clang"))
(setq my-clangd-executable (find-exe llvm-root "clangd"))
(setq my-clang-tidy-executable (find-exe llvm-root "clang-tidy"))

;; In c++-mode, start lsp mode etc unless we're in a temp buffer
;; (don't do it when exporting org-mode blocks)
(defun my-c++-mode-hook ()
  (unless (string-match-p (regexp-quote "*temp*") (buffer-name))
    (add-hook 'before-save-hook 'my-clang-format-before-save nil t)
    (company-mode)
    (lsp)))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(use-package lsp-mode
  :ensure t
  :init
  (require 'lsp-clangd)
  (setq lsp-enable-indentation nil
        lsp-auto-guess-root t
        lsp-clangd-binary-path my-clangd-executable
        lsp-prefer-flymake nil))

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
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "M-RET") #'lsp-ui-sideline-apply-code-actions)
  (define-key lsp-ui-mode-map (kbd "C-c m") #'lsp-ui-imenu)
  :hook ((lsp-mode . lsp-enable-imenu)
         (lsp-mode . lsp-ui-mode)))

;;------------------------------------------------------------------------------
;; Header completion
(use-package company-c-headers
  :ensure t
  :config
  (push 'company-c-headers company-backends))

;;------------------------------------------------------------------------------
;; Building & error navigation

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
(require 'ansi-color)
(defun colorize-compilation-buffer ()
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

;;------------------------------------------------------------------------------
;; Transpose function args
(defun c-forward-to-argsep ()
  "Move to the end of the current c function argument.
Returns point."
  (interactive)
  (while
    (progn (comment-forward most-positive-fixnum)
      (looking-at "[^,)>}]"))
    (forward-sexp))
  (point))

(defun c-backward-to-argsep ()
  "Move to the beginning of the current c function argument.
Returns point."
  (interactive)
  (let ((pt (point)) cur)
    (up-list -1)
    (forward-char)
    (while
      (progn
        (setq cur (point))
        (> pt (c-forward-to-argsep)))
      (forward-char))
    (goto-char cur)))

(defun c-transpose-args-direction (is_forward)
  "Transpose two arguments of a c-function.
The first arg is the one with point in it."
  (interactive)
  (let*
      (;; only different to pt when not 'is_forward'
       (pt-original (point))
       (pt
        (progn
          (when (not is_forward)
            (goto-char (- (c-backward-to-argsep) 1))
            (unless (looking-at ",")
              (goto-char pt-original)
              (user-error "Argument separator not found")))
          (point)))
       (b (c-backward-to-argsep))
       (sep
        (progn (goto-char pt)
               (c-forward-to-argsep)))
       (e
        (progn
          (unless (looking-at ",")
            (goto-char pt-original)
            (user-error "Argument separator not found"))
          (forward-char)
          (c-forward-to-argsep)))
       (ws-first
        (buffer-substring-no-properties
         (goto-char b)
         (progn (skip-chars-forward "[[:space:]\n]")
                (point))))
       (first (buffer-substring-no-properties (point) sep))
       (ws-second
        (buffer-substring-no-properties
         (goto-char (1+ sep))
         (progn (skip-chars-forward "[[:space:]\n]")
                (point))))
       (second (buffer-substring-no-properties (point) e)))
    (delete-region b e)
    (insert ws-first second "," ws-second first)

    ;; Correct the cursor location to be on the same character.
    (if is_forward
        (goto-char
         (+
          ;; word start.
          (- (point) (length first))
          ;; Apply initial offset within the word.
          (- pt b (length ws-first))))
      (goto-char
       (+
        b (length ws-first)
        ;; Apply initial offset within the word.
        (- pt-original (+ pt 1 (length ws-second))))))))


(defun c-transpose-args-forward () (interactive) (c-transpose-args-direction t))
(defun c-transpose-args-backward () (interactive) (c-transpose-args-direction nil))

(defun c-transpose-args (prefix)
  "Transpose argument at point with the argument before it.
With prefix arg ARG, transpose with the argument after it."
  (interactive "P")
  (cond ((not prefix) (c-transpose-args-backward))
        (t (c-transpose-args-forward))))


(eval-after-load 'cc-mode
  '(bind-keys :map c++-mode-map
              ("C-M-t" . c-transpose-args)))

;;------------------------------------------------------------------------------
;; Flycheck setup for c++-mode
(use-package flycheck-clang-tidy
  :ensure t
  :init
  (setq flycheck-clang-tidy-executable my-clang-tidy-executable)
  :after flycheck
  :hook
  ((flycheck-mode . flycheck-clang-tidy-setup)))

;; From https://github.com/flycheck/flycheck/issues/1762#issuecomment-749789589
;; Add buffer local Flycheck checkers after LSP for different major modes.
(defvar-local my-flycheck-local-cache nil)

(defun my-flycheck-local-checker-get (fn checker property)
  ;; Only check the buffer local cache for the LSP checker, otherwise we get
  ;; infinite loops.
  (if (eq checker 'lsp)
      (or (alist-get property my-flycheck-local-cache)
          (funcall fn checker property))
    (funcall fn checker property)))

(advice-add 'flycheck-checker-get
            :around 'my-flycheck-local-checker-get)
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'c++-mode)
              (setq my-flycheck-local-cache '((next-checkers . (c/c++-clang-tidy)))))))

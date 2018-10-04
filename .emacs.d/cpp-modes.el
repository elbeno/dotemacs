;;------------------------------------------------------------------------------
;; C++ mode
(setq-default c-basic-offset 2)

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

;; clang-format
(use-package clang-format
  :ensure t
  :bind
  (("C-c f" . clang-format)))

;; clang-format-on-save
(defcustom my-clang-format-enabled t
  "If t, run clang-format on cpp buffers upon saving."
  :group 'clang-format
  :type 'boolean
  :safe 'booleanp)

(defun my-clang-format-before-save ()
  (interactive)
  (if my-clang-format-enabled
      (when (eq major-mode 'c++-mode) (clang-format-buffer))
    (message "my-clang-format-enabled is false")))
(add-hook 'before-save-hook 'my-clang-format-before-save)

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

;; indentation rules
(defun indentation-c-mode-hook ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'member-init-cont '-)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'case-label '+)
  (c-set-offset 'statement-case-open 0))
(add-hook 'c-mode-common-hook 'indentation-c-mode-hook)

;; Align boost SML tables
(defun align-boost-sml (start end)
 (interactive "r")
 (indent-region start end)
 (align-regexp start end "[[:space:]]*\\([[:space:]]\\)\\*" 1 0)
 (align-regexp start end "\\([[:space:]]*\\)\\+")
 (align-regexp start end "\\([[:space:]]*\\)\\[")
 (align-regexp start end "\\([[:space:]]*\\)[[:space:]]/[^/]" 1 0)
 (align-regexp start end "\\([[:space:]]*\\)="))

(defun find-and-align-boost-sml ()
  "Align all Boost.SML tables in the buffer using align-boost-sml.
This function assumes Boost.SML tables are delimited with
// clang-format off
  and
// clang-format on"
  (interactive)
  (if mark-active
    (align-boost-sml (region-beginning) (region-end))
    (save-excursion
      (let ((case-fold-search t))
        (goto-char (point-min))
        (while (search-forward "// clang-format off" nil t)
          (let ((start (point)))
            (when (search-forward "// clang-format on" nil t)
              (align-boost-sml start (point)))))))))

(eval-after-load 'cc-mode
  '(bind-keys :map c++-mode-map
              ("C-<tab>" . align)
              ("C-]" . find-and-align-boost-sml)))

;;------------------------------------------------------------------------------
;; lsp + clangd + company

;; because clangd doesn't support find references
(defun my-force-lsp-xref ()
  (when (and lsp-enable-xref
             (or (lsp--capability "referencesProvider")
                 (lsp--capability "definitionProvider")))
    (setq-local xref-backend-functions (list #'lsp--xref-backend))))

(setq my-clangd-path "/usr/local/llvm/bin/clangd")

(use-package lsp-mode
  :ensure t
  :config
  (lsp-define-stdio-client lsp-clangd-c++
                           "cpp"
                           #'projectile-project-root
                           (list my-clangd-path)
                           :ignore-regexps
                           '("^Error -[0-9]+: .+$"))
  :hook ((c++-mode . my-force-lsp-xref)
         (c++-mode . lsp-mode)
         (c++-mode . lsp-clangd-c++-enable)))

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
  :hook ((lsp-mode . lsp-enable-imenu)
         (lsp-mode . lsp-ui-mode)))

(use-package company-lsp
  :after company
  :ensure t
  :config
  (require 'company-lsp)
  (push 'company-lsp company-backends)
  :hook (c++-mode . company-mode))

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
              ("M-<down>" . next-error)
              ("M-<up>" . previous-error)
              ("M-k" . projectile-compile-project)))

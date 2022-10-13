;;------------------------------------------------------------------------------
;; Org-mode
(use-package org
  :ensure t
  :commands (org-mode)
  :mode ("\\.org$" . org-mode)
  :bind (:map org-mode-map
              ("C-M-t" . my-org-table-transpose-cells))
  :config
  (setq org-log-done t
        org-support-shift-select t
        org-startup-indented t
        org-src-fontify-natively t
        org-completion-use-ido t
        org-export-allow-bind-keywords t
        org-latex-listings 'minted
        org-reveal-note-key-char nil)
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-minted-options
        '(("frame" "none")
          ("fontsize" "\\scriptsize")
          ("linenos" "")
          ))
  (setq org-beamer-outline-frame-title "Contents")
  (setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                            (sequence "⚑ WAITING(w)" "|")
                            (sequence "|" "✘ CANCELLED(c)")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (js . t)
     (haskell . t)
     (emacs-lisp . t)))
  (add-to-list 'org-structure-template-alist '("n" . "notes")))

;; better header bullets
(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
        '("◉" "✸" "✿" "◎" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; better inline list bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1)
                                                        (match-end 1) "•"))))))

(use-package org-re-reveal
  :ensure t
  :defer)

(use-package htmlize
  :ensure t
  :defer)

(defvar modi/htmlize-initial-flyspell-state nil
  "Variable to store the state of `flyspell-mode' when `htmlize-buffer' is called.")

(defun modi/htmlize-before-hook-fn ()
  (when (fboundp 'flyspell-mode)
    (setq modi/htmlize-initial-flyspell-state flyspell-mode)
    (when flyspell-mode
      (flyspell-mode -1))))
(add-hook 'htmlize-before-hook #'modi/htmlize-before-hook-fn)

(defun modi/htmlize-after-hook-fn ()
  (when (fboundp 'flyspell-mode)
    (when modi/htmlize-initial-flyspell-state
      (flyspell-mode 1))))
(add-hook 'htmlize-after-hook #'modi/htmlize-after-hook-fn)

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c i" 'org-iswitchb)

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'org-latex-packages-alist '("" "minted" nil))
            (bind-key "M-Q" 'toggle-truncate-lines org-mode-map)
            (require 'ox-latex)
            (require 'ox-beamer)
            (require 'org-re-reveal)
            (require 'htmlize)
            (add-to-list 'org-beamer-environments-extra
			 '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))))

(setq initial-major-mode 'org-mode)

;; swap org-mode cells
(defun md-org-table-swap-cells (row col nextrow nextcol)
  (interactive)
  (let ((curfield (org-table-get row col))
        (nextfield (org-table-get nextrow nextcol)))
    (org-table-analyze)
    (org-table-put row col nextfield)
    (org-table-put nextrow nextcol curfield)
    (org-table-align)
    (org-table-goto-field (format "@%s$%s" nextrow nextcol))
    (message "md-org-table-swap-cells %s:%s <-> %s:%s"
             (format "@%s$%s" row col) curfield (format "@%s$%s" nextrow nextcol) nextfield)))

(defun md-org-table-swap-cell-right ()
  (interactive)
  (if (org-at-table-p)
      (let* ((col (org-table-current-column))
             (row (org-table-current-dline))
             (nextrow row)
             (nextcol (+ col 1)))
        (md-org-table-swap-cells row col nextrow nextcol)
        (md-update-todo-status))
    (org-shiftright)))

(defun md-org-table-swap-cell-left ()
  (interactive)
  (if (org-at-table-p)
      (let* ((col (org-table-current-column))
             (row (org-table-current-dline))
             (nextrow row)
             (nextcol (- col 1)))
        (md-org-table-swap-cells row col nextrow nextcol)
        (md-update-todo-status))
    (org-shiftleft)))

(defun my-org-table-transpose-cells (prefix)
  "Transpose argument at point with the argument before it.
With prefix arg ARG, transpose with the argument after it."
  (interactive "P")
  (cond ((not prefix) (md-org-table-swap-cell-left))
        (t (md-org-table-swap-cell-right))))

;; render tables with latex
;; https://old.reddit.com/r/emacs/comments/d3a8or/pretty_org_tables_in_the_buffer_chapter_2_it/

(defun my-render-org-table-at-point ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (overlays-at (point))
        ;; this is a rough solution, because there can
        ;; be other overlays at point
        (delete-overlay (car (overlays-at (point))))

      (let* ((element-type (org-element-type (org-element-at-point))))
        (if (and (not (eq element-type 'table))
                 (not (eq element-type 'table-row)))
            (error "not at an org table")

          (while (not (eq 'table (org-element-type (org-element-at-point))))
            (forward-line -1))

          (my-render-org-table (org-element-at-point)))))))


(defun my-render-org-table (table)
  (interactive)
  (let* ((begin (org-element-property :begin table))
         (end (let ((pos (org-element-property :end table)))
                (goto-char pos)
                (beginning-of-line)
                ;; skip possible space after table
                (while (not (looking-at " *[|#]"))
                  (setq pos (point))
                  (forward-line -1))
                pos))
         (tabletxt (buffer-substring-no-properties begin end))
         (img (with-temp-buffer
                (insert tabletxt)
                (mark-whole-buffer)
                (org-latex-convert-region-to-latex)
                (org-preview-latex-fragment)
                (goto-char (point-min))
                (overlay-get  (car (overlays-at (point))) 'display)))
         (overlay (make-overlay begin end)))
    (overlay-put overlay 'display img)
    (forward-line -1)))


(defun my-render-org-tables-in-buffer ()
  (save-excursion
    (org-element-map (org-element-parse-buffer) 'table 'my-render-org-table)))

;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; tree-sitter grammar repositories
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c" "v0.23.6")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python" "v0.23.6")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;------------------------------------------------------------------------------
;; install languages for tree-sitter
(setq my/treesit-languages '(c cpp python))

(when (fboundp 'treesit-language-available-p)
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (python-mode . python-ts-mode)
          (json-mode . json-ts-mode)))
  (mapc (lambda (lang)
          (unless (treesit-language-available-p lang)
            (treesit-install-language-grammar lang)))
        my/treesit-languages))

(defun use-treesit-for (lang)
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p lang)
       (member lang my/treesit-languages)))

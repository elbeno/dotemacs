;; Colours in graphical mode
(load-theme 'hipster)

(when (display-graphic-p)
  ;; (set-face-foreground 'font-lock-comment-face "gray")
  ;; (set-face-foreground 'font-lock-string-face "firebrick")
  ;; (set-face-foreground 'font-lock-warning-face "black")
  ;; (set-face-background 'font-lock-warning-face "orange")
  ;; (set-face-background 'region "moccasin")
  ;; (set-face-foreground 'region "navy")
  )

;; Highlight FIXME/TODO
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(FIXME\\|TODO\\).*?:" 0 font-lock-warning-face prepend)))

;; Special types of comments
(defface font-lock-comment-strike
  '((t (:strike-through t)))
  "For strike-through comments")

(defface font-lock-comment-important
  '((t (:foreground "#00ff00")))
  "For important")

(defface font-lock-comment-todo
  '((t (:foreground "#ff0000")))
  "For todo comments")

(defun add-custom-keyw()
  "adds a few special keywords"
  (font-lock-add-keywords
   nil
   '(("\\s<+x[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-strike prepend)
     ("\\s<+t[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-todo prepend)
     ("\\s<+i[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-important prepend))))

(add-hook 'prog-mode-hook #'add-custom-keyw)

(defun add-custom-keyw-cpp()
  "adds a few special keywords"
  (font-lock-add-keywords
   nil
   '(("//+x[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-strike prepend)
     ("//+t[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-todo prepend)
     ("//+i[[:space:]]*\\(.*?\\)[[:space:]]*\\s>" 1 'font-lock-comment-important prepend))))

(add-hook 'c++-mode-hook #'add-custom-keyw-cpp)

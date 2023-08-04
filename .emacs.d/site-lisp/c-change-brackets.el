(defun c-change-to-angle-include ()
  (when (re-search-forward "\"" nil t)
    (replace-match ">")
    (re-search-backward "\"" nil t)
    (replace-match "<")))

(defun c-change-to-quote-include ()
  (when (re-search-forward ">" nil t)
    (replace-match "\"")
    (re-search-backward "<" nil t)
    (replace-match "\"")))

(defun my/ts-toggle-quotes (node)
  (let* ((b (treesit-node-start node))
         (e (treesit-node-end node))
         (s (buffer-substring b e))
         (q (substring s 0 1))
         (replace-left (if (string= q "<") "\"" "<"))
         (replace-right (if (string= q "<") "\"" ">")))
    (when (or (string= q "<") (string= q "\""))
      (goto-char b)
      (delete-forward-char 1)
      (insert replace-left)
      (goto-char e)
      (delete-backward-char 1)
      (insert replace-right))))

;;;###autoload
(defun c-toggle-include-quotes ()
  "Change quotes around a string.

Does the change only if the point is inside quotes that are
syntactically correct under the current major mode."
  (interactive)
  (save-excursion
    (if (derived-mode-p 'c++-ts-mode)
        (let ((node (treesit-node-at (point))))
          (if (string= (treesit-node-type node) "system_lib_string")
              (my/ts-toggle-quotes node)
            (my/ts-toggle-quotes (treesit-node-parent node))))
      (cond ((nth 3 (syntax-ppss))
             (c-change-to-angle-include))
            ((nth 1 (syntax-ppss))
             (goto-char (nth 1 (syntax-ppss)))
             (when (looking-at "<")
               (c-change-to-quote-include)))))))

(provide 'c-toggle-include-quotes)

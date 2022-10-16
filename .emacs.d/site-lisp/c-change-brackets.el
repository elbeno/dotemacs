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

;;;###autoload
(defun c-toggle-include-quotes ()
  "Change quotes around a string.

Does the change only if the point is inside quotes that are
syntactically correct under the current major mode."
  (interactive)
  (save-excursion
    (cond ((nth 3 (syntax-ppss))
           (c-change-to-angle-include))
          ((nth 1 (syntax-ppss))
           (goto-char (nth 1 (syntax-ppss)))
           (when (looking-at "<")
             (c-change-to-quote-include))))))

(provide 'c-toggle-include-quotes)

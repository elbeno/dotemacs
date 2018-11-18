;;; cmake-format.el --- Format code using cmake-format  -*- lexical-binding: t; -*-

(defgroup cmake-format nil
  "Format code using cmake-format."
  :group 'tools)

(defcustom cmake-format-executable
  (or (executable-find "cmake-format")
      "~/.local/bin/cmake-format")
  "Location of the cmake-format executable.

A string containing the name or the full path of the executable."
  :group 'cmake-format
  :type '(file :must-match t)
  :risky t)

;;;###autoload
(defun cmake-format-buffer ()
  "Use cmake-format to format the current buffer."
  (interactive)
  (let ((filename (make-temp-file "tmp-cmake-format")))
    ;; write current buffer to temp file
    (let ((coding-system-for-write 'binary)
          (contents (buffer-string)))
      (with-temp-file filename
        (set-buffer-multibyte nil)
        (encode-coding-string contents 'utf-8 nil (current-buffer))))
    ;; run cmake-format on temp file
    (let ((old-point (point))
          (replacement (with-temp-buffer
                         (when (eq (call-process cmake-format-executable nil t nil filename) 0)
                           (buffer-string)))))
      ;; replace contents of current buffer
      (when (not (= (length replacement) 0))
        (erase-buffer)
        (insert replacement)
        (goto-char old-point)))
    ;; get rid of temp file
    (delete-file filename)))

;;;###autoload
(defalias 'cmake-format 'cmake-format-buffer)

(provide 'cmake-format)

;;; cmake-format.el ends here

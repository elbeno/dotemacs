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

(defcustom cmake-format-config-file
  ".cmake-format.yaml"
  "Name of the cmake-format config file.

A string containing the name (without path) of the cmake-format config file."
  :group 'cmake-format
  :type '(file :must-match t)
  :risky t)

(defun cmake-config-argument (filename)
  (if (> (length cmake-format-config-file) 0)
      `("-c" ,(concat (locate-dominating-file filename cmake-format-config-file)
                      cmake-format-config-file))
    nil))

;;;###autoload
(defun cmake-format-buffer ()
  "Use cmake-format to format the current buffer."
  (interactive)
  (let ((filename (make-temp-file "tmp-cmake-format"))
        (config-arg (cmake-config-argument (buffer-file-name))))
    ;; write current buffer to temp file
    (let ((coding-system-for-write 'binary)
          (contents (buffer-string)))
      (with-temp-file filename
        (set-buffer-multibyte nil)
        (encode-coding-string contents 'utf-8 nil (current-buffer))))
    ;; run cmake-format on temp file
    (let ((old-point (point))
          (replacement (with-temp-buffer
                         (when (eq (apply #'call-process
                                          (append `(,cmake-format-executable nil t nil ,filename)
                                                  config-arg)) 0)
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

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

(defun cmake-format-command-line (filename)
  (concat cmake-format-executable " -"
          (if (> (length cmake-format-config-file) 0)
              (concat " -c " (locate-dominating-file filename cmake-format-config-file)
                       cmake-format-config-file)
            "")))

;;;###autoload
(defun cmake-format-buffer ()
  "Use cmake-format to format the current buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (cmake-format-command-line (buffer-file-name))
                           (current-buffer) t))

;;;###autoload
(defalias 'cmake-format 'cmake-format-buffer)

(provide 'cmake-format)

;;; cmake-format.el ends here

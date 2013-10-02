(defun dec-to-hex-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "0123456789")
    (or (looking-at "[0123456789]+")
        (error "No number at point"))
    (replace-match (dec-to-hex (match-string 0)))))

(defun dec-to-hex (numstring &optional prefix)
  (concat (if prefix prefix "0x")
          (format "%x" (string-to-number numstring))))

(defun hex-to-dec (numstring)
  (format "%d" (string-to-number numstring 16)))

(defun hex-to-dec-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "#x0123456789abcdefABCDEF")
    (or (looking-at "\\(0x\\|#x\\|16#\\)\\([0-9a-fA-F]+\\)")
        (error "No number at point"))
    (replace-match (hex-to-dec (match-string 2)))))

(defun toggle-dec-to-hex-at-point ()
  (interactive)
  (condition-case nil
      (hex-to-dec-at-point)
    (error (dec-to-hex-at-point))))

(global-set-key "\C-ch" 'toggle-dec-to-hex-at-point)

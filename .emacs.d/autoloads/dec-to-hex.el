;; dec to hex conversion

(defun dec-to-hex-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+")
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

;; fourcc conversion

(defun fourcc-to-hex-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "0-9a-zA-Z")
    (or (looking-at "[0-9a-zA-Z]\\{1,4\\}\\b")
        (error "No fourcc at point"))
    (replace-match (fourcc-to-hex (match-string 0)) t)))

(defun fourcc-to-hex (fourccstring)
  (concat "0x"
          (mapconcat (lambda (x) (format "%x" x))
                     (mapcar 'string-to-char
                             (mapcar 'string fourccstring))
                     "")))

(defun reverse-string (s)
  (apply 'string (reverse (string-to-list s))))

(defun collect-bytes-to-fourcc (l)
  (cond ((null l) "")
         ((= (length l) 1) (char-to-string (car l)))
         (t (concat (char-to-string (+ (car l) (* 16 (cadr l))))
                    (collect-bytes-to-fourcc (cddr l))))))

(defun hex-to-fourcc (hexstring)
  (reverse-string
   (collect-bytes-to-fourcc
    (mapcar (lambda (x) (string-to-number x 16))
            (reverse (cddr (mapcar 'char-to-string hexstring)))))))

(defun hex-to-fourcc-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "x0123456789abcdefABCDEF")
    (or (looking-at "0x[0-9a-fA-F]\\{1,8\\}\\b")
        (error "No fourcc hex at point"))
    (replace-match (hex-to-fourcc (match-string 0)) t)))

(defun dec-to-fourcc-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+\\b")
        (error "No number at point"))
    (replace-match (hex-to-fourcc (dec-to-hex (match-string 0))) t)))

(defun toggle-fourcc-to-hex-at-point ()
  (interactive)
  (condition-case nil
      (hex-to-fourcc-at-point)
    (error (condition-case nil
                (fourcc-to-hex-at-point)
              (error (dec-to-fourcc-at-point))))))

(global-set-key "\C-cf" 'toggle-fourcc-to-hex-at-point)

;; endian swap at point

(defun string-to-pairs (s)
  (cond ((= (length s) 0) nil)
        ((= (length s) 1) (list s))
        (t (cons (substring s 0 2) (string-to-pairs (substring s 2))))))

(defun endian-swap (hexstring)
  (cond ((= (mod (length hexstring) 2) 0)
         (mapconcat 'identity (reverse (string-to-pairs hexstring)) ""))
        (t (mapconcat 'reverse-string (string-to-pairs (reverse-string hexstring)) ""))))

(defun endian-swap-hex-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "x0123456789abcdefABCDEF")
    (or (looking-at "0x[0-9a-fA-F]\\{1,16\\}\\b")
        (error "No hex at point"))
    (replace-match (concat "0x" (endian-swap (substring (match-string 0) 2))))))

(defun endian-swap-dec-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+")
        (error "No number at point"))
    (replace-match (hex-to-dec (endian-swap (dec-to-hex (match-string 0) ""))) t)))

(defun toggle-endianness-at-point ()
  (interactive)
  (condition-case nil
      (endian-swap-hex-at-point)
    (error (endian-swap-dec-at-point))))

(global-set-key "\C-ce" 'toggle-endianness-at-point)

;; number-to-timestamp at point

; use calc for big numbers
(require 'calc-ext)

(defun hex-to-time (hexstring)
  "Convert a hex string into an Elisp time."
  ;; Could do with some asserts to check byte-list
  (let ((calc-num (concat "16#" hexstring)))
    (list
     (calc-eval
      "rsh(and(idiv($,1000000),16#ffff0000),16)"
      'rawnum
      calc-num)
     (calc-eval
      "and(idiv($,1000000),16#ffff)"
      'rawnum
      calc-num))))

(defun usec-to-ts-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+")
        (error "No number at point"))
    (let ((replacement (usec-to-ts (match-string 0))))
      (and (looking-at "[0-9]+")
           (replace-match replacement)))))

(defun usec-to-ts (numstring)
  (let ((hexstring (format "%x" (string-to-number numstring))))
    (current-time-string (hex-to-time hexstring))))

(global-set-key "\C-ct" 'usec-to-ts-at-point)

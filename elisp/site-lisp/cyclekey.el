;;; cyclekey.el --- Quickly cycle through diacritics at point  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Shankar Rao

;; Author: Shankar Rao <shankar.rao@gmail.com>
;; URL: https://github.com/shankar2k/cyclekey
;; Version: 0.2
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, wp, i18n, diacritic, accent

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides the function cyclekey-cycle, which for the character
;; at point cycles through relevant diacritics and accents.
;;
;; See documentation on https://github.com/shankar2k/cyclekey/.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; History:

;; Version 0.2 (2023-12-08):

;; - Added punctuation support

;; Version 0.1 (2023-12-02):

;; - Initial version

;;; Code:

;;;; Requirements

;;;; Customization

(defcustom cyclekey-languages
  nil
  "List of languages that Cyclekey uses to generate character cycles."
  :type '(repeat string))

(defcustom cyclekey-marks-alist
  (purecopy
   '(("Afrikaans" "aáä" "eéèêë" "iíîï" "oóôö" "uúûü" "yý" "AÁÄ" "EÉÈÊË"
      "IÍÎÏ" "OÓÔÖ" "UÚÛÜ" "YÝ")
     ("Albanian" "cç" "eë" "CÇ" "EË" "\"„“")
     ("Bosnian" "cčć" "dđ" "sš" "zž" "CČĆ" "DĐ" "SŠ" "ZŽ" "\"„”")
     ("Catalan" "aà" "cç" "eéè" "iíï" "oóò" "uúü" "AÀ" "CÇ" "EÉÈ" "IÍÏ"
      "OÓÒ" "UÚÜ" "\"«»")
     ("Croatian" "cčć" "dđ" "sš" "zž" "CČĆ" "DĐ" "SŠ" "ZŽ" "\"„“" "'‚‘")
     ("Czech" "aá" "cč" "dď" "eéĕ" "ií" "nň" "oó" "rř" "sš" "tť" "uúů" "yý"
      "zž" "AÁ" "CČ" "DĎ" "EÉĚ" "IÍ" "NŇ" "OÓ" "RŘ" "SŠ" "TŤ" "UÚŮ" "YÝ"
      "ZŽ" "\"„”" "'‚‘")
     ("Danish" "aæå" "oø" "AÆÅ" "OØ" "\"»«" "'›‹")
     ("Dutch" "aáä" "eéë" "iíï" "oóö" "uúü" "AÁÄ" "EÉË" "IÍÏ" "OÓÖ" "UÚÜ")
     ("Estonian" "aä" "oõö" "sš" "uü" "zž" "AÄ" "OÕÖ" "SŠ" "UÜ" "ZŽ" "\"„”")
     ("French" "aàáâæ" "cç" "eéèêë" "iîï" "oôœ" "uùûü" "yÿ" "AÀÁÆ" "CÇ"
      "EÉÈÊË" "IÎÏ" "OÔŒ" "UÙÛÜ" "YŸ" "\"«»")
     ("German" "aä" "oö" "sß" "uü" "AÄ" "OÖ" "Sß" "UÜ" "\"„“»«" "'›‹‚‘")
     ("Hungarian" "aá" "eé" "ií" "oóöő" "uúüű" "AÁ" "EÉ" "IÍ" "OÓÖŐ" "UÚÜŰ"
      "\"„»«")
     ("Icelandic" "aáæ" "dđ" "eé" "ií" "oóö" "uú" "tþ" "yý" "AÄ" "DÐ" "EÉ"
      "IÍ" "OÓÖ" "UÚ" "TÞ" "YÝ" "\"„“«»" "'‚‘")
     ("Irish" "aá" "eé" "iıí" "oó" "uú" "AÁ" "EÉ" "IÍ" "OÓ" "UÚ" "&⁊")
     ("Italian" "aà" "eéè" "iì" "oóò" "uù" "AÀ" "EÉÈ" "IÌ" "OÓÒ" "UÙ" "\"«»")
     ("Hawaiian" "aā" "eē" "iī" "oō" "uū" "AĀ" "EĒ" "IĪ" "OŌ" "UŪ")
     ("Latvian" "aā" "cč" "eē" "gģ" "iī" "kķ" "lļ" "nņ" "sš" "uū" "zž" "AĀ"
      "CČ" "EĒ" "GĢ" "IĪ" "KĶ" "LĻ" "NŅ" "SŠ" "UŪ" "ZŽ" "\"„“")
     ("Lithuanian" "aą" "cč" "eęė" "iį" "sš" "uųū" "zž" "AĄ" "CČ" "EĘĖ" "IĮ"
      "SŠ" "UŲŪ" "ZŽ" "\"„”" "'‚‘")
     ("Luxembourgish" "aä" "eëé" "AÄ" "EËÉ" "\"„“«»")
     ("Maltese" "cċ" "gġ" "hħ" "zż" "CĊ" "GĠ" "HĦ" "ZŻ")
     ("Montenegrin" "cčć" "dđ" "sšś" "zžź" "CČĆ" "DĐ" "SŠŚ" "ZŽŹ")
     ("Norwegian" "aæå" "eéèê" "oóòôø" "uù" "AÆÅ" "EÉÈÊ" "OÓÒÔØ" "UÙ" "\"« »")
     ("Polish" "aą" "cć" "eę" "lł" "nń" "oó" "sś" "zźż" "AĄ" "CĆ" "EĘ" "LŁ"
      "NŃ" "OÓ" "SŚ" "ZŹŻ" "\"„”«»" "'‚’")
     ("Portuguese" "aáâãà" "cç" "eéê" "ií" "oóôõ" "uú" "AÁÂÃÀ" "CÇ" "EÉÊ"
      "IÍ" "OÓÔÕ" "UÚ" "\"«»")
     ("Romanian" "aăâ" "iî" "sș" "tț" "AĂÂ" "IÎ" "SȘ" "TȚ" "\"„”«»")
     ("Scottish" "aà" "eè" "iì" "oò" "uù" "AÀ" "EÈ" "IÌ" "OÒ" "UÙ")
     ("Serbian" "cčć" "dđ" "sš" "zž" "CČĆ" "DĐ" "SŠ" "ZŽ" "\"„")
     ("Slovak" "aáä" "cč" "dď" "eé" "ií" "lĺľ" "nň" "oóô" "rŕ" "sš" "tť"
      "uú" "yý" "zž" "AÁÄ" "CČ" "DĎ" "EÉ" "IÍ" "LĹĽ" "NŇ" "OÓÔ" "RŔ" "SŠ"
      "TŤ" "UÚ" "YÝ" "ZŽ" "\"„”" "'‚‘")
     ("Slovenian" "cč" "sš" "zž" "CČ" "SŠ" "ZŽ" "\"„”" "'‚‘")
     ("Spanish" "aá" "eé" "ií" "nñ" "oó" "uúü" "yý" "AÁ" "EÉ" "IÍ" "NÑ" "OÓ"
      "UÚÜ" "YÝ" "?¿" "!¡" "\"«»")
     ("Swedish" "aåä" "oö" "AÅÄ" "OÖ" "\"”")
     ("Turkish" "cç" "gğ" "iı" "oö" "sş" "uü" "CÇ" "GĢ" "Iİ" "OÖ" "SŞ" "UÜ")
     ("Turkmen" "aä" "cç" "nň" "oö" "sş" "uü" "yý" "zž" "AÄ" "CÇ" "NŇ" "OÖ"
      "SŞ" "UÜ" "YÝ" "ZŽ")
     ("Welsh" "aâàáä" "eêèéë" "iîìíï" "oôòóö" "uûùúü" "wŵẁẃẅ" "yŷỳýÿ" "AÂÀÁÄ"
      "EÊÈÉË" "IÎÌÍÏ" "OÔÒÓÖ" "UÛÙÚÜ" "WŴẀẂẄ" "YŶỲÝŸ")))
  "Alist mapping languages to lists of character cycles.

Each key is a string corresponding to a known language that uses
a latin alphabet.

Each value is a list of character cycles, where each character
cycle is a string with the first character an English letter or
punctuation mark, and all subsequent characters are variants
marked with various diacritics."
  :type '(alist :key-type string :value-type (repeat string)))

(defcustom cyclekey-save-languages t
  "When true, save ``cyclekey-languages'' for future sessions
whenever it is modified by either ``cyclekey-add-language'' or
``cyclekey-remove-language''."
  :type 'boolean)

;;;; Variables

(defvar cyclekey-forward-map (make-hash-table)
  "Hash table mapping each character to the next diacritic in the cycle.")

(defvar cyclekey-backward-map (make-hash-table)
  "Hash table mapping each character to the previous diacritic in the cycle.")

(defvar cyclekey-full-map (make-hash-table)
  "Hash table mapping each base character to the cycle of all its marks.")

;;;; Functions

(defun cyclekey-init ()
  "Initialize the forward and backwards Cyclekey maps.

This should be called whenever ``cyclekey-languages'' or
``cyclekey-marks-alist'' are modified."
  (clrhash cyclekey-forward-map)
  (clrhash cyclekey-backward-map)
  (clrhash cyclekey-full-map)
  (dolist (lang cyclekey-languages)
    (dolist (cycle (alist-get lang cyclekey-marks-alist nil nil #'equal))
      (let ((first (aref cycle 0)))
        (seq-doseq (next (substring cycle 1))
          (unless (gethash next cyclekey-forward-map)
            (let ((last (gethash first cyclekey-backward-map first)))
              (puthash next first cyclekey-forward-map)
              (puthash last next cyclekey-forward-map)
              (puthash next last cyclekey-backward-map)
              (puthash first next cyclekey-backward-map)))))))
  (maphash #'cyclekey--make-full-cycle cyclekey-forward-map))

(defun cyclekey--make-full-cycle (key val)
  "Add cycle of marks for KEY to ``cyclekey-full-map''.

VAL, the character KEY maps forward to, is included to enable use
with ``maphash''."
  (let ((next val)
        (cycle nil))
    (while (/= next key)
      (setq cycle (concat cycle (list next))
            next (gethash next cyclekey-forward-map)))
    (puthash key cycle cyclekey-full-map)))

(defun cyclekey--is-english-punctuation (ch)
  "Return t if CH is an English punctuation character."
  (and (<= ?! ch ?~)         ; is printable ASCII <= 127
       (not (<= ?0 ch ?9))   ; is not digit
       (not (<= ?A ch ?Z))   ; is not capital letter
       (not (<= ?a ch ?z)))) ; is not lowercase letter

;;;; Commands

;;;###autoload
(defun cyclekey-cycle (backward?)
  "Cycle the character at point to the next diacritic in the Cyclekey map.

This command does nothing if the character at point is not
present in the Cyclekey map. If ``backward?'' is non-nil, then
cycle the character at point to the previous diacritic in the
Cyclekey map."
  (interactive "P")
  (when-let ((ch (gethash (preceding-char)
                          (if backward?
                              cyclekey-backward-map
                            cyclekey-forward-map))))
    (delete-char -1)
    (insert ch)))

;;;###autoload
(defun cyclekey-help ()
  "Show how ``cyclekey-cycle'' cycles through marks for various characters."
  (interactive)
  (with-output-to-temp-buffer "*Cyclekey Help*"    
    (princ "Cyclekey Help\n=============\n\n")
    (princ "Cyclekey Languages\n------------------\n")
    (princ (string-join cyclekey-languages ", "))
    (princ "\n\nCyclekey cycling\n----------------\n")
    (maphash (lambda (ch cycle)
               (when (and cycle (or (<= ?a ch ?z)
                                    (cyclekey--is-english-punctuation ch)))
                 (princ
                  (format "    %c ---> %-19s%s\n" ch cycle
                          (if-let* ((upch (and (<= ?a ch ?z) (upcase ch)))
                                    (upcyc (gethash upch cyclekey-full-map)))
                              (format "%c --> %s" upch upcyc)
                            "")))))
             cyclekey-full-map))
  (with-current-buffer "*Cyclekey Help*"
    (setq truncate-lines t)))

;;;###autoload
(defun cyclekey-add-language (lang)
  "Add marks from LANG to the character cycles in ``cyclekey-cycle''."
  (interactive (list (completing-read "Language to add to Cyclekey: "
                                      cyclekey-marks-alist)))
  (unless (member lang cyclekey-languages)
    (setq cyclekey-languages (append cyclekey-languages (list lang)))
    (when cyclekey-save-languages
      (customize-save-variable 'cyclekey-languages cyclekey-languages))
    (cyclekey-init)
    (when (called-interactively-p 'any)
      (message "Marks from %s added to Cyclekey." lang))))

;;;###autoload
(defun cyclekey-remove-language (lang)
  "Remove marks from LANG from the character cycles in ``cyclekey-cycle''."
  (interactive (list (completing-read "Language to remove from Cyclekey: "
                                      cyclekey-languages)))
  (setq cyclekey-languages (remove lang cyclekey-languages))
  (when cyclekey-save-languages
    (customize-save-variable 'cyclekey-languages cyclekey-languages))
  (cyclekey-init)
  (when (called-interactively-p 'any)
    (message "Marks from %s removed from Cyclekey." lang)))
  

;;;; Footer

(provide 'cyclekey)

;;; cyclekey.el ends here

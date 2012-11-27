;;; ffap-include-start.el --- recognise C #include when at start of line

;; Copyright 2007, 2009 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 4
;; Keywords: files
;; URL: http://user42.tuxfamily.org/ffap-include-start/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-include-start.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ffap-include-start.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; M-x ffap normally only recognises a "#include <foo.h>" when point is
;; within the foo.h filename part.  This tiny spot of code lets it work when
;; point is on the #include part, including at the start of a #include line.

;;; Install:

;; Put ffap-include-start.el in one of your `load-path' directories and the
;; following in your .emacs
;;
;;     (eval-after-load "ffap" '(require 'ffap-include-start))

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - GPLv3
;; Version 3 - set region for ffap-highlight
;; Version 4 - allow missing closing quote, don't extend across newline


;;; Code:

;;;###autoload (eval-after-load "ffap" '(require 'ffap-include-start))

(defadvice ffap-string-at-point (around ffap-include-start activate)
  "Recognise \"#include <foo.h>\" with point anywhere in the directive."
  (require 'thingatpt)
  (if (thing-at-point-looking-at "#[ \t]*include[ \t]+[\"<]\\([^\">\n]+\\)\\([\">]\\|$\\)")
      (progn
        (setq ffap-string-at-point-region (list (match-beginning 1)
                                                (match-end 1)))
        (setq ad-return-value (match-string 1)))
    ad-do-it))

(provide 'ffap-include-start)

;;; ffap-include-start.el ends here

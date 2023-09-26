;;; tspew.el --- Clean and format "template spew" errors from gcc and Clang  -*- lexical-binding: t; -*-

;; Author: Jeff Trull <edaskel@att.net>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(require 'compile)
(require 'cc-mode)
(require 'cl-lib)

(defgroup tspew nil
  "Display C++ compilation results more cleanly.
Suggested usage: (add-hook 'compilation-mode-hook 'tspew-mode)
"
  :group 'compilation :group 'programming)

;; the beginnings of tspew-mode
;; first, a syntax table for error messages
(defvar tspew-syntax-table (make-syntax-table)
  "Syntax table for lexing compiler errors" )
;; modify to suit our needs
;; left and right angle brackets are a kind of parentheses in type names
(modify-syntax-entry ?< "(>" tspew-syntax-table)
(modify-syntax-entry ?> ")<" tspew-syntax-table)

;; colon is a "symbol constituent" - usable in identifiers
(modify-syntax-entry ?: "_" tspew-syntax-table)

;; now we can use (with-symbol-table tspew-syntax-table (movement-fn))

;; we need a grammar for several reasons:
;; 1) to resolve the fact that angle brackets may appear in operator overloads
;; 2) for iterating over template parameters I want "XXX<YYY>" to be a single item
;;    e.g. <int, std::allocator<int>> should be an sexp of length 2, not 3,
;;    with elements "int" and "std::allocator<int>"
;;    - although maybe this is OK? We have to stop before emitting "<" anyway

(defvar tspew-indent-level c-basic-offset
  "Indentation amount for types in error messages")

(defvar tspew-default-fill-width 100
  "Default maximum width of error type display, in columns
If the compilation window is visible, its width will be used instead")

;; captured initial column width
(defvar tspew--fill-width nil
  "Max width in columns for current run")

(defvar tspew-quoted-region-regexp
  ;; in gcc, types and function signatures are enclosed by Unicode left and right single quotes
  ;; in clang it's ASCII single quotes
  (let ((quote-start-regexp "\\(\u2018\\|'\\)")
        (quote-end-regexp "\\(\u2019\\|'\\)")
        ;; some surprising things can be in type names
        (char-lit-regexp "\\('[^']'\\)")
        (allowed-char-regexp "[][[:alnum:]:()<>,&_ =+/*%^.;{}-]"))
    (concat quote-start-regexp "\\(" char-lit-regexp "\\|" allowed-char-regexp "\\)+" quote-end-regexp))
  "Regexp for identifying type and function names (typically quoted)
within an error message")

;; remember where we are in the buffer
;; the compilation filter may give us partial lines, so we have to keep track of how far
;; we've come
(defvar-local tspew--parse-start nil
  "Starting point for incremental error parsing." )

;; define key shortcuts for expand/contract of formatted expressions
(defvar-keymap tspew-mode-map
  :doc "Keymap for the tspew compilation minor mode"
  "C-c +" #'tspew-increase-detail
  "C-c -" #'tspew-decrease-detail
  )

(add-to-list 'minor-mode-map-alist `(tspew-mode . ,tspew-mode-map))

(defun tspew--remove-overlays ()
  (let ((overlays (seq-filter (lambda (ov) (overlay-get ov 'is-tspew))
                              (overlays-in (point-min) (point-max)))))
    (dolist (ov overlays)
      (delete-overlay ov)))
  )

(defun tspew--parse-initialize (_proc)
  "Reset compilation output processing"

  (let ((win (get-buffer-window)))
    (setq-local tspew--fill-width
                (if win (window-body-width win) tspew-default-fill-width)))

  (tspew--remove-overlays)
  (setq tspew--parse-start nil)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)  ;; so we can special-case character syntax
  )

;; create a compilation filter hook to incrementally parse errors
(defun tspew--compilation-filter ()
  "Transform error messages into something prettier."
  ;; Parse from tspew--parse-start to point, or as close as you can get,
  ;; updating tspew--parse-start past the last newline we got.
  ;; Be sure to use "markers" when necessary, as positions are strictly
  ;; buffer offsets and are not "stable" in the iterator sense
  (if (not tspew--parse-start)
      (setq-local tspew--parse-start compilation-filter-start))

  ;; ensure things like "operator()" are considered a single symbol,
  ;; not a symbol followed by parens. The same is true of anonymous classes
  ;; and lambdas, both of which have printed representations containing parens.
  (tspew--mark-special-case-symbols tspew--parse-start (point))

  (while (and (< tspew--parse-start (point))
              (> (count-lines tspew--parse-start (point)) 1))
    ;; we have at least one newline in our working region
    (save-excursion
      (goto-char tspew--parse-start)
      (forward-line)
      (let ((line-end-marker (point-marker)))
        ;; process a single line
        (tspew--handle-line tspew--parse-start line-end-marker)
        (setq-local tspew--parse-start (marker-position line-end-marker))))))

;; TSpew is a minor mode for compilation buffers, not source code
;; To use it you need to enable it after a compilation buffer is created,
;; and they are not created until compilation begins. So you must tell
;; compilation-mode to do it for you using compilation-mode-hook.
;; For example:
;; (add-hook 'compilation-mode-hook 'tspew-mode)
;; will enable tspew for all compiles. You may prefer to restrict it to
;; certain projects instead by writing your own hook.

(define-minor-mode tspew-mode
  "Toggle tspew (Template Spew) mode"
  :init-value nil
  :lighter "TSpew"
  (if tspew-mode
      (progn
        (add-hook 'compilation-start-hook 'tspew--parse-initialize nil t)
        (add-hook 'compilation-filter-hook 'tspew--compilation-filter nil t))
    ;; if we are being toggled off, remove hooks
    (remove-hook 'compilation-start-hook 'tspew--parse-initialize)
    (remove-hook 'compilation-filter-hook 'tspew--compilation-filter)
    ;; overlays too
    (tspew--remove-overlays)
    (kill-local-variable 'tspew--parse-start)))


;; NEW (as of 8/4/2023) plan:
;; Don't bother with start points
;; We have functions for each production in the grammar that return an endpoint (found!) or nil.
;; We make functions like "optional" and "alternative" that wrap them.
;; Each parser only handles internal whitespace. We AND them implicitly.
;; These low-level parsers will not interact with the fill/indent mechanism, so no printer parameter
;; We will gather higher-level objects ("chunks") and then submit them to that facility

;; point updated only on successful parse

;; A lightweight parser formalism
;; A Parser returns t and updates point if successful and returns nil otherwise

;;
;; parser combinators
;;

;; some combinators for use in defining higher-level structures
;; these accept parsers and make new parsers from them

(defun tspew--parser-optional (p)
  "Create a parser that optionally parses its argument (i.e. ignores any failure)"
  (lambda () (or (funcall p) t)))

(defmacro tspew--parser-alternative (&rest parsers)
  "Create a parser that attempts to parse one of several input parsers,
passing if any do"
  `(lambda ()
     (let ((start (point)))
       ;; this whole thing is a macro because of this - "or" is not a function, so we cannot "apply" it.
       ;; instead we build the expression through a macro
       (if (or ,@(mapcar (lambda (p) (list 'funcall p)) parsers))
           t
         (goto-char start)
         nil))))

(defmacro tspew--parser-sequential (&rest parsers)
  "Create a parser that attempts to parse a series of input parsers in sequence,
passing if all do"
  `(lambda ()
     (let ((start (point)))
       (if (and ,@(mapcar (lambda (p) (list 'funcall p)) parsers))
           t
         (goto-char start)
         nil))))

(defun tspew--parser-multiple (p)
  "Create a parser that parses one or more of the input parser, greedily.
To parse zero or more, combine with tspew--parser-optional"
  (lambda ()
    (and (funcall p)            ;; at least one
         (progn
           (while (funcall p))  ;; run until one fails
           t))))                ;; and return true

(defun tspew--parser-alternating (p1 p2)
  "Create a parser that parses p1 and p2 alternately,
passing if at least one parser matches:
p1 [p2 [p1 [p2 [p1 ...]]]]"
  (lambda ()
    (and (funcall p1)
         (progn
           (while (and (funcall p2) (funcall p1)))
           t))))

;; syntactic sugar for combinators - a grammar constructor
;; implementation first
(defun tspew--parser-grammar-expand (grammar)
  "Recursive function implementation of tspew--parser-grammar.
You can call this to see the exact form produced by the grammar, pre-expansion"
  (if (not grammar)
      '(lambda () t)     ;; pass and consume no input
    (cond
     ((stringp grammar)
      `(tspew--parser-keyword ,grammar))    ;; interpret as keyword
     ((not (listp grammar))
      grammar)                              ;; unknown, just pass through (future extension?)
     ((or (equal (car grammar) 'function)   ;; the name of a function (parser, most likely)
          (and (symbolp (car grammar))      ;; a parser generator
               (string-prefix-p "tspew--parser-" (symbol-name (car grammar)))))
          grammar)                          ;; existing parser - just pass it through
     (t (cl-case (car grammar)
        (- (cl-assert (equal (length grammar) 2))
           `(tspew--parser-optional ,(tspew--parser-grammar-expand (cadr grammar))))
        (| `(tspew--parser-alternative
             ,@(mapcar (lambda (p) (tspew--parser-grammar-expand p)) (cdr grammar))))
        (+ (cl-assert (equal (length grammar) 2))
           `(tspew--parser-multiple ,(tspew--parser-grammar-expand (cadr grammar))))
        (* (cl-assert (equal (length grammar) 2))
           `(tspew--parser-optional
             (tspew--parser-multiple ,(tspew--parser-grammar-expand (cadr grammar)))))
        (<> (cl-assert (equal (length grammar) 3))
            `(tspew--parser-alternating ,(tspew--parser-grammar-expand (cadr grammar))
                                        ,(tspew--parser-grammar-expand (caddr grammar))))
        (t ;; it's a list, so it's the default - a sequential parser
         `(tspew--parser-sequential ,@(mapcar (lambda (p) (tspew--parser-grammar-expand p)) grammar)))))
)))

;; then the user interface
(defmacro tspew--parser-grammar (grammar)
  "Create a parser from combinators using shorthand.
An sexp that starts with:
- becomes optional
| becomes alternative
+ becomes multiple (at least one)
* becomes optional multiple (zero or more)
<> becomes alternating (at least the first, then second first second...)
Without one of those initial symbols, the inputs are considered to be
a sequential parser."
  (tspew--parser-grammar-expand grammar))

;; low-level (leaf) parsers

(defun tspew--parse-symbol ()
  "Parse a symbol (a string of characters with word or \"symbol constituent\" syntax)"
  (let ((start (point)))
    ;; skip forward past word and symbol constituents
    ;; forward-symbol skips initial whitespace also, which I don't want
    (or (> (skip-syntax-forward "w_") 0)
        (progn
          (goto-char start)
          nil))))

(defun tspew--parse-cv ()
  "Parse the const or volatile keywords"
  (and (or (looking-at-p "const\\s ") (looking-at "volatile\\s "))
       (progn
         (skip-syntax-forward "w")
         (skip-syntax-forward " ")
         t)))

;; mandatory whitespace
(defun tspew--parse-whitespace ()
  "Parse one or more whitespace characters"
  (let ((start (point)))
    (or (> (skip-syntax-forward " ") 0)
        (progn
          (goto-char start)
          nil))))

(defun tspew--parse-template-preamble ()
  "Parse the initial template<class X, class Y...>
in function template specializations"
  (and (looking-at-p "template<")
       (progn
         (forward-word 1)
         (forward-sexp 1)
         (skip-syntax-forward " ")
         t)))

(defun tspew--parse-with-clause ()
  "Parse a type elaboration for function template instantiations
of the form \"[with X = Y; Q = R; ...]\""
  (and (looking-at-p "\\[with ")
       (progn
         (forward-sexp)
         t)))

(defun tspew--parse-ref-modifier ()
  "Parse a pointer, ref, or rvalue ref"
  (and (looking-at-p "\\*\\|&\\|&&")
       (progn
         (skip-syntax-forward "_")
         t)))

(defun tspew--parse-sequence ()
  "Parse a curly braced sequence (i.e. of types or integers)"
  (and (equal (char-after) ?{)
       (progn
         (forward-sexp)
         t)))

;;
;; parser generators (take a param, return a parser)
;;

;; here we will use "parser" in the name to indicate that result is a parser,
;; so you have to funcall to use it. You can also use the result in a parser
;; combinator (see below)

;; parenthesized expression using the given start character
(defun tspew--parser-paren-expr (parenc)
  "Parse a balanced parenthesis expression starting with
the given opening character"
  (lambda ()
    (and (equal (char-after) parenc)
         (progn
           (forward-sexp)    ;; this could theoretically fail but again, this is compiler output...
           t))))

;; a specific string
(defun tspew--parser-keyword (kwd)
  "Create a parser for a pre-selected keyword.
It requires - and consumes - trailing whitespace"
  (lambda ()
    (and (looking-at-p (concat kwd "\\s "))   ;; trailing whitespace required
         (progn
           (forward-char (length kwd))
           (skip-syntax-forward " ")
           t))))

(defun tspew--parser-memfn-qual ()
  "Parse a member function qualifier, consuming trailing whitespace"
  (tspew--parser-alternative
   (tspew--parser-keyword "const")
   (tspew--parser-keyword "volatile")
   (tspew--parser-keyword "&&")
   (tspew--parser-keyword "&")))

;;
;; parser utilities
;;

;; composed, higher-level parsers

(defun tspew--parse-func-name ()
  "Parse a function name (the bit between the return type
and the open paren of the arguments)"
  ;; for the moment, assume we can use a "type" which is similar, possibly identical
  (tspew--parse-type))

(defun tspew--parse-param-list ()
  "Parse a comma-separated function parameter list as seen
in compiler error messages"
  (forward-sexp)
  )

(defun tspew--parser-builtin-int-type ()
  "Parse a builtin C++ integral type (int/char with modifiers),
with trailing whitespace"
  (tspew--parser-grammar
   (| ( (- "unsigned") "char")
      ( (* (| "long" "short" "unsigned")) "int"))))

(defun tspew--parse-type ()
  "Parse a type as found in compiler error messages"
  ;; either a parenthesized decltype expression, OR
  ;; cv qualifier, followed by symbol, followed optionally
  ;; by a parenthesized expression (angle brackets), followed
  ;; optionally by a symbol (member types, pointer/ref indicators, etc.)
  ;; type := decltype '(' expr ')' | [ cv ] symbol [ sexp [ symbol ] ] ] [ & | && | * ]
  ;; e.g.     const std::vector<double>::iterator
  ;;          ^- cv ^-symbol   ^- sexp ^-symbol
  (funcall (tspew--parser-grammar
            (|
             ;; decltype expression
             ( (- "constexpr") "decltype" (tspew--parser-paren-expr ?\())

             ;; normal types
             (
              ;; first, we can have const/constexpr/volatile
              (* (| "constexpr" #'tspew--parse-cv))
              ;; then one of two things:
              (| (tspew--parser-builtin-int-type)     ;; a builtin int of some kind
                 ( (- (| "typename" "auto" "struct")) ;; a user-defined type (or float or double, which look like types)
                   #'tspew--parse-symbol
                   (- ( (tspew--parser-paren-expr ?<)
                        (- #'tspew--parse-symbol)))))
              ;; either of the above can have reference modifiers
              (- ( (- #'tspew--parse-whitespace) #'tspew--parse-ref-modifier)))))))

(defun tspew--parse-function ()
  "Parse a function signature, as found in compiler error messages"
  ;; func := [ constexpr ] [ static ] type func-name param-list [memfn-qual] [ with-clause ]
  (funcall (tspew--parser-grammar
            ( (- #'tspew--parse-template-preamble)
              ;; BOZO actually not sure which of these keywords will appear first
              (- "constexpr")
              (- "static")
              ;; return type is optional because it could be a constructor
              (- ( #'tspew--parse-type #'tspew--parse-whitespace))
              #'tspew--parse-func-name
              (|
               ;; gcc, and clang sometimes
               ( (tspew--parser-paren-expr ?\()
                 ;; we can have child classes with function call operators here,
                 ;; which themselves can have child classes, and so on
                 ;; gcc seems to format them like types but clang puts the arg lists in parens
                 (- (<> #'tspew--parse-type (tspew--parser-paren-expr ?\()))
                 (- ( #'tspew--parse-whitespace (- (tspew--parser-memfn-qual)) (- #'tspew--parse-with-clause))))

               ;; clang's special function template specialization format (no "with" clause, no param list)
               ;; ::fname<T, U...> vs
               ;; ::fname(T, U...) [with T = X, U = Y...] in gcc
               (tspew--parser-paren-expr ?<))))))

;; here I try to implement a two-part pretty-printing system (that is,
;; both indentation and "fill") as described in a paper by Rose and Welsh
;; (1981), which is paywalled, but there is a nice description of it and
;; related work in "the PretzelBook", see
;; http://www.literateprogramming.com/pretzelbook.pdf
;; There were apparently many similar approaches in the late 70s

;; The scanner is implemented in the literature (such as it is) as a parser
;; that supplies different things to the printer depending on the place
;; you are in the AST. Having only an ad-hoc scanner, I have ad-hoc code for this.

;; the "scanner" (front end) part of the system
(defun tspew--scan (printer)
  "Scan tokens, supplying length information to the back end"
  (with-syntax-table tspew-syntax-table
    ;; tokenize
    (let* ((start (point))
           (syntax (char-syntax (char-after)))
           (tok-range
            ;; "consume" (by moving point) and return next token
            (cl-case syntax
              (?.
               ;; punctuation is passed straight through along with trailing whitespace
               (skip-syntax-forward ".")
               ;; skip trailing whitespace
               (skip-syntax-forward " ")
               (cons start (point)))
              (?\(
               ;; supply just the open "paren" (of whatever type)
               (forward-char)
               (cons start (point)))
              (?\)
               (forward-char)
               ;; closing "paren" may be followed by whitespace
               ;; consume it *if* followed immediately by another closing paren
               (when (and (not (eobp)) (equal (char-syntax (char-after)) ?\s))
                 (skip-syntax-forward " ")
                 (when (not (equal (char-syntax (char-after)) ?\)))
                   ;; NOT another close paren. supply whitespace as next token.
                   (skip-syntax-backward " ")))
               (cons start (+ start 1)))
              (?\s
               ;; whitespace not following punctuation or closing paren
               ;; preserve for readability
               (skip-syntax-forward " ")
               (cons start (point)))
              (t
               ;; grab the next sexp
               (forward-sexp)
               (cons start (point)))))
           (tok (buffer-substring (car tok-range) (cdr tok-range))))

      ;; send token to indent/fill engine
      (funcall printer tok-range)

      ;; optionally send some control information
      ;; we send three kinds:
      ;; "internal break" - a spot to put a newline in between sequential elements, if needed
      ;; "enter hierarchy" - a parenthesized expression begins of specified length
      ;; "exit hierarchy" - a parenthesized expression ends

      (cond
       ((string-match-p "^[,;]\s" tok)
        (funcall printer 'intbrk))    ;; optional newline between elements

       ((equal (char-syntax (string-to-char tok)) ?\()
        ;; we just entered a parenthesized expression
        (funcall printer
                 (cons
                  'enter                   ;; beginning of new hierarchy level
                  (tspew--visible-distance (point)
                                           (save-excursion
                                             (backward-char)       ;; start at open "paren"
                                             (forward-sexp)        ;; skip over balanced parens
                                             (point))))))

       ((equal (char-syntax (string-to-char tok)) ?\))
        (funcall printer 'exit))))))  ;; exit hierarchy level

;; the "printer" (back end)

(defun tspew--printer (initial-indent)
  "Return a closure to \"print\" tokens while maintaining appropriate indentation"
  (let
      ((indentation-stack `((no-break . ,initial-indent)))    ;; current indent level info
       ;; Each element is a dotted pair of:
       ;; 1) the current indentation level in columns
       ;; 2) whether we are splitting the elements of this level one per line
       (space-remaining (- tspew--fill-width initial-indent)) ;; tracking horizontal space
       (prev-tok-end nil)                                     ;; tracking "insertion point"
       (format-instructions '()))                             ;; accumulated changes

    (lambda (cmd)

      ;; the printer maintains the current indentation level and decides when it's
      ;; necessary to start putting out sequence elements on separate lines.
      ;; It maintains a stack of ('brksym . indent) pairs giving for each level
      ;; what the amount of indentation is and whether we are currently breaking
      ;; up the sequence with newlines
      (cl-typecase cmd
        (cons
         (if (equal (car cmd) 'enter)
             ;; an "enter" - push mode for this level
             (let ((len (cdr cmd))
                   (indentation (cdar indentation-stack)))
               (if (or (< len space-remaining)
                       (equal len 1))   ;; trivial (empty) parens
                   ;; there is room enough to print the rest of this sexp
                   ;; don't require line breaks
                   (push (cons 'no-break indentation) indentation-stack)
                 ;; we must switch to a new line to maximize available space
                 (setq indentation (+ indentation tspew-indent-level))
                 ;; new space remaining: whatever is left after indentation
                 (setq space-remaining (- tspew--fill-width indentation))
                 ;; require elements at this level to break/indent
                 (push (cons 'break indentation) indentation-stack)
                 ;; output line break and indent
                 (push (cons prev-tok-end indentation) format-instructions)))

           ;; not an "enter" command, but a plain token
           ;; represented as a range in the buffer
           (cl-assert (and (integerp (car cmd)) (integerp (cdr cmd))))
           (setq prev-tok-end (cdr cmd))
           (setq space-remaining (- space-remaining
                                    (tspew--visible-distance (car cmd) (cdr cmd))))))

        (symbol
         (cl-case cmd

           (result format-instructions) ;; return result

           (exit
            (pop indentation-stack))    ;; restore previous indentation

           (intbrk
            (when (equal (caar indentation-stack) 'break)
              ;; we have a sequence element and previously decided to split one per line
              ;; break and indent to current level (for a new sequence element)
              (setq space-remaining (- tspew--fill-width (cdar indentation-stack)))
              (push (cons prev-tok-end (cdar indentation-stack)) format-instructions)))))))))

(defun tspew--format-region (start end &optional initial-indent)
  "Fill and indent region containing text.
This is the primary engine for the formatting algorithm"

  (let ((printer (tspew--printer (or initial-indent 0))))

    ;; send one token at a time, inserting indentation and line breaks as required
    (save-excursion
      (goto-char start)
      (while (not (equal (point) end))
        (cl-assert (<= (point) end))
        (tspew--scan printer)))

    (funcall printer 'result)))

(defun tspew--format-with-clause (start end)
  "Fill and indent region containing a with clause"

  ;; the semicolon-separated list inside the with clause looks OK when formatted using the type code
  (save-excursion
    (let* ((start (+ start 6))    ;; "[with "
           (end (- end 1))        ;; directly before "]"
           (result (list (cons start 0)))  ;; a single newline after "[with "
           (parse-rhs
            (tspew--parser-alternative #'tspew--parse-sequence #'tspew--parse-type))
           (parse-with-stmt-tparam
            (lambda ()
              (when (looking-at "\\.\\.\\.") (forward-char 3))  ;; skip ellipses, which looks like punctuation
              (tspew--parse-type)))
           (tparam
            (progn
              (goto-char start)
              (funcall (tspew--parser-builtin-int-type))        ;; this might be an integral NTTP
              (funcall parse-with-stmt-tparam)                  ;; skip name
              (buffer-substring start (point)))))

      ;; do first X = Y pair
      (forward-char 3)   ;; skip " = "
      (setq result
            (append result
                    (tspew--format-region
                     (point)
                     (progn (funcall parse-rhs) (point))
                     (+ (length tparam) 3))))
      (while (not (equal (point) end))
        (cl-assert (equal (char-after) ?\;))
        (forward-char)
        (push (cons (+ (point) 1) 0) result)     ;; a newline after every "; "

        (skip-syntax-forward " ")
        (setq tparam
              (buffer-substring (point)
                                (progn
                                  (funcall (tspew--parser-builtin-int-type))
                                  (funcall parse-with-stmt-tparam)
                                  (point))))

        (forward-char 3)     ;; " = "
        (setq result
              (append result
                      (tspew--format-region
                       (point)
                       (progn (funcall parse-rhs) (point))
                       (+ (length tparam) 3)))))
      (forward-char)
      result)))  ;; skip trailing right bracket

(defun tspew--format-function-region (start end)
  "Fill and indent region containing a function"

  ;; Detect (via existing parsers) the different chunks of a function
  ;; then dispatch formatters (such as format-region) as appropriate
  (save-excursion
    (goto-char start)
    (append

     ;; template<class X, class Y...> if present
     (if (looking-at-p "template<")
         (let ((result (tspew--format-template-preamble (point) (progn (tspew--parse-template-preamble) (point)))))
           (skip-syntax-forward " ")
           result)
       '())

     ;; constexpr and/or static (both optional)
     (if (looking-at-p "constexpr ")
         (progn
           (forward-word)
           (skip-syntax-forward " ")
           '())
       '())

     (if (looking-at-p "static ")
         (progn
           (forward-word)
           (skip-syntax-forward " ")
           '())
       '())

     ;; return type
     ;; might be absent if function is a constructor
     ;; you can distinguish it from the function name because it is followed by whitespace, not '('
     ;; this may also happen when the function is an "auto" template
     (let ((tstart (point))
           (tend (progn (tspew--parse-type) (point))))
       (if (equal (char-after) ?\()      ;; must be constructor name
           (progn
             (goto-char tstart)
             '())
         (skip-syntax-forward " ")       ;; consume whitespace
         (append
          (tspew--format-region tstart tend)
          (list (cons (point) 0)))))     ;; newline between return type and function name

     ;; even the function name can require formatting
     (append
      (tspew--format-region (point) (progn (tspew--parse-func-name) (point)))
      (list (cons (point) 0)))

     ;; at this point we could end with a clang-style function template specialization
     (if-let ((fun-spl-end (save-excursion (and (funcall (tspew--parser-paren-expr ?<)) (point)))))
         (tspew--format-region (point) fun-spl-end)

       ;; otherwise it's the gcc possibilities: parameters, member function qualifiers, with clause
       (append
        (tspew--format-region (point) (progn  (tspew--parse-param-list) (point)))

        ;; the param list may be followed by (no whitespace) "::" and a type, also requiring formatting
        ;; and then optionally another param list, and optionally another type, and...
        (if (looking-at "::")
            (let ((result '()))
              (while (looking-at "::")
                (setq result
                      (append result
                              (list (cons (point) 0))
                              (tspew--format-region (point) (progn (tspew--parse-type) (point)))))
                (if (equal (char-after) ?\()
                    (setq result
                          (append result
                                  (list (cons (point) 0))
                                  (tspew--format-region (point) (progn (funcall (tspew--parser-paren-expr ?\()) (point)))))))
              result)
          '())

        ;; skip trailing space and memfn qual, if present
        (if (< (point) end)
            (progn
              (skip-syntax-forward " ")
              (funcall (tspew--parser-memfn-qual))
              '())
          '())

        (if (< (point) end)
            (let ((wc-start (progn (skip-syntax-forward " ") (point))))
              (if (tspew--parse-with-clause)
                  (append
                   ;; newline before with clause, if present
                   (list (cons wc-start 0))
                   (tspew--format-with-clause wc-start (point)))
                '()))
          '()))))))

;; newlines in between these:
;; 1) output static if present
;; 2) format return type
;; 3) if func-name has template args, format individually
;;    otherwise, format it as a unit with the param list
;; 4) if with-clause present, format it

(defun tspew--format-quoted-expr (tstart tend)
  "Split up and indent a quoted region within an error message as necessary
to meet line width requirements"
  ;; At the moment we handle types or function names (as in "required from" lines)
  ;; We check to see which one we have. Types are simple. For functions, we break them up into chunks
  ;; separated by whitespace, like "return type" "function parameter list" or "with clause"
  ;; and format those separately using the indent/fill algorithm
  (with-restriction tstart tend
    (with-syntax-table tspew-syntax-table
      (save-excursion
        (let ((result (list (cons tstart 0))))   ;; initial newline
          (goto-char tstart)
          (cond
           ((tspew--parse-function)
            (when (not (equal (point) tend))
              (message "found a function: |%s| but it does not fill the quoted expression |%s|"
                       (buffer-substring tstart (point))
                       (buffer-substring tstart tend)))
            (append result (tspew--format-function-region tstart (point))))

           ((tspew--parse-type)
            (when (not (equal (point) tend))
              (message "found a type: |%s| but it does not fill the quoted expression |%s|"
                       (buffer-substring tstart (point))
                       (buffer-substring tstart tend)))
            (append result (tspew--format-region tstart (point))))

           (t
            (message (format "Found a quoted expression I don't understand: |%s|"
                             (buffer-substring tstart tend)))
            nil)))))))

(defun tspew--format-template-preamble (tstart tend)
  "Format a function template preamble e.g. template<class X, class Y, class Z>"

  (save-excursion
    (goto-char tstart)
    (forward-word)  ;; skip "template"
    (cl-assert (equal (char-after) ?<))
    (append
     (tspew--format-region (point) tend)
     (list (cons tend 0)))))   ;; terminal newline

(defun tspew--mark-special-case-symbols (start end)
  "Mark various tricky elements so they are considered \"symbol constituents\"
This includes operator overloads, lambdas, and anonymous classes"
  (let ((opr-regexp "operator\\(<<\\|<\\|>>\\|>\\|()\\|\\[]\\)")
        (anon-class-regexp "(anonymous class)\\|{anonymous}")
        (lambda-clang-regexp "(lambda at [[:alnum:]_/.-]+:[0-9]+:[0-9]+)")
        (lambda-gcc-regexp "<lambda([^)]+)>"))
    (save-excursion
      (goto-char start)
      (while (re-search-forward
              (concat "\\(" opr-regexp "\\)\\|\\(" anon-class-regexp "\\)\\|\\(" lambda-clang-regexp "\\)\\|\\(" lambda-gcc-regexp "\\)")
              end t)
        (with-silent-modifications
          (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (string-to-syntax "_")))))))

;; contents can be functions, function specializations, maybe other things?
(defun tspew--handle-quoted-expr (tstart tend)
  "Fill and indent a single quoted expression (type or function)
within an error message"
  ;; create an overlay covering the expression
  (let ((instructions (tspew--format-quoted-expr tstart tend)))

    (when (equal 0 (length instructions))
      (message "no instructions produced for region |%s|" (buffer-substring tstart tend)))
    (dolist (instr instructions)
      (let* ((istart (car instr))
             (indentation (cdr instr))
             (ov (make-overlay istart istart)))
        ;; display indented and filled types in place of the original
        (overlay-put ov 'before-string (concat "\n" (make-string indentation ?\s)))
        ;; remember overlay
        ;; [JET] I initially kept a list of overlays and used that, but compilation-mode
        ;; calls kill-all-local-variables, which deletes the buffer-local value
        ;; of my list. So instead, we use properties:
        (overlay-put ov 'is-tspew t)))))

(defun tspew--handle-line (lstart lend)
  "Process a single line of error output"
  ;; lstart is a position, lend is a marker
  ;; is this an error message with a type?
  (let* ((err-regexp (cadr (assoc 'gnu compilation-error-regexp-alist-alist))))
    (save-excursion
      (goto-char lstart)
      (if (and (looking-at-p err-regexp)  ;; error line
               ;; ignore static asserts (too complex! plus, what would we do with them?)
               (not (save-excursion (re-search-forward "static_assert\\|static assertion" lend t)))
               ;; the line is too long
               (>= (- (line-end-position) (line-beginning-position)) tspew--fill-width))
          ;; while there is still a match remaining in the line:
          (while (re-search-forward tspew-quoted-region-regexp lend t)
            (let ((tstart (+ (match-beginning 0) 1))
                  (tend (- (match-end 0) 1)))
              ;; process this region
              (tspew--handle-quoted-expr tstart tend)
              ;; mark region with depths within parentheses (or angle brackets)
              (with-syntax-table tspew-syntax-table
                (tspew--mark-depths tstart tend))
              ;; advance past matched text
              (goto-char (+ tend 1))))))))

;;
;; depth-based folding support
;;

(defun tspew--mark-depths (start end)
  "Mark regions of text inside parentheses/angle brackets
with their depths, as an overlay property"
  (save-excursion
    (goto-char start)
    (let ((pos-stack nil)
          (max-depth 0))
      (while (not (equal (point) end))
        (cl-case (char-syntax (char-after))
          (?\(
           (push (+ (point) 1) pos-stack))
          (?\)
           (let ((ov (make-overlay (car pos-stack) (point))))
             (overlay-put ov 'tspew-depth (length pos-stack))
             (setq max-depth (max (length pos-stack) max-depth))
             (overlay-put ov 'is-tspew t)
             (pop pos-stack)))
          (t nil))
        (forward-char 1))

      ;; create an overlay recording the maximum depth encountered
      (let ((ov (make-overlay start end)))
        (overlay-put ov 'tspew-max-depth (+ max-depth 1))
        (overlay-put ov 'is-tspew t)))))

(defun tspew--fold-to-depth (start end level)
  "Hide text regions with depth >= level.
When level is nil, all regions are made visible"
  (dolist
      (ov (overlays-in start end))
    (when-let ((depth (overlay-get ov 'tspew-depth)))
      ;; regions with depth > N are contained within regions of depth N
      ;; therefore we need only hide those exactly at "level"
      (if (and level (equal depth level))
          (progn
            (overlay-put ov 'invisible t)
            (overlay-put ov 'before-string "..."))
        (overlay-put ov 'invisible nil)
        (overlay-put ov 'before-string nil)))))

(defun tspew-decrease-detail ()
  "Hide (or \"fold\") the lowest levels of hierarchy in a tspew-formatted region.
Each time you use this command one additional level is hidden."
  (interactive)

  (if-let* ((ov (seq-find (lambda (o) (overlay-get o 'tspew-max-depth))
                          (overlays-in (point) (+ (point) 1))))
            (max-depth (overlay-get ov 'tspew-max-depth)))
      (progn
        (if-let ((depth (overlay-get ov 'tspew-current-depth)))
            (overlay-put ov 'tspew-current-depth (max (- depth 1) 1))
          ;; create a tspew-current-depth property from max-depth
          (overlay-put ov 'tspew-current-depth (max (- (overlay-get ov 'tspew-max-depth) 1) 0)))
        (message "hiding depth %d and higher" (overlay-get ov 'tspew-current-depth))
        (tspew-fold (overlay-get ov 'tspew-current-depth)))
    (error "no formatted region found")))

(defun tspew-increase-detail ()
  "Expose more of the lower levels of hierarchy in a tspew-formatted region.
Each time you use this command one additional level is revealed."
  (interactive)

  (if-let* ((ov (seq-find (lambda (o) (overlay-get o 'tspew-max-depth))
                          (overlays-in (point) (+ (point) 1))))
            (max-depth (overlay-get ov 'tspew-max-depth)))
      (progn
        (if-let ((depth (overlay-get ov 'tspew-current-depth)))
            (overlay-put ov 'tspew-current-depth
                         (min (+ depth 1) (overlay-get ov 'tspew-max-depth)))
          (overlay-put ov 'tspew-current-depth (overlay-get ov 'tspew-max-depth)))
        (message "hiding depth %d and higher" (overlay-get ov 'tspew-current-depth))
        (tspew-fold (overlay-get ov 'tspew-current-depth)))
    (error "no formatted region found")))

(defun tspew--quoted-range-at (pos)
  "Return the range, including quotes, within which pos is found.
Returns nil if pos is not within a quoted range."
  (save-excursion
    (beginning-of-line)
    (let ((end (save-excursion (end-of-line) (point)))
          (rstart nil)
          (rend nil))
      (while (re-search-forward tspew-quoted-region-regexp end t)
        (when (and (>= pos (match-beginning 0)) (< pos (match-end 0)))
          (setq rstart (match-beginning 0))
          (setq rend (match-end 0))))
      (if (and rstart rend) (list rstart rend) nil))))

(defun tspew--visible-distance (start end)
  "Return the number of visible characters in the buffer between the
given positions, taking into account overlays with invisible and
before-string properties"
  (cl-assert (> end start))
  ;; thinking about the algorithm
  ;; we start in an unknown place. If at the start of an overlay with before-string, add its length
  ;; if in an invisible overlay, note start point
  ;; go to next overlay transition
  ;; if now visible (or at end), subtract distance from prev pos/start point and mark us as visible again
  ;; if we were visible previously, take no action

  (with-restriction start end
    (save-excursion
      (goto-char start)
      (let* (
             ;; get before-string property, if any, of an invisible overlay that starts here
             ;; we count only the invisible ones, and only at the start points, to avoid double-counting
             ;; if it's visible, the before-string is visually before this point
             ;; if it's not the start point, we've taken it into account already
             (before-string-at-point
              (lambda ()
                (cl-reduce (lambda (x y) (or x y))   ;; reduce with or - but it's a macro so we do this
                           (mapcar (lambda (ov)
                                     (and (equal (overlay-start ov) (point))
                                          (overlay-get ov 'invisible)
                                          (overlay-get ov 'before-string)))
                                   (overlays-in (point) (+ (point) 1)))
                           :initial-value nil)))
             (before (funcall before-string-at-point))
             (prev-invisible (get-char-property start 'invisible))
             (prev-pos start)
             (distance (if before (+ (- end start) (length before)) (- end start))))
        (goto-char (next-overlay-change start))
        (while (< (point) end)
          (when prev-invisible (setq distance (- distance (- (point) prev-pos))))
          (setq prev-invisible (get-char-property (point) 'invisible))
          (setq prev-pos (point))
          (if-let ((before (funcall before-string-at-point)))
              (setq distance (+ distance (length before))))
          (goto-char (next-overlay-change (point))))
        (if prev-invisible (- distance (- (point) prev-pos)) distance)))))

(defun tspew-fold (&optional level)
  "Fold the quoted region containing point to the requested level.
Text at the designated level, or deeper, will be replaced with ellipses.
The value nil will unfold all levels."
  (interactive "P")
  ;; find the quoted region containing point
  (if-let* ((range (tspew--quoted-range-at (point)))
            (start (car range))
            (end (cadr range)))
      (progn
        (tspew--fold-to-depth start end (and level (prefix-numeric-value level)))
        ;; remove indentation overlays from the region in preparation for reformatting
        (dolist (ov (overlays-in start end))
          (when (and (overlay-get ov 'is-tspew)
                     (not (overlay-get ov 'tspew-depth))
                     (not (overlay-get ov 'tspew-max-depth)))
            (delete-overlay ov)))
        ;; and now perform formatting again keeping in mind the folded expressions
        (tspew--handle-quoted-expr (+ start 1) (- end 1)))
    (error "Not inside a quoted region")))

;; BOZO should this be tspew-mode?
(provide 'tspew)
;;; tspew.el ends here

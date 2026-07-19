;;; tests for boost-sml -*- mode: Emacs-lisp; lexical-binding: t ; -*-

;; Copyright (C) 2026 Jeffrey E. Trull

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Code:
(require 'ert)
(require 'boost-sml)

;;; Test infrastructure
(defvar boost-sml-tests--testdata-preamble
  (concat
   "struct tbl {\n"
   "  auto operator()() const noexcept {\n"
   "    using namespace sml;\n"
   "    // clang-format off\n"
   "    return make_transition_table(\n"))

(defvar boost-sml-tests--testdata-postscript
  (concat
   "        );\n"
   "        // clang-format on\n"
   "  }\n"
   "};\n"))

;; alignment test boilerplate
(defmacro boost-sml-tests--define-test (name docstring orig expected &optional var value)
  "Define a single align-table-at test called NAME with DOCSTRING on text ORIG,
expecting EXPECTED. If symbol VAR is present, override it to VALUE for this test only"
  `(ert-deftest ,name () ,docstring
                (with-temp-buffer
                  (c++-mode)
                  (insert boost-sml-tests--testdata-preamble)
                  (insert ,orig)
                  (insert boost-sml-tests--testdata-postscript)
                  (goto-char 1)
                  (search-forward "make_transition_table")
                  (indent-region (point)
                                 (save-excursion (forward-sexp) (point)))
                  ; run align-table-at-point with VAR, if present, overridden
                  (let ,(when var (list (list var value)))
                    (boost-sml/align-table-at (point)))

                  (let ((expected-result
                         (concat boost-sml-tests--testdata-preamble
                                 ,expected
                                 boost-sml-tests--testdata-postscript)))
                    (should (equal (buffer-substring-no-properties (point-min) (point-max)) expected-result))))))

;; note that the SML examples un-indent starting states by 1, but this formatter instead adds an extra space
;; to the non-initial states


;;; Tests:
(boost-sml-tests--define-test
 boost-sml-tests-different-size-actions
 "Align actions of different sizes"
 (concat "    *state<Foo> + event<START> / very_large_function_object_name = state<Bar>,\n"
         "    state<Bar> + event<FINISH> / small_name = X\n")
 (concat "        *state<Foo> + event<START>   / very_large_function_object_name = state<Bar>,\n"
         "         state<Bar> + event<FINISH>  / small_name                      = X\n"))

(boost-sml-tests--define-test
 boost-sml-tests-one-guard
 "Two non-guard transitions aligned with one guarded"
 (concat "    *state<Foo> + event<START> [ should_go_to_bar ] = state<Bar>,\n"
         "   state<Foo> + event<START>  = state<Baz>,\n"
         "    state<Bar> + event<FINISH> / small_name = X\n")
 (concat "        *state<Foo> + event<START> [should_go_to_bar]              = state<Bar>,\n"
         "         state<Foo> + event<START>                                 = state<Baz>,\n"
         "         state<Bar> + event<FINISH>                   / small_name = X\n"))

(boost-sml-tests--define-test
 boost-sml-tests-one-anon
 "Align one anonymous transition with an evented one"
 (concat "    *state<Foo> + event<START> = state<Bar>,\n"
         "    state<Bar> = X\n")
 (concat "        *state<Foo> + event<START>  = state<Bar>,\n"
         "         state<Bar>                 = X\n"))

(boost-sml-tests--define-test
 boost-sml-tests-repro1
 "User reproduction of multiple issues"
 (concat "    *state<ENTER> + event<EVT1> / action1 = state<END>,\n"
         "    *state<ENTER> + event<EVT2>          = state<STATE1>,\n"
         "   state<STATE1> / action1 = state<STATE2>,\n"
         "   state<STATE2> / action2 = state<END>,\n"
         "   state<END>  = X\n")
 (concat "        *state<ENTER>  + event<EVT1>  / action1 = state<END>,\n"
         "        *state<ENTER>  + event<EVT2>            = state<STATE1>,\n"
         "         state<STATE1>                / action1 = state<STATE2>,\n"
         "         state<STATE2>                / action2 = state<END>,\n"
         "         state<END>                             = X\n"))

(boost-sml-tests--define-test
 boost-sml-tests-string-literal-states
 "States using SML string literal"
 (concat "    *\"enter\"_s + event<EVT1> / action1 = \"s1\"_s,\n"
         "   \"s1\"_s + event<EVT2> = \"end\"_s,\n"
         "   \"end\"_s = X\n")
 (concat "        *\"enter\"_s + event<EVT1>  / action1 = \"s1\"_s,\n"
         "         \"s1\"_s    + event<EVT2>            = \"end\"_s,\n"
         "         \"end\"_s                            = X\n"))


(boost-sml-tests--define-test
 boost-sml-tests-guard-with-equal
 "guard lambda with equal sign"
 (concat "    *state<ENTER> + event<EVT1> [ []{ bool result = true; return result; } ] = state<STATE1>,\n"
         "  state<STATE1> = X\n")
 (concat "        *state<ENTER>  + event<EVT1> [[]{ bool result = true; return result; }] = state<STATE1>,\n"
         "         state<STATE1>                                                          = X\n"))

(boost-sml-tests--define-test
 boost-sml-tests-guard-with-equals-and-fwd-slash
 "guard lambda with forward slash and equals signs"
 (concat "    *state<ENTER> + event<EVT1> [ []{ return ( 2 / 2) == 1; } ] = state<STATE1>,\n"
         "  state<STATE1> = X\n")
 (concat "        *state<ENTER>  + event<EVT1> [[]{ return ( 2 / 2) == 1; }] = state<STATE1>,\n"
         "         state<STATE1>                                             = X\n"))

(boost-sml-tests--define-test
 boost-sml-tests-action-with-equals-and-fwd-slash
 "action lambda with forward slash and equals signs"
 (concat "    *state<ENTER> + event<EVT1> / []{ int value = 6 / 3; } = state<STATE1>,\n"
         "    state<STATE1> = X\n")
 (concat "        *state<ENTER>  + event<EVT1>  / []{ int value = 6 / 3; } = state<STATE1>,\n"
         "         state<STATE1>                                           = X\n"))

(boost-sml-tests--define-test
 boost-sml-tests-multiple-actions
 "actions composed with comma operator"
 (concat "   *state<ENTER> + event<EVT1> / ( do_thing_a, do_thing_b) = state<STATE1>,\n"
         "   state<STATE1> = X\n")
 (concat "        *state<ENTER>  + event<EVT1>  / (do_thing_a, do_thing_b) = state<STATE1>,\n"
         "         state<STATE1>                                           = X\n"))


(boost-sml-tests--define-test
 boost-sml-tests-basic
 "Simple pair of transitions to align"
 (concat "    *state<Foo> + event<START> = state<Bar>,\n"
         "      state<Bar> + event<FINISH> = X\n")
 (concat "        *state<Foo> + event<START>   = state<Bar>,\n"
         "         state<Bar> + event<FINISH>  = X\n"))

(boost-sml-tests--define-test
 boost-sml-tests-orthogonal-regions
 "multiple start states, i.e. orthogonal regions"
 (concat "    *state<ENTER1> + event<EVT1> / do_something = state<STATE1>,\n"
         "  *state<ENTER2> + event<EVT2> = state<STATE2>,\n"
         "  state<STATE1> = X,\n"
         "  state<STATE2> / do_something_else = X\n")
 (concat "        *state<ENTER1> + event<EVT1>  / do_something      = state<STATE1>,\n"
         "        *state<ENTER2> + event<EVT2>                      = state<STATE2>,\n"
         "         state<STATE1>                                    = X,\n"
         "         state<STATE2>                / do_something_else = X\n"))

(boost-sml-tests--define-test
 boost-sml-tests-anon-with-guards
 "Two guarded transitions, both anonymous"
 (concat "    *state<Foo>  [ should_go_to_bar ] = state<Bar>,\n"
         "   state<Foo> [should_not_go_to_bar]  = state<Baz>,\n"
         "    state<Bar> + event<FINISH> / small_name = X\n")
 (concat "        *state<Foo>    [should_go_to_bar]                  = state<Bar>,\n"
         "         state<Foo>    [should_not_go_to_bar]              = state<Baz>,\n"
         "         state<Bar> + event<FINISH>           / small_name = X\n"))

(boost-sml-tests--define-test
 boost-sml-tests-anon-with-guards-aligned
 "Two guarded transitions, both anonymous, with guards aligned"
 (concat "    *state<Foo>  [ should_go_to_bar ] = state<Bar>,\n"
         "   state<Foo> [should_not_go_to_bar]  = state<Baz>,\n"
         "    state<Bar> + event<FINISH> / small_name = X\n")
 (concat "        *state<Foo>                 [should_go_to_bar]                  = state<Bar>,\n"
         "         state<Foo>                 [should_not_go_to_bar]              = state<Baz>,\n"
         "         state<Bar> + event<FINISH>                        / small_name = X\n")
; run in a different mode where events and guards are separately aligned in columns
 boost-sml-align-guards-beyond-events t
)

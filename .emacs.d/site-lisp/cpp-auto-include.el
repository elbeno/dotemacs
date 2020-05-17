;;; cpp-auto-include.el --- auto include header file for C++

;; Copyright (C) 2014 by Syohei YOSHIDA
;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/

;; Copyright (C) 2015 by Ben Deane
;; URL: https://github.com/elbeno/

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

;; How to output include lines: the outer %s values are replaced with <> or ""
(defvar cpp-auto-include/std-header-format "#include %s%s%s")

;; How to match include lines: this should match an include produced by
;; cpp-auto-include/std-header-format
(defvar cpp-auto-include/std-header-regexp "^\\s-*#\\s-*include\\s-*\\([<\"]\\)%s[>\"]")

;; When the buffer's file extension is in this list, put "using namespace std;"
;; after the header block
(defvar cpp-auto-include/ensure-using-namespace-std nil)

;; The namespace to use in a "using namespace" directive
(defvar cpp-auto-include/using-namespace "std")

;; When true, ensure standard #include lines use angle brackets (rather than quotes)
(defvar cpp-auto-include/ensure-brackets t)

;; When true, omit headers that are known to be included by others
(defvar cpp-auto-include/minimal-headers nil)

(require 'cl-lib)
(require 'rx)

;; Regexes for how to match things that correspond to each standard header. The
;; section of the standard for each is given in ;; [brackets].
(defvar cpp-auto-include/header-regexp
  `(;; [support.types]
    ("cstddef" ("*")
     ,(rx (and symbol-start
               (or "size_t" "ptrdiff_t" "nullptr_t" "max_align_t" "byte")
               symbol-end)))
    ("cstddef" nil
     ,(rx (and symbol-start
               (or (and "offsetof" (* space) "(")
                   (and "NULL" symbol-end)))))
    ;; [limits.syn]
    ("limits" ("*")
     ,(rx (and symbol-start
               (or (and "numeric_limits" (* space) "<")
                   (and  (or "round_indeterminate"
                             "round_toward_zero"
                             "round_to_nearest"
                             "round_toward_infinity"
                             "round_toward_neg_infinity"
                             "denorm_indeterminate"
                             "denorm_absent"
                             "denorm_present")
                         symbol-end)))))
    ;; [climits.syn]
    ("climits" nil
     ,(rx (and symbol-start
               (or "CHAR_BIT" "CHAR_MAX" "CHAR_MIN"
                   "INT_MIN" "INT_MAX" "LLONG_MAX" "LLONG_MIN"
                   "LONG_MAX" "LONG_MIN" "MB_LEN_MAX"
                   "SCHAR_MIN" "SCHAR_MAX" "SHRT_MAX" "SHRT_MIN"
                   "UCHAR_MAX" "UINT_MAX" "ULLONG_MAX" "ULONG_MAX" "USHRT_MAX")
               symbol-end)))
    ;; [cfloat.syn]
    ("cfloat" nil
     ,(rx (and symbol-start
               (or "FLT_RADIX" "DECIMAL_DIG"
                   "FLT_DECIMAL_DIG" "DBL_DECIMAL_DIG" "LDBL_DECIMAL_DIG"
                   "FLT_MIN" "DBL_MIN" "LDBL_MIN"
                   "FLT_TRUE_MIN" "DBL_TRUE_MIN" "LDBL_TRUE_MIN"
                   "FLT_MAX" "DBL_MAX" "LDBL_MAX"
                   "FLT_EPSILON" "DBL_EPSILON" "LDBL_EPSILON"
                   "FLT_DIG" "DBL_DIG" "LDBL_DIG"
                   "FLT_MANT_DIG" "DBL_MANT_DIG" "LDBL_MANT_DIG"
                   "FLT_MIN_EXP" "DBL_MIN_EXP" "LDBL_MIN_EXP"
                   "FLT_MIN_10_EXP" "DBL_MIN_10_EXP" "LDBL_MIN_10_EXP"
                   "FLT_MAX_EXP" "DBL_MAX_EXP" "LDBL_MAX_EXP"
                   "FLT_MAX_10_EXP" "DBL_MAX_10_EXP" "LDBL_MAX_10_EXP"
                   "FLT_ROUNDS" "FLT_EVAL_METHOD"
                   "FLT_HAS_SUBNORM" "DBL_HAS_SUBNORM" "LDBL_HAS_SUBNORM")
               symbol-end)))
    ;; [cstdint.syn]
    ("cstdint" ("*")
     ,(rx (and symbol-start
               (or (and (zero-or-one "u")
                        "int"
                        (zero-or-one (or "_fast" "_least"))
                        (or "8" "16" "32" "64")
                        "_t")
                   "intmax_t" "intptr_t"
                   "uintmax_t" "uintptr_t")
               symbol-end)))
    ("cstdint" nil
     ,(rx (and symbol-start
               (or (and (zero-or-one "U")
                        "INT"
                        (or "8" "16" "32" "64" "MAX")
                        "_C"
                        (* space) "(")
                   (and (or (and "INT_"
                                 (zero-or-one (or "FAST" "LEAST"))
                                 (or "8" "16" "32" "64")
                                 "_MIN")
                            (and (zero-or-one "U")
                                 "INT_"
                                 (zero-or-one (or "FAST" "LEAST"))
                                 (or "8" "16" "32" "64")
                                 "_MAX")
                            (and "INT"
                                 (or "MAX" "PTR")
                                 "_MIN")
                            (and (zero-or-one "U")
                                 "INT"
                                 (or "MAX" "PTR")
                                 "_MAX")
                            (and (or "PTRDIFF" "SIG_ATOMIC" "WCHAR" "WINT")
                                 (or "_MAX" "_MIN"))
                            "SIZE_MAX")
                        symbol-end)))))
    ;; [support.start.term]
    ("cstdlib" ("*")
     ,(rx (and symbol-start
               (or "_Exit" "at_quick_exit" "quick_exit"
                   "abort" "exit" "atexit")
               (* space) "(")))
    ;; [cstdlib.syn]
    ("cstdlib" nil
     ,(rx (and symbol-start
               (or "EXIT_FAILURE" "EXIT_SUCCESS")
               symbol-end)))
    ;; [support.dynamic]
    ("new" ("*")
     ,(rx (and symbol-start
               (or (and (or "get_new_handler" "set_new_handler")
                        (* space) "(")
                   (and "launder" (* space) (or "(" "<"))
                   (and (or "new_handler"
                            "nothrow" "nothrow_t"
                            "bad_alloc" "bad_array_new_length"
                            "align_val_t" "destroying_delete_t"
                            "hardware_destruct_interference_size"
                            "hardware_constructive_interference_size")
                        symbol-end)))))
    ;; [support.rtti]
    ("typeinfo" ("*")
     ,(rx (and symbol-start
               (or "type_info" "bad_cast" "bad_typeid")
               symbol-end)))
    ;; [support.exception]
    ("exception" ("*")
     ,(rx (and symbol-start
               (or (and (or "get_unexpected" "set_unexpected" "unexpected"
                            "get_terminate" "set_terminate" "terminate"
                            "uncaught_exception" "uncaught_exceptions"
                            "current_exception"
                            "rethrow_exception")
                        (* space) "(")
                   (and (or "make_exception_ptr"
                            "throw_with_nested"
                            "rethrow_if_nested")
                        (* space) (or "(" "<"))
                   (and (or "unexpected_handler" "terminate_handler"
                            "bad_exception" "nested_exception"
                            "exception" "exception_ptr")
                        symbol-end)))))
    ;; [support.initlist]
    ("initializer_list" ("*")
     ,(rx (and symbol-start
               "initializer_list"
               (* space) "<")))
    ;; [support.runtime]
    ("csetjmp" ("*")
     ,(rx (and symbol-start
               (or (and "longjmp"
                        (* space) "(")
                   (and "jmp_buf"
                        symbol-end)))))
    ("csetjmp" nil
     ,(rx (and symbol-start
               "setjmp"
               (* space) "(")))
    ("csignal" ("*")
     ,(rx (and symbol-start
               (or (and (or "signal" "raise")
                        (* space) "(")
                   (and "sig_atomic_t"
                        symbol-end)))))
    ("csignal" nil
     ,(rx (and symbol-start
               "SIG"
               (or "ABRT" "FPE" "ILL"
                   "INT" "SEGV" "TERM"
                   "_DFL" "_IGN" "_ERR")
               symbol-end)))
    ("cstdalign" nil
     ,(rx (and symbol-start
               "__alignas_is_defined"
               symbol-end)))
    ("cstdarg" ("*")
     ,(rx (and symbol-start
               "va_list"
               symbol-end)))
    ("cstdarg" nil
     ,(rx (and symbol-start
               (or "va_start" "va_arg" "va_copy" "va_end")
               (* space) "(")))
    ("cstdbool" nil
     ,(rx (and symbol-start
               "__bool_true_false_are_defined"
               symbol-end)))
    ("cstdlib" ("*")
     ,(rx (and symbol-start
               (or "system" "getenv")
               (* space) "(")))
    ("ctime" ("*")
     ,(rx (and symbol-start
               (or (and (or "clock" "time" "difftime" "timespec_get"
                            "ctime" "asctime" "strftime"
                            "wcsftime" "gmtime" "localtime" "mktime")
                        (* space) "(")
                   (and  (or "clock_t" "time_t" "tm" "timespec")
                         symbol-end)))))
    ("ctime" nil
     ,(rx (and symbol-start
               "CLOCKS_PER_SEC"
               symbol-end)))
    ;; [std.exceptions]
    ("stdexcept" ("*")
     ,(rx (and symbol-start
               (or "logic_error" "domain_error"
                   "invalid_argument" "length_error"
                   "out_of_range" "runtime_error"
                   "range_error" "overflow_error" "underflow_error")
               symbol-end)))
    ;; [assertions]
    ("cassert" nil
     ,(rx (and symbol-start
               "assert"
               (* space) "(")))
    ;; [errno]
    ("cerrno" nil
     ,(rx (and symbol-start
               (or "E2BIG" "EACCES" "EADDRINUSE" "EADDRNOTAVAIL" "EAFNOSUPPORT" "EAGAIN"
                   "EALREADY" "EBADF" "EBADMSG" "EBUSY" "ECANCELED" "ECHILD" "ECONNABORTED"
                   "ECONNREFUSED" "ECONNRESET" "EDEADLK" "EDESTADDRREQ" "EDOM" "EEXIST"
                   "EFAULT" "EFBIG" "EHOSTUNREACH" "EIDRM" "EILSEQ" "EINPROGRESS" "EINTR"
                   "EINVAL" "EIO" "EISCONN" "EISDIR" "ELOOP" "EMFILE" "EMLINK" "EMSGSIZE"
                   "ENAMETOOLONG" "ENETDOWN" "ENETRESET" "ENETUNREACH" "ENFILE" "ENOBUFS"
                   "ENODATA" "ENODEV" "ENOENT" "ENOEXEC" "ENOLCK" "ENOLINK" "ENOMEM" "ENOMSG"
                   "ENOPROTOOPT" "ENOSPC" "ENOSR" "ENOSTR" "ENOSYS" "ENOTCONN" "ENOTDIR"
                   "ENOTEMPTY" "ENOTRECOVERABLE" "ENOTSOCK" "ENOTSUP" "ENOTTY" "ENXIO"
                   "EOPNOTSUPP" "EOVERFLOW" "EOWNERDEAD" "EPERM" "EPIPE" "EPROTO"
                   "EPROTONOSUPPORT" "EPROTOTYPE" "ERANGE" "EROFS" "ESPIPE" "ESRCH" "ETIME"
                   "ETIMEDOUT" "ETXTBSY" "EWOULDBLOCK" "EXDEV" "errno")
               symbol-end)))
    ;; [syserr]
    ("system_error" ("*")
     ,(rx (and symbol-start
               (or  (and (or "generic_category" "system_category"
                             "make_error_code" "make_error_condition")
                         (* space) "(")
                    (and (or "is_error_code_enum" "is_error_condition_enum")
                         (* space) "<")
                    (and (or "error_category" "error_code"
                             "error_condition" "system_error"
                             (and "errc::"
                                  (or
                                   "address_family_not_supported" "address_in_use"
                                   "address_not_available" "already_connected"
                                   "argument_list_too_long" "argument_out_of_domain"
                                   "bad_address" "bad_file_descriptor" "bad_message"
                                   "broken_pipe" "connection_aborted"
                                   "connection_already_in_progress" "connection_refused"
                                   "connection_reset" "cross_device_link"
                                   "destination_address_required" "device_or_resource_busy"
                                   "directory_not_empty" "executable_format_error"
                                   "file_exists" "file_too_large" "filename_too_long"
                                   "function_not_supported" "host_unreachable"
                                   "identifier_removed" "illegal_byte_sequence"
                                   "inappropriate_io_control_operation" "interrupted"
                                   "invalid_argument" "invalid_seek" "io_error"
                                   "is_a_directory" "message_size" "network_down"
                                   "network_reset" "network_unreachable" "no_buffer_space"
                                   "no_child_process" "no_link" "no_lock_available"
                                   "no_message_available" "no_message" "no_protocol_option"
                                   "no_space_on_device" "no_stream_resources"
                                   "no_such_device_or_address" "no_such_device"
                                   "no_such_file_or_directory" "no_such_process"
                                   "not_a_directory" "not_a_socket" "not_a_stream"
                                   "not_connected" "not_enough_memory" "not_supported"
                                   "operation_canceled" "operation_in_progress"
                                   "operation_not_permitted" "operation_not_supported"
                                   "operation_would_block" "owner_dead" "permission_denied"
                                   "protocol_error" "protocol_not_supported"
                                   "read_only_file_system" "resource_deadlock_would_occur"
                                   "resource_unavailable_try_again" "result_out_of_range"
                                   "state_not_recoverable" "stream_timeout" "text_file_busy"
                                   "timed_out" "too_many_files_open_in_system"
                                   "too_many_files_open" "too_many_links"
                                   "too_many_symbolic_link_levels" "value_too_large"
                                   "wrong_protocol_type")))
                         symbol-end)))))
    ;; [utility]
    ("utility" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start "move"
               (* space) "(" (or alpha "_") (* (or alnum "_")) ")")))
    ("utility" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               (or (and (or "swap" "exchange" "move_if_noexcept")
                        (* space) (or "(" "<"))
                   (and (or "forward" "pair" "declval"
                            "integer_sequence" "index_sequence"
                            "make_integer_sequence" "make_index_sequence"
                            "index_sequence_for"
                            "in_place_type_t" "in_place_type"
                            "in_place_index_t" "in_place_index")
                        (* space) "<")
                   (and "pair" (* space) "{")
                   (and (or "make_pair" "to_chars" "from_chars")
                        (* space) "(")
                   (and "chars_format::"
                        (or "scientific" "fixed" "hex" "general")
                        symbol-end)
                   (and  (or "piecewise_construct" "piecewise_construct_t"
                             "in_place" "in_place_t")
                         symbol-end)))))
    ;; [tuple]
    ("tuple" ("*")
     ,(rx (and symbol-start
               (or (and (or "ignore" "apply" "make_from_tuple")
                        (* space) "(")
                   (and (or "make_tuple" "forward_as_tuple"
                            "tie" "tuple_cat")
                        (* space) (or "(" "<"))
                   (and "tuple" (* space) "{")
                   (and (or "tuple" "tuple_size" "tuple_element" "tuple_element_t")
                        (* space) "<")))))
    ;; [any]
    ("any" ("*")
     ,(rx (and symbol-start
               (or (and "any_cast" (* space) "<")
                   "any" "bad_any_cast"))))
    ;; [variant]
    ("variant" ("*")
     ,(rx (and symbol-start
               (or (and "visit" (* space) "(")
                   (and (or "variant"
                            "variant_size" "variant_size_v"
                            "variant_alternative" "variant_alternative_t"
                            "holds_alternative" "get_if")
                        (* space) "<")
                   "monostate" "bad_variant_access" "variant_npos"))))
    ;; [optional]
    ("optional" ("*")
     ,(rx (and symbol-start
               (or (and "make_optional" (* space) "(")
                   (and "optional" (* space) "<")
                   "nullopt" "nullopt_t" "bad_optional_access"))))
    ;; [filesystem]
    ("filesystem" ("filesystem::" "*")
     ,(rx (and symbol-start
               (or (and (or "u8path" "absolute" "canonical" "weakly_canonical"
                            "relative" "proximate" "copy_file" "copy_symlink"
                            "create_directory" "create_directories"
                            "create_hard_link" "create_symlink" "create_directory_symlink"
                            "current_path" "exists" "equivalent"
                            "file_size" "hard_link_count" "last_write_time"
                            "permissions" "read_symlink" "remove_all" "rename"
                            "resize_file" "space" "status" "symlink_status"
                            "temp_directory_path"
                            "is_block_file" "is_character_file"
                            "is_directory" "is_empty"
                            "is_fifo" "is_other"
                            "is_regular_file" "is_socket"
                            "is_symlink" "status_known")
                        (* space) "(")
                   "path" "filesystem_error" "directory_entry"
                   "directory_iterator" "recursive_directory_iterator"
                   "file_status" "space_info" "file_type"
                   "perms" "perm_options" "copy_options"
                   "directory_options" "file_time_type"))))
    ;; [template.bitset]
    ("bitset" ("*") ;; ("string" "iosfwd")
     ,(rx (and symbol-start
               "bitset"
               (* space) "<")))
    ;; [memory.syn]
    ("memory" ("*")
     ,(rx (and symbol-start
               (or (and (or "align" "declare_reachable"
                            "declare_no_pointers" "undeclare_no_pointers"
                            "get_pointer_safety" "addressof"
                            "shared_from_this")
                        (* space) "(")
                   (and (or "undeclare_unreachable"
                            "return_temporary_buffer"
                            "uninitialized_copy" "uninitialized_copy_n"
                            "uninitialized_fill" "uninitialized_fill_n"
                            "uninitialized_move" "uninitialized_move_n"
                            "uninitialized_default_construct"
                            "uninitialized_default_construct_n"
                            "uninitialized_value_construct"
                            "uninitialized_value_construct_n"
                            "destroy_at" "destroy" "destroy_n"
                            "to_address")
                        (* space) (or "(" "<"))
                   (and (or "unique_ptr" "shared_ptr" "weak_ptr"
                            "pointer_traits" "uses_allocator"
                            "allocator" "raw_storage_iterator"
                            "get_temporary_buffer"
                            "default_delete"
                            "make_unique" "make_shared" "allocate_shared"
                            (and (or "static" "dynamic" "const" "reinterpret")
                                 "_pointer_cast")
                            "get_deleter" "owner_less")
                        (* space) "<")
                   (and (or (and "pointer_safety::"
                                 (or "relaxed" "preferred" "strict"))
                            "allocator_arg"
                            "allocator_arg_t"
                            "bad_weak_ptr"
                            "enable_shared_from_this")
                        symbol-end)))))
    ;; [c.malloc]
    ("cstdlib" ("*")
     ,(rx (and symbol-start
               (or "calloc" "free" "malloc" "realloc" "aligned_alloc")
               (* space) "(")))
    ("cstring" ("*")
     ,(rx (and symbol-start
               (or "memchr" "memcmp" "memmove" "memset" "memcpy"
                   "strcpy" "strncpy" "strcat" "strncat" "strxfrm"
                   "strlen" "strcmp" "strncmp" "strcoll" "strchr"
                   "strrchr" "strspn" "strcspn" "strpbrk" "strstr"
                   "strtok" "strerror")
               (* space) "(")))
    ;; [functional]
    ("functional" ("*")
     ,(rx (and symbol-start
               (or (and (or "ref" "cref" "not_fn"
                            "bind" "mem_fn" "invoke")
                        (* space) (or "(" "<"))
                   (and (or "reference_wrapper"
                            "plus" "minus" "multiplies" "divides"
                            "modulus" "negate"
                            "equal_to" "not_equal_to"
                            "greater" "less" "greater_equal" "less_equal"
                            "logical_and" "logical_or" "logical_not"
                            "bit_and" "bit_or" "bit_xor" "bit_not"
                            "is_bind_expression" "is_placeholder"
                            "function" "hash"
                            "default_searcher" "boyer_moore_searcher"
                            "boyer_moore_horspool_searcher")
                        (* space) "<")
                   (and "bad_function_call"
                        symbol-end)))))
    ;; deprecated stuff omitted
    ("functional" ("placeholders::" "*")
     ,(rx (and symbol-start
               (or "_1" "_2" "_3" "_4" "_5" "_6")
               symbol-end)))
    ;; [meta.type.synop]
    ("type_traits" ("*")
     ,(rx (and symbol-start
               (or (and (or "integral_constant" "bool_constant"
                            "alignment_of" "rank" "extent" "void_t"
                            (and (or
                                  "is_void" "is_null_pointer"
                                  "is_integral" "is_floating_point"
                                  "is_array" "is_pointer"
                                  "is_lvalue_reference" "is_rvalue_reference"
                                  "is_member_object_pointer" "is_member_function_pointer"
                                  "is_enum" "is_union" "is_class" "is_function" "is_reference"
                                  "is_arithmetic" "is_fundamental" "is_object" "is_scalar"
                                  "is_compound" "is_member_pointer" "is_const" "is_volatile"
                                  "is_trivial" "is_trivially_copyable" "is_standard_layout"
                                  "is_pod" "is_literal_type" "is_empty" "is_polymorphic"
                                  "is_abstract" "is_final" "is_signed" "is_unsigned"
                                  "is_constructible" "is_default_constructible"
                                  "is_copy_constructible" "is_move_constructible"
                                  "is_assignable" "is_copy_assignable" "is_move_assignable"
                                  "is_destructible" "is_trivially_constructible"
                                  "is_trivially_default_constructible"
                                  "is_trivially_copy_constructible"
                                  "is_trivially_move_constructible" "is_trivially_assignable"
                                  "is_trivially_copy_assignable" "is_trivially_move_assignable"
                                  "is_trivially_destructible" "is_nothrow_constructible"
                                  "is_nothrow_default_constructible" "is_nothrow_copy_constructible"
                                  "is_nothrow_move_constructible" "is_nothrow_assignable"
                                  "is_nothrow_copy_assignable" "is_nothrow_move_assignable"
                                  "is_nothrow_destructible" "has_virtual_destructor"
                                  "is_same" "is_base_of"
                                  "is_convertible" "has_unique_object_representations"
                                  "is_aggregate" "is_swappable_with" "is_swappable"
                                  "is_nothrow_swappable_with" "is_nothrow_swappable"
                                  "is_invocable" "is_invocable_r"
                                  "is_nothrow_invocable" "is_nothrow_invocable_r"
                                  "conjunction" "disjunction" "negation")
                                 (zero-or-one "_v"))
                            (and (or "remove_const" "remove_volatile" "remove_cv"
                                     "add_const" "add_volatile" "add_cv"
                                     "remove_reference" "add_lvalue_reference"
                                     "add_rvalue_reference" "make_signed" "make_unsigned"
                                     "remove_extent" "remove_all_extents" "remove_pointer"
                                     "add_pointer" "aligned_storage" "aligned_union"
                                     "decay" "enable_if" "conditional" "common_type"
                                     "underlying_type" "result_of" "remove_cv_ref"
                                     "invoke_result")
                                 (zero-or-one "_t")))
                        (* space) "<")
                   (and "endian::"
                        (or "little" "big" "native")
                        symbol-end)
                   (and (or "true_type" "false_type")
                        symbol-end)))))
    ;; [ratio.syn]
    ("ratio" ("*")
     ,(rx (and symbol-start
               (or (and "ratio"
                        (zero-or-one (or "_add" "_subtract"
                                         "_multiply" "_divide"
                                         "_equal" "_not_equal"
                                         "_less" "_less_equal"
                                         "_greater" "_greater_equal"))
                        (* space) "<")
                   (and (or "yocto" "zepto" "atto" "femto" "pico"
                            "nano" "micro" "milli" "centi" "deci"
                            "deca" "hecto" "kilo" "mega" "giga"
                            "tera" "peta" "exa" "zetta" "yotta")
                        symbol-end)))))
    ;; [time.syn]
    ("chrono" ("chrono::" "*")
     ,(rx (and symbol-start
               (or (and (or "duration_cast" "time_point_cast" "clock_cast")
                        (* space) (or "(" "<"))
                   (and (or "duration" "time_point"
                            "treat_as_floating_point" "duration_values"
                            "is_clock" "is_clock_v" "clock_time_conversion"
                            "time_of_day")
                        (* space) "<")
                   (and (or "nanoseconds" "microseconds" "milliseconds"
                            "seconds" "minutes" "hours"
                            "system_clock" "steady_clock" "high_resolution_clock"
                            "utc_clock" "tai_clock" "gps_clock" "file_clock" "local_t"
                            "last_spec")
                        symbol-end)))))
    ;; [allocator.adaptor.syn]
    ("scoped_allocator" ("*")
     ,(rx (and symbol-start
               "scoped_allocator_adaptor"
               (* space) "<")))
    ;; [type.index.synopsis]
    ("typeindex" ("*")
     ,(rx (and symbol-start
               "type_index"
               symbol-end)))
    ;; [string.view.synop]
    ("string_view" ("*")
     ,(rx (and symbol-start
               (or (and "basic_string_view"
                        (* space) "<")
                   (and (or "string_view" "wstring_view" "u16string_view" "u32string_view")
                        symbol-end)))))
    ;; [string.classes]
    ("string" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               (or (and (or "to_string" "to_wstring" "getline"
                            "stoi" "stol" "stoul" "stoll" "stoull"
                            "stof" "stod" "stold")
                        (* space) "(")
                   (and (or "char_traits" "basic_string")
                        (* space) "<")
                   (and (or "string" "wstring" "u16string" "u32string")
                        symbol-end)))))
    ;; [c.strings]
    ("cctype" ("*")
     ,(rx (and symbol-start
               (or "isalnum" "tolower" "toupper" "isblank" "isalpha"
                   "iscntrl" "isdigit" "isgraph" "islower" "isprint"
                   "ispunct" "isspace" "isupper" "isxdigit")
               (* space) "(")))
    ("cwctype" ("*")
     ,(rx (and symbol-start
               (or (and (or "iswalnum" "iswalpha" "iswblank" "iswcntrl" "iswctype"
                            "iswdigit" "iswgraph" "iswlower" "iswprint" "iswpunct"
                            "iswspace" "iswupper" "iswxdigit" "towctrans" "towlower"
                            "towupper" "wctrans" "wctype")
                        (* space) "(")
                   (and (or "wctrans_t" "wctype_t" "wint_t")
                        symbol-end)))))
    ("cwctype" nil
     ,(rx (and symbol-start
               "WEOF"
               symbol-end)))
    ("cwchar" ("*")
     ,(rx (and symbol-start
               (or (and (or "btowc" "mbsinit" "vwscanf" "wcsncpy" "wcstoull"
                            "fgetwc" "mbsrtowcs" "wcrtomb" "wcspbrk" "wcstoul"
                            "fgetws" "putwchar" "wcscat" "wcsrchr" "wcsxfrm"
                            "fputwc" "putwc" "wcschr" "wcsrtombs" "wctob"
                            "fputws" "swprintf" "wcscmp" "wcsspn" "wmemchr"
                            "fwide" "swscanf" "wcscoll" "wcsstr" "wmemcmp"
                            "fwprintf" "ungetwc" "wcscpy" "wcstod" "wmemcpy"
                            "fwscanf" "vfwprintf" "wcscspn" "wcstof" "wmemmove"
                            "getwchar" "vfwscanf" "wcsftime" "wcstok" "wmemset"
                            "getwc" "vswprintf" "wcslen" "wcstold" "wprintf"
                            "mbrlen" "vswscanf" "wcsncat" "wcstoll" "wscanf"
                            "mbrtowc" "vwprintf" "wcsncmp" "wcstol")
                        (* space) "(")
                   (and "mbstate_t")
                        symbol-end))))
    ("cwchar" nil
     ,(rx (and symbol-start
               (or "WCHAR_MAX" "WCHAR_MIN")
               symbol-end)))
    ("cstdlib" ("*")
     ,(rx (and symbol-start
               (or "atof" "mblen" "strtof" "strtoul"
                   "atoi" "mbtowc" "strtol" "strtoull"
                   "atol" "mbstowcs" "strtold" "wctomb"
                   "atoll" "strtod" "strtoll" "wcstombs")
               (* space) "(")))
    ("cstdlib" nil
     ,(rx (and symbol-start
               "MB_CUR_MAX"
               symbol-end)))
    ("cuchar" ("*")
     ,(rx (and symbol-start
               (or "mbrtoc16" "c16rtomb" "mbrtoc32" "c32rtomb")
               (* space) "(")))
    ("cuchar" nil
     ,(rx (and symbol-start
               (or "__STDC_UTF_16__" "__STDC_UTF_32__")
               symbol-end)))
    ;; [locale.syn]
    ("locale" ("*")
     ,(rx (and symbol-start
               (or (and (or "use_facet" "has_facet"
                            "wstring_convert"
                            "wbuffer_convert" "ctype" "ctype_byname"
                            "codecvt" "codecvt_byname"
                            "num_get" "num_put"
                            "numpunct" "numpunct_byname"
                            "collate" "collate_byname"
                            "time_get" "time_get_byname" "time_put" "time_put_byname"
                            "money_get" "money_put" "moneypunct" "moneypunct_byname"
                            "messages" "messages_byname")
                        (* space) "<")
                   (and (or "locale" "ctype_base" "codecvt_base" "time_base"
                            "money_base" "messages_base")
                        symbol-end)))))
    ;; [locale.stdcvt]
    ("codecvt" ("*")
     ,(rx (and symbol-start
               (or (and (or "codecvt_utf8"
                            "codecvt_utf16"
                            "codecvt_utf8_utf16")
                        (* space) "<")
                   (and (or "codecvt_mode"
                            "consume_header" "generate_header" "little_endian")
                        symbol-end)))))
    ;; [c.locales]
    ("clocale" ("*")
     ,(rx (and symbol-start
               (or (and (or "localeconv" "setlocale")
                        (* space) "(")
                   (and "lconv"
                        symbol-end)))))
    ("clocale" nil
     ,(rx (and symbol-start
               (or "LC_ALL"
                   "LC_COLLATE" "LC_CTYPE" "LC_TIME"
                   "LC_MONETARY" "LC_NUMERIC")
               symbol-end)))
    ;; [sequences.general]
    ("array" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               "array"
               (* space) (or "<" "{"))))
    ("deque" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               "deque"
               (* space) (or "<" "{"))))
    ("forward_list" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               "forward_list"
               (* space) (or "<" "{"))))
    ("list" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               "list"
               (* space) (or "<" "{"))))
    ("vector" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               "vector"
               (* space) (or "<" "{"))))
    ;; [associative.map.syn]
    ("map" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               (or "map" "multimap")
               (* space) "<")))
    ;; [associative.set.syn]
    ("set" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               (or "set" "multiset")
               (* space) "<")))
    ;; [unord.map.syn]
    ("unordered_map" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               (or "unordered_map" "unordered_multimap")
               (* space) "<")))
    ;; [unord.set.syn]
    ("unordered_set" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               (or "unordered_set" "unordered_multiset")
               (* space) "<")))
    ;; [queue.syn]
    ("queue" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               (or "queue" "priority_queue")
               (* space) (or "<" "{"))))
    ;; [stack.syn]
    ("stack" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               "stack"
               (* space) (or "<" "{"))))
    ;; [iterator.syn]
    ("iterator" ("*")
     ,(rx (and symbol-start
               (or (and (or "iterator_traits" "iterator"
                            "reverse_iterator" "move_iterator"
                            "istream_iterator" "ostream_iterator"
                            "istreambuf_iterator" "ostreambuf_iterator")
                        (* space) "<")
                   (and (or "advance" "distance" "next" "prev"
                            "make_reverse_iterator"
                            "back_inserter"
                            "front_inserter"
                            "inserter"
                            "make_move_iterator"
                            "begin" "end" "cbegin" "cend"
                            "rbegin" "rend" "crbegin" "crend"
                            "size" "empty" "data")
                        (* space) (or "<" "("))
                   (and (or "input_iterator_tag"
                            "output_iterator_tag"
                            "forward_iterator_tag"
                            "bidirectional_iterator_tag"
                            "random_access_iterator_tag")
                        symbol-end)))))
    ;; [algorithms.general]
    ("algorithm" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start "move"
               (* space) "(" (or alpha "_") (* (or alnum "_"))
               (* space) ",")))
    ("algorithm" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
           (or "all_of" "any_of" "none_of"
               "for_each"
               "find" "find_if" "find_if_not"
               "find_end" "find_first_of"
               "adjacent_find"
               "count" "count_if"
               "mismatch" "equal"
               "is_permutation"
               "search" "search_n"
               "copy" "copy_n"
               "copy_if" "copy_backward"
               "move_backward"
               "swap_ranges" "iter_swap"
               "transform"
               "replace" "replace_if"
               "replace_copy" "replace_copy_if"
               "fill" "fill_n"
               "generate" "generate_n"
               "remove" "remove_if"
               "remove_copy" "remove_copy_if"
               "unique" "unique_copy"
               "reverse" "reverse_copy"
               "rotate" "rotate_copy" "shuffle"
               "is_partitioned"
               "partition" "stable_partition"
               "partition_copy" "partition_point"
               "sort" "stable_sort"
               "partial_sort" "partial_sort_copy"
               "is_sorted" "is_sorted_until"
               "nth_element"
               "lower_bound" "upper_bound"
               "equal_range" "binary_search"
               "merge" "inplace_merge"
               "includes"
               "set_union" "set_intersection"
               "set_difference" "set_symmetric_difference"
               "push_heap" "pop_heap"
               "make_heap" "sort_heap"
               "is_heap" "is_heap_until"
               "min" "max" "minmax"
               "min_element" "max_element" "minmax_element"
               "lexicographical_compare"
               "next_permutation" "prev_permutation"
               "clamp" "shift_left" "shift_right"
               "compare_3way" "lexicographical_compare_3way")
           (* space) (or "<" "("))))
    ;; [alg.c.library]
    ("cstdlib" ("*")
     ,(rx (and symbol-start
               (or "bsearch" "qsort")
               (* space) "(")))
    ;; [cfenv.syn]
    ("cfenv" ("*")
     ,(rx (and symbol-start
               (or (and (or "feclearexcept" "fegetexceptflag"
                            "feraiseexcept" "fesetexceptflag" "fetestexcept"
                            "fegetround" "fesetround"
                            "fegetenv" "feholdexcept" "fesetenv" "feupdateenv")
                        (* space) "(")
                   (and (or "fenv_t" "fexcept_t")
                        symbol-end)))))
    ("cfenv" nil
     ,(rx (and symbol-start
               (or "FE_ALL_EXCEPT" "FE_DIVBYZERO"
                   "FE_INEXACT" "FE_INVALID"
                   "FE_OVERFLOW" "FE_UNDERFLOW"
                   "FE_DOWNWARD" "FE_TONEAREST"
                   "FE_TOWARDZERO" "FE_UPWARD"
                   "FE_DFL_ENV")
               symbol-end)))
    ;; [complex.syn]
    ("complex" ("*")
     ,(rx (and symbol-start
               "complex"
               (* space) "<")))
    ;; [rand.synopsis]
    ("random" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               (or (and (or "linear_congruential_engine"
                            "mersenne_twister_engine"
                            "subtract_with_carry_engine"
                            "discard_block_engine"
                            "independent_bits_engine"
                            "shuffle_order_engine"
                            "generate_canonical"
                            "uniform_int_distribution"
                            "uniform_real_distribution"
                            "binomial_distribution"
                            "geometric_distribution"
                            "negative_binomial_distribution"
                            "poisson_distribution"
                            "exponential_distribution"
                            "gamma_distribution"
                            "weibull_distribution"
                            "extreme_value_distribution"
                            "normal_distribution"
                            "lognormal_distribution"
                            "chi_squared_distribution"
                            "cauchy_distribution"
                            "fisher_f_distribution"
                            "student_t_distribution"
                            "discrete_distribution"
                            "piecewise_constant_distribution"
                            "piecewise_linear_distribution")
                        (* space) "<")
                   (and (or "random_device" "seed_seq"
                            "minstd_rand0" "minstd_rand"
                            "mt19937" "mt19937_64"
                            "ranlux24_base" "ranlux48_base"
                            "ranlux24" "ranlux48"
                            "knuth_b"
                            "default_random_engine"
                            "bernoulli_distribution")
                        symbol-end)))))
    ;; [valarray.syn]
    ("valarray" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               (or (and (or "valarray"
                            "slice_array" "gslice_array"
                            "mask_array" "indirect_array")
                        (* space) "<")
                   (and (or "slice" "gslice")
                        symbol-end)))))
    ;; [numeric.ops.overview]
    ("numeric" ("*")
     ,(rx (and symbol-start
               (or "accumulate"
                   "inner_product"
                   "partial_sum"
                   "adjacent_difference"
                   "iota" "reduce"
                   "transform_reduce" "inclusive_scan" "exclusive_scan"
                   "transform_inclusive_scan" "transform_exclusive_scan"
                   "gcd" "lcm")
               (* space) (or "<" "("))))
    ;; [c.math]
    ("cmath" ("*")
     ,(rx (and symbol-start
               (or (and (or "abs" "cosh" "fmod" "logb" "remquo"
                            "acos" "erf" "frexp" "lrint" "rint"
                            "acosh" "erfc" "hypot" "lround" "round"
                            "asin" "exp2" "ilogb" "modf" "scalbln"
                            "asinh" "exp" "ldexp" "nan" "scalbn"
                            "atan" "expm1" "lgamma" "nanf" "sin"
                            "atan2" "fabs" "llrint" "nanl" "sinh"
                            "atanh" "fdim" "llround" "nearbyint" "sqrt"
                            "cbrt" "floor" "log" "nextafter" "tan"
                            "ceil" "fma" "log10" "nexttoward" "tanh"
                            "copysign" "fmax" "log1p" "pow" "tgamma"
                            "cos" "fmin" "log2" "remainder" "trunc"
                            "fpclassify" "isgreaterequal" "islessequal" "isnan" "isunordered"
                            "isfinite" "isinf" "islessgreater" "isnormal" "signbit"
                            "isgreater" "isless")
                        (* space) "(")
                   (and (or "float_t" "double_t")
                        symbol-end)))))
    ("cmath" nil
     ,(rx (and symbol-start
               (or "FP_FAST_FMA" "FP_ILOGBNAN" "FP_SUBNORMAL" "HUGE_VALL" "MATH_ERRNO"
                   "FP_FAST_FMAF" "FP_INFINITE" "FP_ZERO INFINITY" "MATH_ERREXCEPT"
                   "FP_FAST_FMAL" "FP_NAN HUGE_VAL" "NAN" "math_errhandling"
                   "FP_ILOGB0" "FP_NORMAL" "HUGE_VALF")
               symbol-end)))
    ("cmath" nil
     ,(rx (and symbol-start
               (or "assoc_laguerre" "assoc_legendre" "beta" "comp_ellint_1"
                   "comp_ellint_2" "comp_ellint_3" "cyl_bessel_i" "cyl_bessel_j"
                   "cyl_bessel_k" "cyl_neumann" "ellint_1" "ellint_2" "ellint_3"
                   "expint" "hermite" "legendre" "laguerre" "riemann_zeta"
                   "sph_bessel" "sph_legendre" "sph_neumann")
               (zero-or-one (or "f" "l"))
               (* space) "(")))
    ("cstdlib" ("*")
     ,(rx (and symbol-start
               (or (and (or "ldiv" "lldiv"
                            "labs" "llabs"
                            "div" "rand" "srand")
                        (* space) "(")
                   (and (or "div_t" "ldiv_t" "lldiv_t")
                        symbol-end)))))
    ("cstdlib" nil
     ,(rx (and symbol-start
               "RAND_MAX"
               symbol-end)))
    ;; [iostream.objects.overview]
    ("iostream" ("*") ;; ("ios" "streambuf" "istream" "ostream")
     ,(rx (and symbol-start
               (or "cin" "cout" "cerr" "clog"
                   "wcin" "wcout" "wcerr" "wclog")
               symbol-end)))
    ;; [iostreams.base.overview]
    ("ios" ("*") ;; ("iosfwd")
     ,(rx (and symbol-start
               (or (and (or "fpos" "basic_ios")
                        (* space) "<")
                   (and (or "streamoff" "streamsize"
                            "internal" "left" "right"
                            "dec" "hex" "oct"
                            "fixed" "scientific" "hexfloat" "defaultfloat"
                            "io_errc"
                            (and (zero-or-one "no")
                                 (or "boolalpha" "showbase" "showpoint"
                                     "skipws" "uppercase" "unitbuf")))
                        symbol-end)))))
    ;; [stream.buffers.overview]
    ("streambuf" ("*")
     ,(rx (and symbol-start
               (or (and (or "basic_streambuf")
                        (* space) "<")
                   (and (or "streambuf" "wstreambuf")
                        symbol-end)))))
    ;; [iostream.format.overview]
    ("istream" ("*")
     ,(rx (or
           (and (not (in "<[a-zA-Z_0-9]"))
                "iostream"
                symbol-end)
           (and symbol-start
                (or (and (or "basic_istream"
                             "basic_iostream")
                         (* space) "<")
                    (and (or "istream" "wistream" "wiostream" "ws")
                         symbol-end))))))
    ("ostream" ("*")
     ,(rx (and symbol-start
               (or (and (or "basic_ostream")
                        (* space) "<")
                   (and (or "ostream" "wostream" "endl" "ends" "flush")
                        symbol-end)))))
    ("iomanip" ("*")
     ,(rx (and symbol-start
               (or (and (or "resetiosflags" "setiosflags"
                            "setbase" "setprecision" "setw")
                        (* space) "(")
                   (and (or "setfill"
                            "get_money" "put_money"
                            "get_time" "put_time"
                            "quoted")
                        (* space) (or "<" "("))))))
    ;; [string.streams.overview]
    ("sstream" ("*")
     ,(rx (and symbol-start
               (or (and (or "basic_stringbuf"
                            "basic_istringstream"
                            "basic_ostringstream"
                            "basic_stringstream")
                        (* space) "<")
                   (and (or "stringbuf" "wstringbuf"
                            "stringstream" "wstringstream"
                            "istringstream" "wistringstream"
                            "ostringstream" "wostringstream")
                        symbol-end)))))
    ;; [fstreams]
    ("fstream" ("*")
     ,(rx (and symbol-start
               (or (and (or "basic_filebuf"
                            "basic_ifstream"
                            "basic_ofstream"
                            "basic_fstream")
                        (* space) "<")
                   (and (or "filebuf" "wfilebuf"
                            "fstream" "wfstream"
                            "ifstream" "wifstream"
                            "ofstream" "wofstream")
                        symbol-end)))))
    ;; [c.files]
    ("cstdio" ("*")
     ,(rx (and symbol-start
               (or (and (or "clearerr" "fopen" "fsetpos" "putchar" "snprintf" "vscanf"
                            "fclose" "fprintf" "ftell" "puts" "sprintf" "vsnprintf"
                            "feof" "fputc" "fwrite" "remove" "sscanf" "vsprintf"
                            "ferror" "fputs" "getc" "rename" "tmpfile" "vsscanf"
                            "fflush" "fread" "getchar" "rewind" "tmpnam"
                            "fgetc" "freopen" "perror" "scanf" "ungetc"
                            "fgetpos" "fscanf" "printf" "setbuf" "vfprintf"
                            "fgets" "fseek" "putc" "setvbuf" "vprintf")
                        (* space) "(")
                   (and (or "FILE" "fpos_t")
                        symbol-end)))))
    ("cstdio" nil
     ,(rx (and symbol-start
               (or "BUFSIZ" "FOPEN_MAX" "SEEK_CUR" "TMP_MAX" "_IONBF" "stdout"
                   "EOF" "L_tmpnam" "SEEK_END" "_IOFBF" "stderr"
                   "FILENAME_MAX" "SEEK_SET" "_IOLBF" "stdin")
               symbol-end)))
    ("cinttypes" ("*")
     ,(rx (and symbol-start
               (or (and (or "imaxabs" "strtoimax" "wcstoimax"
                            "imaxdiv" "strtoumax" "wcstoumax")
                        (* space) "(")
                   (and "imaxdiv_t"
                        symbol-end)))))
    ("cinttypes" nil
     ,(rx (and symbol-start
               (and (or "PRI" "SCN")
                    (or "d" "i" "o" "u" "x" "X")
                    (or "MAX"
                        "PTR"
                        (and (zero-or-one (or "FAST" "LEAST"))
                             (or "8" "16" "32" "64"))))
               symbol-end)))
    ;; [re.syn]
    ("regex" ("*") ;; ("initializer_list")
     ,(rx (and symbol-start
               (or (and (or "regex_traits"
                            "basic_regex"
                            "sub_match"
                            "match_results"
                            "regex_iterator"
                            "regex_token_iterator")
                        (* space) "<")
                   (and (or "regex_match"
                            "regex_search"
                            "regex_replace")
                        (* space) (or "<" "("))
                   (and (or "error_type" "regex_error"
                            "regex" "wregex"
                            "csub_match" "wcsub_match"
                            "ssub_match" "wssub_match"
                            "cmatch" "wcmatch"
                            "smatch" "wsmatch"
                            "cregex_iterator" "wcregex_iterator"
                            "sregex_iterator" "wsregex_iterator"
                            "cregex_token_iterator" "wcregex_token_iterator"
                            "sregex_token_iterator" "wsregex_token_iterator"
                            "syntax_option_type" "icase" "nosubs" "optimize"
                            "collate" "ECMAScript" "basic" "extended" "awk" "grep" "egrep"
                            "match_flag_type" "match_default"
                            "match_not_bol" "match_not_eol"
                            "match_not_bow" "match_not_eow"
                            "match_any" "match_not_null"
                            "match_continuous" "match_prev_avail"
                            "format_default" "format_sed" "format_no_copy"
                            "error_collate" "error_ctype"
                            "error_escape" "error_backref"
                            "error_brack" "error_paren" "error_brace"
                            "error_badbrace" "error_range" "error_space"
                            "error_badrepeat" "error_complexity" "error_stack")
                        symbol-end)))))
    ;; [atomics.syn]
    ("atomic" ("*")
     ,(rx (and symbol-start
               (or (and (or "kill_dependency"
                            "atomic")
                        (* space) "<")
                   (and (or (and "atomic_flag_"
                                 (or "test_and_set"
                                     "clear")
                                 (zero-or-one "_explicit"))
                            "atomic_thread_fence"
                            "atomic_signal_fence")
                        (* space) "(")
                   (and (or "atomic_is_lock_free"
                            "atomic_init"
                            (and "atomic_"
                                 (or "load"
                                     "store"
                                     "exchange"
                                     "compare_exchange_weak"
                                     "compare_exchange_strong"
                                     "fetch_add"
                                     "fetch_sub"
                                     "fetch_and"
                                     "fetch_or"
                                     "fetch_xor")
                                 (zero-or-one "_explicit")))
                        (* space) (or "<" "("))
                   (and (or "memory_order"
                            "atomic_flag"
                            (and "memory_order_"
                                 (or "relaxed" "consume" "acquire"
                                     "release" "acq_rel" "seq_cst")))
                            (and "atomic_"
                                 (or "char" "schar" "uchar"
                                     "short" "ushort" "int" "uint"
                                     "long" "ulong" "llong" "ullong"
                                     "char16_t" "char32_t" "wchar_t"))
                            (and "atomic_"
                                 (zero-or-one "u")
                                 "int_"
                                 (or "least" "fast")
                                 (or "8" "16" "32" "64")
                                 "_t")
                            "atomic_intptr_t"
                            "atomic_uintptr_t"
                            "atomic_size_t"
                            "atomic_ptrdiff_t"
                            "atomic_intmax_t"
                            "atomic_uintmax_t"
                        symbol-end)))))
    ("atomic" nil
     ,(rx (and symbol-start
               (or (and (or "ATOMIC_VAR_INIT"
                            "ATOMIC_FLAG_INIT")
                        (* space) "(")
                   (and "ATOMIC_"
                        (or "BOOL" "CHAR" "CHAR16_T"
                            "CHAR32_T" "WCHAR_T"
                            "SHORT" "INT" "LONG" "LLONG" "POINTER")
                        "_LOCK_FREE"
                        symbol-end)))))
    ;; [thread.threads]
    ("thread" ("*")
     ,(rx (and symbol-start
               "thread"
               symbol-end)))
    ("thread" ("this_thread::" "*")
     ,(rx (and symbol-start
               (or (and (or "sleep_until" "sleep_for")
                        (* space) (or "<" "("))
                   (and (or "get_id" "yield")
                        (* space) "(")))))
    ;; [thread.mutex]
    ("mutex" ("*")
     ,(rx (and symbol-start
               (or (and (or "lock" "try_lock" "call_once")
                        (* space) (or "<" "("))
                   (and (or "lock_guard" "unique_lock" "scoped_lock")
                        (* space) "<")
                   (and (or "mutex" "recursive_mutex"
                            "timed_mutex" "recursive_timed_mutex"
                            "defer_lock_t" "try_to_lock_t" "adopt_lock_t"
                            "defer_lock" "try_to_lock" "adopt_lock"
                            "once_flag")
                        symbol-end)))))
    ("shared_mutex" ("*")
     ,(rx (and symbol-start
               (or (and "shared_lock"
                        (* space) "<")
                   (and (or "shared_timed_mutex" "shared_mutex")
                        symbol-end)))))
    ("condition_variable" ("*")
     ,(rx (and symbol-start
               (or (and "notify_all_at_thread_exit"
                        (* space) "(")
                   (and (or "condition_variable" "condition_variable_any"
                            (and "cv_status::"
                                 (or "no_timeout"
                                     "timeout")))
                        symbol-end)))))
    ;; [futures.overview]
    ("future" ("*")
     ,(rx (and symbol-start
               (or (and (or "async")
                        (* space) (or "<" "("))
                   (and (or "future_category")
                        (* space) "(")
                   (and (or "promise" "future" "shared_future" "packaged_task")
                        (* space) "<")
                   (and (or "future_error"
                            (and "future_errc::"
                                 (or "broken_promise"
                                     "future_already_retrieved"
                                     "promise_already_satisfied"
                                     "no_state"))
                            (and "launch::"
                                 (or "async"
                                     "deferred"))
                            (and "future_status::"
                                 (or "ready"
                                     "timeout"
                                     "deferred")))
                        symbol-end)))))
    ;; [execution.syn]
    ("execution" ("*")
     ,(rx (and symbol-start
               (and (or "is_execution_policy") (* space) "<"))))
    ("execution" ("execution::" "*")
     ,(rx (and symbol-start
               (or "sequenced_policy" "parallel_policy"
                   "parallel_unsequenced_policy" "unsequenced_policy"
                   "seq" "par" "par_unseq" "unseq")
               symbol-end)))
    ;; [charconv.syn]
    ("charconv" ("*")
     ,(rx (and symbol-start
               (or (and (or "from_chars" "to_chars") (* space) "(")
                   (and "chars_format" symbol-end)))))
    ;; [mem.res.syn]
    ("memory_resource" ("pmr::" "*")
     ,(rx (and symbol-start
               (or (and "polymorphic_allocator" (* space) "<")
                   (and (or "new_delete_resource" "null_memory_resource"
                            "get_default_resource" "set_default_resource")
                        (* space) "(")
                   (and (or "memory_resource" "pool_options" "synchronized_pool_resource"
                            "unsynchronized_pool_resource" "monotonic_buffer_resource")
                        symbol-end)))))
    ))

;; Headers that are included by other headers
(defvar cpp-auto-include/subsumed-headers
  '(("initializer_list" . ("utility" "string"
                           "array" "deque" "forward_list" "list" "vector"
                           "map" "set" "unordered_map" "unordered_set"
                           "queue" "stack"
                           "algorithm" "random" "valarray" "regex"))
    ("string" . ("bitset"))
    ("streambuf" . ("iostream"))
    ("istream" . ("iostream"))
    ("ostream" . ("iostream"))
    ("ios" . ("iostream"))))

(defvar cpp-auto-include/member-access-regexp
  (rx (and (or "." "->") (* space))))

;; Insert a blank line at line, if it's not already blank
(defun cpp-auto-include/ensure-blank-line-at (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (- line 1))
    (unless (re-search-forward "^\\s-*$" (line-end-position) t)
      (insert "\n"))))

;; Find the line where a certain header is included, and whether or not it uses
;; quotes or angle brackets around the include
(defun cpp-auto-include/include-line (header)
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (format cpp-auto-include/std-header-regexp header)))
      (cond ((re-search-forward regexp nil t)
             (cons (line-number-at-pos) (string= "\"" (match-string 1))))
            (t '(nil . nil))))))

;; Is point in a string or comment?
(defsubst cpp-auto-include/in-string-or-comment-p ()
  (nth 8 (syntax-ppss)))

;; Is point really at a match?
(defun cpp-auto-include/false-match ()
  (or (cpp-auto-include/in-string-or-comment-p)
      (save-excursion
        (goto-char (match-beginning 0))
        (looking-back cpp-auto-include/member-access-regexp))))

;; Does the buffer contain a regexp? Start searching from line (so as not to
;; generate false positives on includes) and omit strings and comments.
(defun cpp-auto-include/buffer-has-keyword-p (regexp line)
  (save-excursion
    (goto-char (point-min))
    (when line
      (forward-line line))
    (let (finish)
      (while (and (not finish) (re-search-forward regexp nil t))
        (unless (cpp-auto-include/false-match)
          (setq finish t)))
      finish)))

;; Same as above, but for just one line
(defun cpp-auto-include/line-has-keyword-p (regexp line)
  (save-excursion
    (goto-char (point-min))
    (when line
      (forward-line (- line 1)))
    (let (finish)
      (while (and (not finish) (re-search-forward regexp (line-end-position) t))
        (unless (cpp-auto-include/false-match)
          (setq finish t)))
      finish)))

;; Parse a file: figure out which headers it should include based on the regexp
;; matches from the table.
(defun cpp-auto-include/parse-file ()
  (cl-loop with added = nil
           with removed = nil
           with case-fold-search = nil

           for info in cpp-auto-include/header-regexp
           for header = (nth 0 info)
           for regexp = (nth 2 info)
           for (included-line . uses-quotes) = (cpp-auto-include/include-line header)
           for has-keyword = (cpp-auto-include/buffer-has-keyword-p regexp included-line)

           ;; remove all standard includes, we will replace them in alpha order
           do
           (progn
             (when has-keyword
               (cl-pushnew (cons header uses-quotes) added :test 'equal))
             (when included-line
               (cl-pushnew (cons header included-line) removed :test 'equal)))

           finally
           return (list :added added :removed removed)))

;; Same as above, but just based on one line
(defun cpp-auto-include/parse-line (line)
  (cl-loop with added = nil
           with removed = nil
           with case-fold-search = nil

           for info in cpp-auto-include/header-regexp
           for header = (nth 0 info)
           for regexp = (nth 2 info)
           for (included-line . uses-quotes) = (cpp-auto-include/include-line header)
           for has-keyword = (cpp-auto-include/line-has-keyword-p regexp line)

           ;; remove all standard includes, we will replace them in alpha order
           do
           (progn
             (when has-keyword
               (cl-pushnew (cons header uses-quotes) added :test 'equal))
             (when included-line
               (cl-pushnew (cons header uses-quotes) added :test 'equal)
               (cl-pushnew (cons header included-line) removed :test 'equal)))

           finally
           return (list :added added :removed removed)))

;; Find the insertion point for stl headers: after the last #include in the file
;; or, if none, after #pragma once (if any)
(defun cpp-auto-include/header-insert-point-by-regexp (regexp)
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward regexp nil t)
      (forward-line 1)
      (point))))

(defun cpp-auto-include/header-insert-point ()
  (cl-some 'cpp-auto-include/header-insert-point-by-regexp
           '("^#\\s-*include\\s-*[<\"]" "#pragma once")))

;; Add the headers we need to the file, and the using namespace std; directive
(defun cpp-auto-include/add-headers (headers use-std)
  (save-excursion
    (let ((insert-point (or (cpp-auto-include/header-insert-point) (point-min))))
      (goto-char insert-point)
      (unless (eq (line-number-at-pos) 1)
        (cpp-auto-include/ensure-blank-line-at (line-number-at-pos))
        (forward-line))
      (dolist (h headers)
        (let* ((header (car h))
               (uses-quotes (cdr h))
               (surround-char-left
                (if (and uses-quotes (not cpp-auto-include/ensure-brackets)) "\"" "<"))
               (surround-char-right
                (if (and uses-quotes (not cpp-auto-include/ensure-brackets)) "\"" ">")))
          (insert (format (concat cpp-auto-include/std-header-format "\n")
                          surround-char-left header surround-char-right))))
      (cpp-auto-include/ensure-blank-line-at (line-number-at-pos))
      (forward-line)
      (when use-std
        (insert (concat "using namespace " cpp-auto-include/using-namespace ";\n"))
        (cpp-auto-include/ensure-blank-line-at (line-number-at-pos))))))

;; Remove stl headers from the file
(defun cpp-auto-include/remove-headers (headers)
  (save-excursion
    (cl-loop with deleted-lines = 0
             initially (goto-char (point-min))
             for (header . line) in (sort headers (lambda (a b) (< (cdr a) (cdr b))))
             for curline = 1 then (line-number-at-pos)
             do
             (progn
               (forward-line (- line curline deleted-lines))
               (let ((beg (point)))
                 (forward-line 1)
                 (delete-region beg (point))
                 (cl-incf deleted-lines))))))

;; Is a header superfluous because the list already contains another header that
;; includes it?
(defun cpp-auto-include/header-is-subsumed (h headers)
  (let* ((hname (car h))
         (parents (cdr (assoc hname cpp-auto-include/subsumed-headers))))
    (when parents
      (cl-some (lambda (x) (assoc x headers))
               parents))))

;; Boil down the list of headers to the minimum by eliminating those which are
;; included by others
(defun cpp-auto-include/minimize-headers (headers)
  (if cpp-auto-include/minimal-headers
      (cl-loop with min-headers = nil
               for h in headers
               do
               (unless (cpp-auto-include/header-is-subsumed h headers)
                 (setq min-headers (cons h min-headers)))
               finally
               return min-headers)
    headers))

;; Should we have a using directive in this file?
(defun cpp-auto-include/should-use-namespace-std ()
  (member (file-name-extension (buffer-file-name))
          cpp-auto-include/ensure-using-namespace-std))

;; Remove "using namespace std;" if any
(defun cpp-auto-include/remove-using-namespace-std ()
  (save-excursion
    (goto-char (point-max))
    (let ((regexp (format "^\\s-*using\\s-*namespace\\s-*%s\\s-*;\\s-*$"
                          cpp-auto-include/using-namespace)))
      (when (re-search-backward regexp nil t)
        (let ((beg (point)))
          (forward-line 1)
          (delete-region beg (point)))))))

;; Ensure there is a namespace qualifier at point
(defun cpp-auto-include/ensure-qualifier-at-point (nslist)
  (dolist (ns nslist)
    (let ((real-ns (if (string= ns "*")
                       (concat cpp-auto-include/using-namespace "::")
                     ns)))
      (backward-char (length real-ns))
      (when (not (looking-at real-ns))
        (forward-char (length real-ns))
        (insert real-ns))
      (backward-char (length real-ns)))))

;; Qualify all occurrences of a regexp using the namespace list
(defun cpp-auto-include/ns-qualify-regexp-occurrences (regexp nslist line)
  (save-excursion
    (goto-char (point-min))
    (when line
      (forward-line line))
    (while (re-search-forward regexp nil t)
      (unless (cpp-auto-include/false-match)
        (save-excursion
          (goto-char (match-beginning 0))
          (cpp-auto-include/ensure-qualifier-at-point nslist))))))

;; Qualify all standard things in the buffer: use this with headers where you
;; don't want "using namespace std;"
;;;###autoload
(defun cpp-auto-include/namespace-qualify-file ()
  (interactive)
  (cl-loop with case-fold-search = nil

           for info in cpp-auto-include/header-regexp
           for header = (nth 0 info)
           for nslist = (nth 1 info)
           for regexp = (nth 2 info)
           for (included-line . uses-quotes) = (cpp-auto-include/include-line header)

           do
           (when nslist
             (cpp-auto-include/ns-qualify-regexp-occurrences regexp nslist included-line))))

;;;###autoload
(defun cpp-auto-include/ensure-includes-for-file ()
  (interactive)
  (let* ((info (cpp-auto-include/parse-file))
         (added (plist-get info :added))
         (removed (plist-get info :removed))
         (use-std (cpp-auto-include/should-use-namespace-std)))
    (when removed
      (cpp-auto-include/remove-headers removed))
    (when use-std
      (cpp-auto-include/remove-using-namespace-std))
    (when added
      (let ((min-headers (cpp-auto-include/minimize-headers added)))
        (cpp-auto-include/add-headers
         (sort min-headers (lambda (x y) (string< (car x) (car y))))
         use-std)))))

;;;###autoload
(defun cpp-auto-include/ensure-includes-for-current-line ()
  (interactive)
  (let* ((info (cpp-auto-include/parse-line (line-number-at-pos (point))))
         (added (plist-get info :added))
         (removed (plist-get info :removed)))
    (when removed
      (cpp-auto-include/remove-headers removed))
    (when added
      (let ((min-headers (cpp-auto-include/minimize-headers added)))
        (cpp-auto-include/add-headers
         (sort min-headers (lambda (x y) (string< (car x) (car y))))
         nil)))))

(provide 'cpp-auto-include)

;;; cpp-auto-include.el ends here

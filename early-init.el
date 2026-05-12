;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; don't gc during startup
(defvar my/gc-cons-threshold (* 32 1024 1024))
(defvar my/gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(defun my/restore-gc ()
  (setq gc-cons-threshold my/gc-cons-threshold
        gc-cons-percentage my/gc-cons-percentage))
(add-hook 'emacs-startup-hook #'my/restore-gc 99)

;; debugging
(setq debug-on-error nil)

;; don't resize the frame on font changes etc
(setq frame-inhibit-implied-resize t)

;; prefer to load newer compiled files
(setq load-prefer-newer t)

;; disable warnings from legacy advice API
(setq ad-redefinition-action 'accept)

;; Better process interaction
(setq read-process-output-max (* 2 1024 1024)
      process-adaptive-read-buffering nil)

;; suppress lexical binding warnings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

;;------------------------------------------------------------------------------
;; package setup
(setq package-enable-at-startup nil
      use-package-expand-minimally t
      use-package-minimum-reported-time 0
      use-package-verbose t
      use-package-compute-statistics t
      package-install-upgrade-built-in t
      use-package-enable-imenu-support t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("stable-melpa" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 ("nongnu" . "http://elpa.nongnu.org/nongnu/")))

;;------------------------------------------------------------------------------
;; local early settings
(setq early-local-file (expand-file-name "early-local.el" user-emacs-directory))
(when (file-exists-p early-local-file)
  (load early-local-file))

;;------------------------------------------------------------------------------
;; native compilation options
(setq native-comp-compiler-options '("-O2"
                                     "-g0"
                                     "-fno-finite-math-only"))

(when (boundp 'my/cpu-architecture)
  (setq native-comp-driver-options `(,(format "-mtune=%s" my/cpu-architecture)
                                     ,(format "-march=%s" my/cpu-architecture))))

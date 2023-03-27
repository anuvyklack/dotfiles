;; -*- lexical-binding: t; -*-

(setq
 gc-cons-threshold most-positive-fixnum
 read-process-output-max (* 1024 1024 4) ; 4mb
 inhibit-compacting-font-caches t
 message-log-max 16384
 package-enable-at-startup nil
 load-prefer-newer noninteractive)

(when (featurep 'native-compile)
  (defvar inhibit-automatic-native-compilation)
  (setq inhibit-automatic-native-compilation nil)
  (defvar native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; origami-fold-style: triple-braces
;; End:

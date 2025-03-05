;;; pre-init.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
;;; Fonts

;; The right way to set fonts in Emacs according to eli-zaretskii (Emacs
;; maintainer):
;; - Assign the "primary" font on the `default' face via `set-face-font';
;; - Assign font overrides to `fontset-default' via `set-fontset-font' for
;;   everything else.
(set-face-font 'default "PragmataPro Mono Liga-13.7")
;; (set-face-font 'default "Inconsolata LGC-13")

(setopt use-default-font-for-symbols nil)

;; Emacs populates `url-proxy-services' variable from `https_proxy',
;; `socks_proxy', `no_proxy' environment variables.
(setq url-proxy-services '(("socks" . "127.0.0.1:10808")
                           ("https" . "127.0.0.1:10809")))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 0)
                          ("nongnu" . 0)
                          ("melpa"  . 75)))


(provide 'pre-init)
;;; pre-init.el ends here

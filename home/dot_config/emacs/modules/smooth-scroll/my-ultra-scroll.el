;;; my-ultra-scroll.el --- -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:

(leaf ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :global-minor-mode ultra-scroll-mode
  :custom
  ;; Enable smooth scrolling with PageDown and PageUp keys
  (pixel-scroll-precision-interpolate-page . t))

(provide 'my-ultra-scroll)
;;; my-ultra-scroll.el ends here

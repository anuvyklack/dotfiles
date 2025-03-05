;;; my-pixel-scroll.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:

(leaf pixel-scroll
  ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;; versions, except for emacs-mac.
  ;;
  ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;; this version of Emacs natively supports smooth scrolling.
  ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  :unless (and (eq window-system 'mac)
               (bound-and-true-p mac-carbon-version-string))
  :global-minor-mode pixel-scroll-precision-mode
  ;; :hook (after-init-hook . pixel-scroll-precision-mode)
  :custom
  ;; (pixel-scroll-precision-use-momentum . nil)
  (pixel-scroll-precision-interpolate-page . t)
  (pixel-scroll-precision-interpolate-mice . nil)
  ;; (pixel-scroll-precision-large-scroll-height . 20.0)
  (pixel-scroll-precision-interpolation-total-time . 0.3)
  :bind
  ([remap scroll-up-command] . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up))

(provide 'my-pixel-scroll)
;;; my-pixel-scroll.el ends here

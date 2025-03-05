;;; my-scroll.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:

(setq
 ;; Do not jump half the page when point goes out of the screen.
 scroll-conservatively 101
 scroll-margin 0
 scroll-step 0)

(setq scroll-conservatively 101
      scroll-margin 0
      scroll-step 0)

;; Restore original `scroll-conservatively' value for some functions.
;; Mainly for functions that perform text replacement, to center the screen when
;; jump to the next occurrence.
(mapc (lambda (fun)
        (eval `(define-advice ,fun
                   (:around (orig-fun &rest args) scroll-conservatively)
                 (let ((scroll-conservatively (default-value 'scroll-conservatively)))
                   (apply orig-fun args)))))
      '(dired-do-find-regexp-and-replace
        projectile-replace
        projectile-replace-regexp))

;;; Smooth scroll
(require 'my-ultra-scroll)
;; (require 'my-pixel-scroll)

(provide 'my-scroll)
;;; my-scroll.el ends here

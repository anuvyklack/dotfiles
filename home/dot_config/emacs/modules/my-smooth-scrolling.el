;;; my-smooth-scrolling.el ---  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:

(require 'leaf)

;;; Options

;; Do not jump half the page when point goes out of the screen.
(setq scroll-conservatively 101)
(setq scroll-margin 0)
(setq scroll-step 0)

;; Restore original `scroll-conservatively' value for some functions.
;; Mainly for functions that perform text replacement, to center the screen when
;; jump to the next occurrence.
(let ((fn (lambda (fun)
            (eval `(define-advice ,fun
                       (:around (orig-fun &rest args) scroll-conservatively)
                     (let ((scroll-conservatively (default-value 'scroll-conservatively)))
                       (apply orig-fun args)))))))
  (mapc fn '(dired-do-find-regexp-and-replace
             projectile-replace
             projectile-replace-regexp)))

;; Why is jit-lock-stealth-time nil by default?
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2022-02/msg00352.html
(setq jit-lock-stealth-time 1.25 ; Calculate fonts when idle for 1.25 seconds
      jit-lock-stealth-nice 0.5  ; Seconds between font locking
      jit-lock-chunk-size 4096)

(setq jit-lock-defer-time 0)
(with-eval-after-load 'meow
  ;; To avoid fontification while typing
  (add-hook 'meow-insert-enter-hook
            (lambda () (setq jit-lock-defer-time 0.25))
            nil t)
  (add-hook 'meow-insert-exit-hook
            (lambda () (setq jit-lock-defer-time 0))
            nil t))

;;; pixel-scroll

;; (leaf pixel-scroll
;;   :after meow
;;   ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
;;   ;; versions, except for emacs-mac.
;;   ;;
;;   ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
;;   ;; this version of Emacs natively supports smooth scrolling.
;;   ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
;;   :unless (and (eq window-system 'mac)
;;                (bound-and-true-p mac-carbon-version-string))
;;   :global-minor-mode pixel-scroll-precision-mode
;;   :custom
;;   ;; (pixel-scroll-precision-use-momentum . nil)
;;   (pixel-scroll-precision-interpolate-page . t)
;;   (pixel-scroll-precision-interpolate-mice . nil)
;;   ;; (pixel-scroll-precision-large-scroll-height . 20.0)
;;   (pixel-scroll-precision-interpolation-total-time . 0.3)
;;   :bind
;;   ([remap scroll-up-command] . pixel-scroll-interpolate-down)
;;   ([remap scroll-down-command] . pixel-scroll-interpolate-up))

;;; ultra-scroll

(leaf ultra-scroll
  :after meow
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :global-minor-mode ultra-scroll-mode
  :custom
  ;; Enable smooth scrolling with PageDown and PageUp keys
  (pixel-scroll-precision-interpolate-page . t)
  ;; (pixel-scroll-precision-large-scroll-height . 20.0)
  (pixel-scroll-precision-interpolation-total-time . 0.3)
  :bind
  ([remap scroll-up-command] . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up))

;;; Meow keybindings

(with-eval-after-load 'meow
  (meow-normal-define-key
   '("C-f" . pixel-scroll-interpolate-down)
   '("C-b" . pixel-scroll-interpolate-up)
   '("C-d" . my:pixel-scroll-interpolate-half-page-down)
   '("C-u" . my:pixel-scroll-interpolate-half-page-up)))

;; (advice-add 'pixel-scroll-interpolate-down :before #'meow--cancel-selection)
;; (advice-add 'pixel-scroll-interpolate-up :before #'meow--cancel-selection)

(defun my:pixel-scroll-interpolate-half-page-up ()
  "Interpolate a scroll upwards by half page."
  (interactive)
  ;; (meow--cancel-selection)
  (pixel-scroll-precision-interpolate (/ (window-text-height nil t) 2)
                                      nil 1))

(defun my:pixel-scroll-interpolate-half-page-down ()
  "Interpolate a scroll downwards by half page."
  (interactive)
  ;; (meow--cancel-selection)
  (pixel-scroll-precision-interpolate (- (/ (window-text-height nil t) 2))
                                      nil 1))

(let ((num-of-lines 4))
  (defun my:pixel-scroll-interpolate-line-up (count)
    (interactive "p")
    (let* ((pixel-scroll-precision-interpolation-total-time 0.08)
           (pixels-per-line (/ (window-text-height nil t)
                               (window-text-height)))
           (delta (* pixels-per-line
                     num-of-lines
                     count)))
      (pixel-scroll-precision-interpolate delta nil 1)))

  (defun my:pixel-scroll-interpolate-line-down (count)
    (interactive "p")
    (let* ((pixel-scroll-precision-interpolation-total-time 0.08)
           (pixels-per-line (/ (window-text-height nil t)
                               (window-text-height)))
           (delta (- (* pixels-per-line
                        num-of-lines
                        count))))
      (pixel-scroll-precision-interpolate delta nil 1))))

(provide 'my-smooth-scrolling)

;;; my-smooth-scrolling.el ends here

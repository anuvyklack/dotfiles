;;; my-meow.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Meow

;; Emacs can't distinguish `TAB' from `C-i' and `RET' from `C-m'
(keymap-set input-decode-map "C-i" [C-i])

(leaf meow
  ;; :vc (:url "https://github.com/anuvyklack/meow"
  ;;      :rev :newest)
  :load-path "~/code/emacs/meow"
  :require t
  :setq
  ;; (meow-cursor-type-normal . 'bar)
  ;; (meow-cursor-type-insert . 'hbar)
  (meow-cursor-type-normal . 'bar)
  (meow-cursor-type-insert . 'box)
  (meow-cheatsheet-layout . meow-cheatsheet-layout-qwerty)
  (meow-selection-command-fallback
   . '((meow-change . meow-change-char)
       (meow-kill . delete-backward-char) ; meow-delete
       (meow-cancel-selection . keyboard-quit)
       (meow-pop-selection . meow-pop-grab)
       (meow-beacon-change . meow-beacon-change-char)))
  :custom
  ;; Do not show expand hints
  ;; (meow-expand-hint-remove-delay . 0.0)
  (meow-expand-exclude-mode-list . '(markdown-mode org-mode
                                     fundamental-mode))
  (meow-mode-state-list . '((conf-mode . normal)
                            (fundamental-mode . normal)
                            (help-mode . normal)
                            (helpful-mode . normal)
                            (prog-mode . normal)
                            (text-mode . normal)))
  (meow-use-clipboard . nil)
  ;; (meow-display-thing-help . t)
  (meow-char-thing-table . '((?d . defun)
                             (?e . symbol)
                             (?p . paragraph)
                             (?l . line)
                             (?v . visual-line)
                             (?\" . string)
                             (?{ . curly)
                             (?} . curly)
                             (?\( . round)
                             (?\) . round)
                             (?\[ . square)
                             (?\] . square)
                             (?% . buffer)
                             (?. . sentence)))
  (meow-expand-selection-type . 'expand) ; select
  :config
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore)
   '("C-f" . meow-page-down)
   '("C-b" . meow-page-up))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("<backspace>" . execute-extended-command) ; emacs M-x
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("0" . meow-expand-0)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("C-." . meow-bounds-of-thing)
   '("." . meow-expand-1)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-kill)                   ; meow-delete
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   `("g" . ,(define-keymap
              "g" #'beginning-of-buffer
              "e" #'end-of-buffer
              "h" #'move-beginning-of-line ; #'back-to-indentation
              "l" #'move-end-of-line
              ;; "h" #'mwim-beginning
              ;; "l" #'mwim-end
              ))
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   ;; '("m" . meow-join)
   ;; '("o" . meow-block)
   `("m" . ,(define-keymap
              "a" #'meow-bounds-of-thing
              "i" #'meow-inner-of-thing
              "j" #'meow-join
              "m" #'meow-block))
   '("n" . meow-search)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-mark-word) ; meow-kill
   '("S" . meow-mark-symbol)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . undo-redo)                   ; meow-undo-in-selection
   '("v" . meow-select-mode)
   ;; `("v" . ,(define-keymap
   ;;            "h" #'meow-left-expand
   ;;            "j" #'meow-next-expand
   ;;            "k" #'meow-prev-expand
   ;;            "l" #'meow-right-expand))
   ;; '("w" . meow-mark-word)
   ;; '("W" . meow-mark-symbol)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("/" . meow-visit)
   '("<escape>" . meow-cancel-selection) ; ignore
   ;; '("> >" . indent-region)
   ;; '("c c" . indent-relative)
   )
  (meow-normal-define-key
   '("C-o" . meow-pop-to-mark)
   '("<C-i>" . meow-unpop-to-mark)
   ;; '("C-f" . meow-page-down)
   ;; '("C-b" . meow-page-up)
   )
  (meow-global-mode 1)
  (meow-setup-indicator))

;;; Visual / Select / Expand state

(setq meow-select-keymap (make-keymap))
(meow-define-state select
  "meow state for expanding selection like in Helix"
  :lighter " [S]"
  :keymap meow-select-keymap)

;; meow-define-state creates the variable
(setq meow-cursor-type-select 'hollow)

(meow-define-keys 'select
  '("<escape>" . meow-normal-mode)
  '("h" . meow-left-expand)
  '("j" . meow-next-expand)
  '("k" . meow-prev-expand)
  '("l" . meow-right-expand)
  '("w" . forward-word)
  '("e" . forward-word)
  '("b" . backward-word)
  '("x" . meow-line-expand)
  ;; '("l" . sp-forward-sexp)
  ;; '("h" . sp-backward-sexp)
  ;; '("j" . sp-down-sexp)
  ;; '("k" . sp-up-sexp)
  ;; '("n" . sp-forward-slurp-sexp)
  ;; '("b" . sp-forward-barf-sexp)
  ;; '("v" . sp-backward-barf-sexp)
  ;; '("c" . sp-backward-slurp-sexp)
  ;; '("u" . meow-undo)
  )

;;; scroll-on-jump

(leaf scroll-on-jump
  :ensure t
  :after meow
  ;; :setq (scroll-on-jump-curve . 'smooth-in)
  :config
  (scroll-on-jump-advice-add meow-undo)
  (scroll-on-jump-advice-add meow-search)
  (scroll-on-jump-advice-add meow-visit)
  (scroll-on-jump-advice-add meow-line)
  (scroll-on-jump-advice-add meow-beginning-of-thing)
  (scroll-on-jump-advice-add meow-end-of-thing)
  (scroll-on-jump-advice-add meow-pop-selection))

;;; Make word thing to behave like in Vim

;; (defun forward-vimlike-word (&optional arg)
;;   "Alternate `forward-word'. Essentially the same idea as Vim's 'e'."
;;   (interactive "^p")
;;   (setq arg (or arg 1))
;;   (cl-destructuring-bind (sign move-func char-func)
;;       (if (>= arg 0)
;;           '(1 skip-syntax-forward char-after)
;;         '(-1 skip-syntax-backward char-before))
;;     (with-syntax-table (standard-syntax-table)
;;       (let ((distance sign))
;;         (while (and distance (> (abs distance) 0) (> (* arg sign) 0))
;;           (setq distance
;;                 (when-let ((next-char (funcall char-func))
;;                            (next-syntax (char-syntax next-char)))
;;                   (cond ((eq next-syntax ?w)
;;                          (funcall move-func "w"))
;;                         ((eq next-syntax ?\ )
;;                          (prog1
;;                              (funcall move-func " ")
;;                            (forward-vimlike-word sign)))
;;                         (t
;;                          (funcall move-func "^w ")))))
;;           (setq arg (- arg sign)))
;;         (and distance (> (abs distance) 0))))))
;;
;; (put 'vimlike-word 'forward-op #'forward-vimlike-word)
;;
;; (setq meow-word-thing 'vimlike-word)

;;; C-w - delete word backward

;; Make C-w kill word backward and rebind `kill-region' to C-c C-k
(keymap-global-set "C-w" #'backward-kill-word)
(keymap-global-set "C-c C-k" #'kill-region)

(defun my-C-w-dwim (&optional arg)
  "`delete-backward-char`, but if region is active then kill region.
With prefix arg N, delete backward to the start of the Nth word."
  (interactive "P")
  (cond (arg
         (my-backward-delete-word (prefix-numeric-value arg)))
        ((use-region-p)
         (kill-region (region-beginning) (region-end)))
        (t
         (my-backward-delete-word (prefix-numeric-value arg)))))

(defun my-backward-delete-word (arg)
  "Like `backward-kill-word`, but just delete."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg)
                                (point))))

(keymap-global-set "C-w" #'my-C-w-dwim)

;;; Provide
(provide 'my-meow)
;;; my-meow.el ends here

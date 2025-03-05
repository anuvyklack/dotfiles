;;; my-meow.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf meow
  :ensure t
  ;; :vc (:url "https://github.com/meow-edit/meow"
  ;;      :rev :newest)
  :require t
  :setq
  ;; (meow-cursor-type-normal . 'bar)
  ;; (meow-cursor-type-insert . 'hbar)
  (meow-cheatsheet-layout . meow-cheatsheet-layout-qwerty)
  (meow-selection-command-fallback . '((meow-change . meow-change-char)
                                       (meow-kill . meow-delete)
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
  (meow-expand-selection-type . 'select) ; select
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
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("0" . meow-expand-0)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("C-." . meow-expand-1)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-kill)                   ; meow-delete
   '("D" . meow-backward-delete)
   ;; '("e" . meow-next-word)
   ;; '("E" . meow-next-symbol)
   '("e" . meow-mark-word)
   '("E" . meow-mark-symbol)
   '("f" . meow-find)
   ;; '("g" . meow-cancel-selection)
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
   ;; '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . undo-redo)                   ; meow-undo-in-selection
   '("v" . meow-visit)
   ;; '("w" . meow-mark-word)
   ;; '("W" . meow-mark-symbol)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . my:meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . meow-cancel-selection) ; ignore
   ;; '("> >" . indent-region)
   ;; '("c c" . indent-relative)
   )
  (meow-normal-define-key
   '("C-f" . meow-page-down)
   '("C-b" . meow-page-up))
  (meow-global-mode 1)
  (meow-setup-indicator))


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


;; Source: https://github.com/meow-edit/meow/issues/547#issue-2108290308
(defun my:meow-line (n &optional expand)
  "Select the current line, eol is not included.

Create selection with type (expand . line).
For the selection with type (expand . line), expand it by line.
For the other types, change selection type to (expand . line)
 and grow selection to cover the entire lines

Prefix:
numeric, repeat times."
  (interactive "p")
  (let* ((orig (mark t))
         (n (if (meow--direction-backward-p) (- n) n))
         (forward (> n 0)))
    (cond ((not (region-active-p))
           (let ((m (if forward
                        (line-beginning-position)
                      (line-end-position)))
                 (p (save-mark-and-excursion
                      (if forward
                          (progn
                            (forward-line (1- n))
                            (line-end-position))
                        (progn
                          (forward-line (1+ n))
                          (when (meow--empty-line-p)
                            (backward-char 1))
                          (line-beginning-position))))))
             (thread-first
               (meow--make-selection '(expand . line) m p expand)
               (meow--select))
             (meow--maybe-highlight-num-positions '(meow--backward-line-1 . meow--forward-line-1))))
          ((equal '(expand . line) (meow--selection-type))
           (let (p)
             (save-mark-and-excursion
               (forward-line n)
               (goto-char
                (if forward
                    (setq p (line-end-position))
                  (setq p (line-beginning-position)))))
             (thread-first
               (meow--make-selection '(expand . line) orig p expand)
               (meow--select))
             (meow--maybe-highlight-num-positions '(meow--backward-line-1 . meow--forward-line-1))))
          (t
           (let ((m (save-mark-and-excursion
                      (goto-char orig)
                      (if forward (line-beginning-position) (line-end-position))))
                 (p (if forward (line-end-position) (line-beginning-position))))
             (thread-first
               (meow--make-selection '(expand . line) m p expand)
               (meow--select))
             (meow--maybe-highlight-num-positions '(meow--backward-line-1 . meow--forward-line-1)))))))

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

;;; Provide
(provide 'my-meow)
;;; my-meow.el ends here

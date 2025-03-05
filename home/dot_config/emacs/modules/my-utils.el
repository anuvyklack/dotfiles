;;; my-utils.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; https://stackoverflow.com/questions/24356401/how-to-append-multiple-elements-to-a-list-in-emacs-lisp
(defun my:add-to-list (lst elements &optional append)
  "Add ELEMENTS to the front of the LIST.

If APPEND is non-nil add ELEMENTS to the end of the LIST.
This function change the value of the LIST symbol.

LST sould be a symbol.
ELEMENTS could be either a list or a single element."
  (unless (consp elements)
    (setq elements (list elements)))
  (set lst (if append
               (append (symbol-value lst) elements)
             (append elements (symbol-value lst)))))

(provide 'my-utils)
;;; my-utils.el ends here

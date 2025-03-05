;;; my-motions.el --- -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:

(require 'leaf)

;; Move where I mean
(leaf mwim
  :ensure t
  :commands (mwim
             mwim-beginning
             mwim-end
             mwim-beginning-of-code-or-line
             mwim-beginning-of-line-or-code
             mwim-beginning-of-code-or-line-or-comment
             mwim-end-of-code-or-line
             mwim-end-of-line-or-code)
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

(provide 'my-motions)
;;; my-motions.el ends here

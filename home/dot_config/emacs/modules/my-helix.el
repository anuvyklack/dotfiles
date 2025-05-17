;;; my-helix.el --- Helix -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(require 'leaf)

(leaf helix
  :load-path "~/code/emacs/helix"
  :require t
  :config
  (helix-keymap-set nil 'normal
    "<backspace>" #'execute-extended-command))

(leaf keypad
  :load-path "~/code/emacs/helix"
  :require t
  :config
  (helix-keymap-set nil 'normal
    "SPC" #'keypad
    "C-h k" #'keypad-describe-key))

(provide 'my-helix)
;;; my-helix.el ends here

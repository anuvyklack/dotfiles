;;; my-help.el -*- no-byte-compile: t; lexical-binding: t; -*-

(setopt help-window-select t)
(keymap-unset help-map "C-c" :remove) ; 'describe-copying

(leaf helpful
  :ensure t
  :hook (helpful-mode . outline-minor-mode)
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         (help-map ("F" . helpful-function)
                   ("s" . helpful-symbol))))

(provide 'my-help)
;;; my-help.el ends here

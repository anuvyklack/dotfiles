;;; post-init.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Libraries

(unless (package-installed-p 'leaf)
  (package-install 'leaf))
(leaf leaf-keywords :ensure t)
(leaf dash :ensure t :require t)
(leaf f :ensure t :require t)
(leaf s :ensure t :require t)

(require 'xdg)

;; Recursively add to `load-path' all folders in `$XDG_CONFIG_HOME/emacs/modules/' directory.
(let ((modules-dir (thread-first
                     (file-name-concat (xdg-config-home) "emacs" "modules")
                     (file-name-as-directory))))
  (setq load-path `(,@load-path
                    ,modules-dir
                    ,@(f-directories modules-dir nil t))))

(require 'my-utils)
(require 'my-meow)
(require 'my-motions)
(require 'my-scroll)
(require 'my-help)

(provide 'post-init)
;;; post-init.el ends here

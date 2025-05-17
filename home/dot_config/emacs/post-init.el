;;; post-init.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'xdg)

(unless (package-installed-p 'leaf)
  (package-install 'leaf))
(leaf leaf-keywords :ensure t)
(leaf dash :ensure t :require t)
(leaf f :ensure t :require t)
(leaf s :ensure t :require t)

;; Recursively add to `load-path' all folders in
;; `$XDG_CONFIG_HOME/emacs/modules/' directory.
(let ((modules-dir (-> (file-name-concat (xdg-config-home) "emacs" "modules")
                       (file-name-as-directory))))
  (setq load-path `(,@load-path
                    ,modules-dir
                    ,@(f-directories modules-dir nil t))))

(require 'my-utils)
(require 'my-emacs-core)
(require 'my-color-scheme)
(require 'my-meow)
;; (require 'my-helix)
(require 'my-motions)
;; (require 'my-smooth-scrolling)
(require 'my-help)
(require 'my-completion)

(provide 'post-init)
;;; post-init.el ends here

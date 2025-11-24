;;; my-emacs-core.el --- -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:

(require 'leaf)

;; Rebind `usinversal-argument' from `C-u' to `M-u'.
;; By default `M-u' is binded to `upcase-word', so we can reuse it,
;; and `C-u' I use for scrolling like in Vim.
(keymap-global-set "M-u" #'universal-argument)
(keymap-set universal-argument-map "M-u" #'universal-argument-more)

;; User credentials. Some functionality uses this to identify you, e.g. GPG
;; configuration, email clients, file templates and snippets.
(setq user-full-name "Yuriy Artemyev"
      user-mail-address "anuvyklack@gmail.com")

;; Highlight cursor line in `list-packages' menu.
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; `M-=' (`describe-char') will show human readable output.
(setopt what-cursor-show-names t)

(setopt confirm-kill-emacs nil)

(setopt history-delete-duplicates t)

;; Automatically revert the buffer when its visited file changes on disk. Auto
;; Revert will not revert a buffer if it has unsaved changes, or if its file on
;; disk is deleted or renamed.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; Vertical motion starting at end of line keeps to ends of lines.
(setopt track-eol t)

;; Keep track of opened files.
(leaf recentf
  :hook ((after-init-hook . (lambda()
                              (let ((inhibit-message t))
                                (recentf-mode 1))))
         (kill-emacs-hook . recentf-cleanup))
  :config
  (setq recentf-auto-cleanup (if (daemonp) 300)))

;; Save minibuffer history between sessions.
(add-hook 'after-init-hook #'savehist-mode)

;; Save the last location within a file upon reopening.
(add-hook 'after-init-hook #'save-place-mode)

(leaf which-key
  ;; :require t
  :commands which-key-mode
  :hook (after-init-hook . which-key-mode)
  :custom
  (which-key-idle-delay . 1.5)
  (which-key-idle-secondary-delay . 0.25)
  (which-key-add-column-padding . 1)
  (which-key-max-description-length . 40))

(provide 'my-emacs-core)
;;; my-emacs-core.el ends here

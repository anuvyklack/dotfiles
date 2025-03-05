;;; my-completion.el --- -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
(require 'leaf)

(leaf vertico
  :ensure t
  :commands vertico-mode
  :hook (after-init-hook . vertico-mode)
  :bind (vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("<escape>" . meow-motion-mode)
         ("<escape>" . meow-minibuffer-quit)
         ))

(leaf orderless
  :ensure t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion)))))

(leaf marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init-hook . marginalia-mode))

(leaf embark
  :ensure t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :pre-setq (prefix-help-command . #'embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ; pick some comfortable binding
   ("C-;" . embark-dwim)        ; good alternative: M-.
   ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'
  :config
  ;; ;; Hide the mode line of the Embark live/completions buffers
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
  ;;                nil
  ;;                (window-parameters (mode-line-format . none))))
  )

(leaf embark-consult
  :ensure t
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf consult
  :ensure t
  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :setq
  ;; Optionally configure the register formatting. This improves the register
  (register-preview-delay . 0.5)
  (register-preview-function . #'consult-register-format)
  ;; ;; Use Consult to select xref locations with preview
  ;; (xref-show-xrefs-function . #'consult-xref)
  ;; (xref-show-definitions-function . #'consult-xref)

  ;; :advice
  ;; ;; Optionally tweak the register preview window.
  ;; (:override register-preview consult-register-window)

  :config
  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")

  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         (isearch-mode-map
          ("M-e" . consult-isearch-history)
          ("M-s e" . consult-isearch-history)
          ("M-s l" . consult-line)
          ("M-s L" . consult-line-multi))
         ;; Minibuffer history
         (minibuffer-local-map
          ("M-s" . consult-history)
          ("M-r" . consult-history))))

(provide 'my-completion)
;;; my-completion.el ends here

;;init.el -*- lexical-binding: t; -*-

;;; Setup elpaca {{{
(defvar elpaca-installer-version 0.3)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package ;; Install use-package support
        (elpaca-use-package-mode) ;; Enable :elpaca use-package keyword.
        ; (setq elpaca-use-package-by-default t) ;; Assume :elpaca t unless otherwise specified.
        )

(elpaca-wait) ;; Block until current queue processed.

;; Make elpaca native keybindings have precedence over evil keymaps in elpaca buffer.
(with-eval-after-load 'evil
  (with-eval-after-load 'elpaca-ui   (evil-make-intercept-map elpaca-ui-mode-map))
  (with-eval-after-load 'elpaca-info (evil-make-intercept-map elpaca-info-mode-map)))

; ;; Expands to: (elpaca evil (use-package evil :demand t))
; (use-package evil :demand t)
;
; ;;Turns off elpaca-use-package-mode current declartion
; ;;Note this will cause the declaration to be interpreted immediately (not deferred).
; ;;Useful for configuring built-in emacs features.
; (use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))
;
; ;; Don't install anything. Defer execution of BODY
; (elpaca nil (message "deferred"))

;;}}}

;;; Libraries {{{
(use-package s    :elpaca t) ;; string manipulation library
(use-package dash :elpaca t) ;; list manipulation library
(use-package diminish :elpaca t)
(use-package hydra :elpaca t)
(use-package general :elpaca t
  :config
  (general-auto-unbind-keys))
;; }}}

;;; Core packages {{{

;; (defvar leader-map (make-sparse-keymap))
(define-prefix-command 'leader-map)

(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(put 'narrow-to-region 'disabled nil)

(use-package emacs
  :custom
  (user-mail-address "anuvyklack@gmail.com")
  (user-full-name "Yuriy Artemyev")
  (ring-bell-function #'ignore)
  (inhibit-startup-message t)
  (x-gtk-use-system-tooltips nil)
  (cursor-type 'box)
  (cursor-in-non-selected-windows nil)
  (require-final-newline t)
  (truncate-lines t) ;; do not wrap long lines
  (fill-column 80)
  (comment-empty-lines t)
  (scroll-conservatively 101) ;; Do not jump half the page when point goes out of the screen.
  ;; (scroll-preserve-screen-position t)
  :config
  ;; (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  (blink-cursor-mode 0)
  (set-fringe-mode 3) ;; Give some breathing room
  (column-number-mode 1) ;; show column number in modeline
  )

(use-package display-line-numbers
  :custom
  ;; (display-line-numbers-width 4)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  :hook (prog-mode . display-line-numbers-mode))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package mwheel
  :custom
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-progressive-speed nil))

;; (global-display-line-numbers-mode t)
;; ;; Disable line numbers for some modes
;; (dolist (mode '(org-mode-hook
;;                 term-mode-hook
;;                 shell-mode-hook
;;                 eshell-mode-hook
;;                 helpful-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package better-defaults
  :elpaca t
  :config
  (when window-system
    (set-face-attribute 'default nil :font "Inconsolata LGC" :height 125)
    ;; (set-face-attribute 'default nil :font "Monego" :height 125)
    (set-frame-size (selected-frame) 134 63)
    (set-frame-position (selected-frame) 1190 35)))

(use-package pixel-scroll
  :when (fboundp #'pixel-scroll-precision-mode)
  :hook (after-init . pixel-scroll-precision-mode)
  :custom
  (scroll-margin 0))

(require 'my-ibuffer)

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (isearch-lazy-highlight t))

(use-package outline
  :custom
  (outline-minor-mode-cycle t))

;;}}}

;;; Color schemes {{{

(use-package ef-themes
  :elpaca t
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  :config
  (load-theme 'ef-light :no-confirm)
  ;; (load-theme 'ef-day :no-confirm)
  (set-cursor-color "black"))

;;}}}

;;; Evil {{{
(use-package evil
  :elpaca t
  :custom
  (evil-want-integration t)  ;; need for evil-collection
  (evil-want-keybinding nil) ;; need for evil-collection
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump t)
  ;; (evil-want-minibuffer t)
  ;; (evil-search-module 'evil-search)
  (evil-move-beyond-eol t) ;; need for lispyville
  :config
  ;; (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-select-search-module 'evil-search-module 'isearch)
  (customize-set-variable 'evil-want-Y-yank-to-eol t) ;; Y -> y$
  (evil-ex-define-cmd "ls" 'ibuffer) ;; bind ':ls' command to 'ibuffer instead of 'list-buffers
  (evil-mode 1))

(use-package evil-collection
  :elpaca t
  :after evil
  :config
  (evil-collection-init)
  (general-def
    :states '(normal visual)
    "SPC" '(:keymap leader-map) ; use 'Space' as leader key
    "<backspace>" 'evil-ex
    "g h" 'evil-first-non-blank
    "g l" 'evil-end-of-line
    "g b" 'ibuffer
    "C-p" 'consult-yank-from-kill-ring
    "/" 	'consult-line
    "?" 	'evil-search-forward
    "z n" 'narrow-to-region
    "z w" 'widen)
  (general-def
    :keymaps 'leader-map
    "h" '(:keymap help-map :which-key "help"))
  (general-def
    :states 'insert
    "C-l" 'right-char))

(use-package evil-nerd-commenter
  ;; Use `gc{motion}' to comment target, `gcc' to comment line.
  :elpaca t
  :after (evil general)
  :custom
  (evilnc-comment-text-object "c")
  :config
  ;; (require 'compat-29)
  ;; (keymap-set evil-normal-state-map "g c" #'evilnc-comment-operator)
  ;; (keymap-set evil-visual-state-map "g c" #'evilnc-comment-operator)
  (general-def
   :states '(normal visual)
   "g c" 'evilnc-comment-operator))

(use-package evil-visualstar
  :elpaca t
  :after evil
  :config (global-evil-visualstar-mode))

(use-package evil-goggles
  :elpaca t
  :after evil
  :custom
  ;; (evil-goggles-duration 0.100) ;; default is 0.200
  (evil-goggles-enable-delete nil)
  ;; (evil-goggles-enable-change nil)
  :config
  (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-matchit
  :elpaca t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :elpaca t
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :elpaca t
  :after (evil general)
  :config
  (general-def
    :states '(normal visual)
    "C-a" 'evil-numbers/inc-at-pt
    "C-x" 'evil-numbers/dec-at-pt)
  (general-def
    :states 'visual
    "g C-a" 'evil-numbers/inc-at-pt-incremental
    "g C-x" 'evil-numbers/dec-at-pt-incremental))

;;}}}

;;; Completion framework {{{

;; (use-package icomplete
;;   :config
;;   (fido-vertical-mode 1))

(use-package vertico ;;{{{
  :elpaca t
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-n" . vertico-next-group)
         ("C-p" . vertico-previous-group)
         ("C-l" . vertico-insert)
         ("C-f" . vertico-scroll-up)
         ("C-b" . vertico-scroll-down)
         ;; ("C-g" . vertico-exit)
         :map minibuffer-local-map
         ("C-h" . backward-kill-word))
  :custom
  (vertico-count 14) ;; How many candidates to show.
  (vertico-scroll-margin 2)
  (vertico-cycle nil)
  ;; ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; ;; Vertico commands are hidden in normal buffers.
  ;; (read-extended-command-predicate #'command-completion-default-include-p)
  (enable-recursive-minibuffers t) ;; Enable recursive minibuffers
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :init
  (ido-mode -1)
  (vertico-mode)
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
) ;;}}}

(use-package orderless
  :elpaca t
  :after vertico
  :init
  (setq completion-styles '(orderless))
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  (setq orderless-matching-styles
        '(orderless-initialism orderless-prefixes orderless-regexp)))

(use-package marginalia
  :elpaca t
  ;; :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :elpaca t
  :preface
  (define-prefix-command 'consult-prefix-map)
  :bind (:map leader-map
         ;; ("fo" . consult-recent-file)
         ("fo" . consult-outline)
         ("fb" . consult-buffer)
         ("fg" . consult-grep)
         ("fi" . consult-imenu))
  :init
  (setq completion-in-region-function #'consult-completion-in-region))

;;}}}

;;; Dired {{{
(use-package dired
  :after general
  :custom
  (dired-listing-switches "-lAhF -v --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-no-confirm t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies  'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|\\`[.].+")
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  (dired-mode . hl-line-mode)
  :config
  (general-def
    :keymaps 'dired-mode-map
    :states 'normal
    "<backspace>" 'dired-up-directory
    ;; "<tab>" 'dired-subtree-cycle
    "l" 'dired-find-file
    "h" 'dired-up-directory
    "w" 'dired-display-file
    ")" 'dired-omit-mode
    "<" 'dired-prev-marked-file
    ">" 'dired-next-marked-file)
  (general-def
    :keymaps 'dired-mode-map
    :states 'visual
    "d" 'dired-flag-file-deletion
    "u" 'dired-unmark))

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t))

(use-package dired-ranger
  :elpaca t)

(use-package dired-subtree :elpaca t)
(use-package dired-narrow  :elpaca t)
(use-package dired-open    :elpaca t)
(use-package dired-toggle-sudo :elpaca t)

(use-package dired-collapse
  :elpaca t
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-rainbow
  :elpaca t
  :config
  (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
  (defconst my-video-files-extensions
    '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg" "mkv")
    "Media files.")
  (dired-rainbow-define media "#ce5c00" my-video-files-extensions)
  ;; Highlight executable files, but not directories:
  (dired-rainbow-define-chmod executable-unix "#4e9a06" "-.*x.*"))

(use-package diredfl ;; Addtional syntax highlighting for dired
  :elpaca t
  :hook ((dired-mode . diredfl-mode)
         (dirvish-directory-view-mode . diredfl-mode))
  ;; :config
  ;; (set-face-attribute 'diredfl-dir-name nil :bold t)
  )

;; C-x C-d to open 'dired-recent-open'.
(use-package dired-recent
  :elpaca t
  :config
  (dired-recent-mode t))

;; (use-package diredc
;;   :disabled
;;   :elpaca t)

;;}}}

;;; Text editting {{{

(use-package lispy
  :elpaca t
  :custom
  (lispy-safe-delete t)
  (lispy-safe-copy t)
  (lispy-safe-paste t)
  :hook (emacs-lisp-mode . lispy-mode))

(use-package lispyville
  :elpaca t
  :after general
  :hook
  (lispy-mode . (lambda ()
                  (lispyville-mode)
                  (general-def
                    :keymaps 'local
                    :states 'normal
                    "(" 	 	'lispyville-backward-up-list
                    "g c"   'lispyville-comment-or-uncomment
                    "[ SPC" 'evil-collection-unimpaired-insert-newline-above
                    "] SPC" 'evil-collection-unimpaired-insert-newline-below)
                  (general-def :keymaps 'local :states 'visual
                    "g c" 'lispyville-comment-or-uncomment)))
  :config
  (lispyville-set-key-theme
   '(operators  c-w  c-u  prettify  additional-motions  additional  wrap)))

;; }}}

;;; Tools {{{

(use-package zoxide
  :elpaca t
  :bind (:map leader-map
         ("fz" . zoxide-find-file)))

;; Garbage Collector Magic Hack: https://gitlab.com/koral/gcmh
(use-package gcmh
  :elpaca t
  :diminish
  :config (gcmh-mode 1))

(use-package no-littering
  :elpaca t
  :demand t
  :config
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
      (convert-standard-filename
        (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

(use-package recentf ;; Save recent files. {{{
  :after no-littering
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (recentf-mode 1)
  ;; ;; Save recentf list into file every 10 minutes.
  ;; (run-at-time nil 600 'recentf-save-list)
  )

;; (use-package recentf ;; Save recent files.
;;   :hook (after-init . recentf-mode)
;;   :defines (recentf-exclude)
;;   :config
;;   (add-to-list 'recentf-exclude "\\.gpg\\")
;;   (dolist (dir (list (expand-file-name ".cache/" user-emacs-directory)
;;                      (expand-file-name "workspace/.cache/" user-emacs-directory)))
;;     (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*"))))

;;}}}

(use-package savehist ;; Save minibuffer history.
  :after no-littering
  ;; :hook (after-init . savehist-mode)
  :config
  (savehist-mode 1))

(use-package projectile
  :elpaca t
  ;; :custom
  ;; (projectile-auto-discover t)
  ;; (projectile-project-search-path)
  :config
  (projectile-mode)
  (keymap-set leader-map "p" 'projectile-command-map)
  (keymap-set projectile-command-map "B" #'projectile-ibuffer)
  (keymap-unset projectile-command-map "I" t))

(use-package ibuffer-projectile
  :elpaca t
  :hook (ibuffer . (lambda ()
                     (ibuffer-projectile-set-filter-groups)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic)))))

;; (use-package moom
;;   :elpaca t
;;   :after transient
;;   :init
;;   (when window-system
;;     (set-face-attribute 'default nil :font "Inconsolata LGC" :height 125))
;;   :config
;;   (moom-mode 1)
;;   (moom-move-frame-to-edge-right))

(use-package origami
  :elpaca t
  ;; :custom
  ;; (origami-show-fold-header t) ;; highlight fold headers
  ;; :config
  ;; (global-origami-mode 1)
  )

(use-package helpful
  :elpaca t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command]  . helpful-command)
         ([remap describe-key] . helpful-key)
         :map help-map
         ("F" . helpful-function)))

;; (use-package elisp-demos
;;   :elpaca t
;;   :after helpful
;;   :config
;;   (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package rainbow-delimiters
  :elpaca t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rg
  :elpaca t)

(use-package zoom
  :disabled
  :elpaca t
  :custom
  (split-width-threshold 130) ;; Threshold to split window horizontally.
  (zoom-mode t)
  (zoom-ignored-major-modes '(dired-mode))
  (temp-buffer-resize-mode t))

(use-package easy-escape
  :elpaca t
  :hook (emacs-lisp-mode . easy-escape-minor-mode)
  ;; :config
  ;; (add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode)
  )

;; Provides 'far-fill-paragraph' function which fills the paragraph at the point.
(use-package far
  :elpaca (:host github :repo "eshrh/far.el"))

;;}}}

;;; My functions {{{

(defun my/paste-and-indent-after ()
  (interactive)
  (with-undo-amagamate ;; emacs 29
   (evil-paste-after 1)
   (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))))

(defun my/paste-and-indent-before ()
  (interactive)
  (with-undo-amagamate ;; emacs 29
   (evil-paste-before 1)
   (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))))

;;}}}

;;; Keybindings {{{

;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") #'keyboard-escape-quit)
;; (keymap-global-set "<escape>" #'keyboard-escape-quit) ; emacs 29

(use-package which-key
  :elpaca t
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1.2)
  (which-key-popup-type 'side-window)
  :init
  (which-key-mode))

;;}}}

;;; UI tweaks {{{

(use-package all-the-icons
  :elpaca t
  :config
  (let ((cache (expand-file-name
                ".all-the-icons-font-installed" user-emacs-directory)))
    (unless (file-exists-p cache)
      (all-the-icons-install-fonts t)
      (with-temp-buffer (write-file cache)))))

(use-package doom-modeline
  :elpaca t
  :custom
  (doom-modeline-height 30)
  :config
  (doom-modeline-mode 1))

(use-package page-break-lines
  :elpaca t
  ;; :config
  ;; (global-page-break-lines-mode 1)
  :hook
  (help-mode . page-break-lines-mode)
  (outline-mode . page-break-lines-mode))

;;}}}

;;; Hooks / Major modes {{{

(add-hook 'prog-mode-hook
          (lambda ()
            ;; (hs-minor-mode) ;; activate folding for all programming modes
            (setq show-trailing-whitespace t)
            ;; (electric-pair-mode)
            ))

(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode)))

(add-hook 'python-mode-hook
          (lambda ()
            (setq evil-shift-width  python-indent)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq evil-shift-width  lisp-body-indent)
            (setq tab-width lisp-body-indent)

            ;; (evil-global-set-key 'normal (kbd "K") 'describe-symbol)
            (evil-global-set-key 'normal (kbd "K") #'helpful-at-point)

            ;; (evil-local-set-key 'normal "p" 'my/paste-and-indent-after)
            ;; (evil-local-set-key 'normal "P" 'my/paste-and-indent-before)
            ))

;; Set options for 'init.el' file.
(add-hook 'find-file-hook
          (lambda ()
            (when (string-equal (buffer-file-name) user-init-file)
              ;; (outline-minor-mode 1)
              (origami-mode 1)
              (evil-global-set-key 'normal (kbd "zj") 'origami-forward-fold)
              (evil-global-set-key 'normal (kbd "zk") 'origami-previous-fold))))

;;}}}

;; Local Variables:
;; origami-fold-style: triple-braces
;; End:

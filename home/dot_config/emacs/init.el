;;; init.el -*- lexical-binding: t; no-byte-compile: t -*-
;;; Helheim
;;;; Fonts

(require 'cl-macs)

(cl-defun helheim-set-fontset-font (font charsets &key (fontset t) add)
  "Force some code point diapasons to use particular FONT."
  (declare (indent 1))
  (dolist (charset charsets)
    (set-fontset-font fontset charset font nil add)))

;;;;; Main font
;; Set up fonts before anything else so error messages during startup were
;; readable.
;;
;; Place cursor before the character and press “ga” to see information about it.
;; Press "<F1> k ga" to find out which command is bound to "ga".

;; `face-font-family-alternatives'
(let* ((font "PragmataPro Liga")
       (spec (font-spec :family font :size 13.9 :weight 'normal))
       ;; (font "Hack")
       ;; (spec (font-spec :family "Hack" :size 13.0))
       ;; (font "Cascadia Code")
       ;; (spec (font-spec :family font :size 13.0 :weight 'normal))
       ;; (font "TX-02")
       ;; (spec (font-spec :family font :size 12.8))
       )
  (set-face-font 'default spec)
  (set-face-font 'fixed-pitch spec)
  ;; Prepend our font to the "fontset-default" to make it the first fallback
  ;; candidate for itself. This plays when text is bold or italic and the
  ;; default font lacks glyphs for those styles but does provide them for the
  ;; regular style. With this change, Emacs will use the regular glyphs from
  ;; the default font when bold or italic variants are unavailable, instead of
  ;; falling back to a different font.
  ;;   BUG: Using `font-spec' with `set-fontset-font' doesn't work, despite
  ;; documentation claims it is.
  (set-fontset-font t 'unicode font nil 'prepend))

;; (font-face-attributes (face-attribute 'fixed-pitch :font))
;; (font-face-attributes (face-font 'fixed-pitch))

;;;;; Nerd Icons

;; (setq nerd-icons-scale-factor 0.9)
(setq nerd-icons-default-adjust 0.1)

(helheim-set-fontset-font "Symbols Nerd Font Mono"
  '((#xe5fa . #xe6b7) ;; Seti-UI + Custom  
    (#xe700 . #xe8ef) ;; Devicons  
    (#xed00 . #xf2ff) ;; Font Awesome  
    (#xe200 . #xe2a9) ;; Font Awesome Extension  
    (#xe300 . #xe3e3) ;; Weather  
    (#xf400 . #xf533) #x2665 #x26A1 ;; Octicons   ♥ ⚡
    (#x23fb . #x23fe) #x2b58 ;; IEC Power Symbols ⏻ ⏾ ⭘
    (#xf300 . #xf381) ;; Font Logos   
    (#xe000 . #xe00a) ;; Pomicons  
    (#xea60 . #xec1e) ;; Codicons  
    (#x276c . #x2771) ;; Heavy Angle Brackets ❬ ❱
    (#xee00 . #xee0b) ;; Progress  
    (#xf0001 . #xf1af0))) ;; Material Design Icons 󰀁 󱫰

;; In the modeline, we’re not restricted by a rigid grid, and non-monospace
;; Powerline symbols look better.
(helheim-set-fontset-font "Symbols Nerd Font" ;; "Symbols Nerd Font"
  `(;; Powerline Symbols
    (#xe0a0 . #xe0a2) ;;  
    (#xe0b0 . #xe0b3) ;;  
    ;; Powerline Extra Symbols
    (#xe0b4 . #xe0c8) ;;   
    (#xe0cc . #xe0d7) ;;  
    #xe0a3 #xe0ca))   ;;  

;; Restore some icons.
(helheim-set-fontset-font "PragmataPro"
  ;; Font Awesome
  `(#xf0c5   ;; 
    #xf114   ;; 
    #xf115)) ;; 

;;;;; Unicode

;; General Punctuation Unicode Block
;;  ‐ ‑ ‒ – — ― ‖ ‗
;; ‘ ’ ‚ ‛ “ ” „ ‟
;; † ‡ • ‣ ․ ‥ … ‧ ‰ ‱ ′ ″ ‴ ‵ ‶ ‷ ‸ ‹ ›
;; ※ ‼ ‽ ‾ ‿ ⁀ ⁁ ⁂ ⁃ ⁄ ⁅ ⁆ ⁇ ⁈ ⁉ ⁊ ⁋ ⁌ ⁍
;; ⁎ ⁏ ⁐ ⁑ ⁒ ⁓ ⁔ ⁕ ⁖ ⁗ ⁘ ⁙ ⁚ ⁛ ⁜ ⁝ ⁞
;; №

;; ;; Box Drawing
;; ;; #x2500  ─
;; ;; #x259f  ▟
;; (set-fontset-font t (cons ?\x2500 ?\x259f) "Symbols Nerd Font Mono")

;; (progn
;;   (setq use-default-font-for-symbols nil)
;;   (set-face-font 'default (font-spec :family "Inconsolata LGC" :size 17))
;;   ;; Unicode Symbols for Legacy Computing
;;   (set-fontset-font t (cons ?🬀 ?🯊) "LegacyComputing")
;;   (set-fontset-font t (cons ?🯰 ?🯹) "LegacyComputing"))

;;;; Helheim core

;; In case you use VPN. Also Emacs populates `url-proxy-services' variable
;; from: `https_proxy', `socks_proxy', `no_proxy' environment variables.
(setq url-proxy-services '(("socks" . "127.0.0.1:10808")
                           ("https" . "127.0.0.1:10809"))
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq helheim-package-manager 'elpaca) ;; or 'straight
(require 'helheim-core)

;;;; Color theme

(setup helheim-modus-themes
  (:require t)
  (load-theme 'modus-operandi t)
  ;; (load-theme 'modus-vivendi t)
  )

;; I can recommend `leuven' theme for org-mode work. It has so many nice little
;; touches to spruce up org-mode elements that some users switch to it from
;; their usual dark doom or modus themes when working on org-mode projects.
;;   You may try it with ": load-theme" then type "leuven".
(setup leuven-theme (:install t))

;; (setup helheim-ef-themes
;;   (:require t)
;;   (load-theme 'ef-light t))

(setup my-ef-themes
  (:require t)
  (load-theme 'ef-light t))

;;;; Essentials

(require 'helheim-minibuffer) ; Emacs version of command palette
(require 'helheim-completion)

(require 'helheim-ibuffer)  ; Buffers menu
(require 'helheim-dired)    ; File-manager
(require 'helheim-embark)   ; Context-aware action menus
(require 'helheim-outline)  ; See "Outline Mode" in Emacs manual
(require 'helheim-tab-bar)  ; Each tab represents a set of windows, as in Vim

;;;;; Modeline

(setopt mode-line-percent-position nil)

(setup helheim-modeline ; Normal people call this "status line"
  ;; The function to handle `buffer-file-name'.
  (setq doom-modeline-buffer-file-name-function #'identity ; `buffer-file-name'
        doom-modeline-buffer-file-truename-function #'identity) ; `buffer-file-truename'
  (setq doom-modeline-support-imenu t)
  ;; (:hook doom-modeline-mode-hook size-indication-mode)
  (:require t))

;; (let ((spec (font-spec :family "Basic Commercial LT" :weight 'normal))
;;       (spec (font-spec :family "ITC Avant Garde Gothic W1G" :weight 'medium))
;;       (spec (font-spec :family "Noto Sans" :size 13.9 :weight 'normal)))
;;   (set-face-font 'mode-line spec)
;;   (set-face-font 'mode-line-active spec)
;;   (set-face-font 'mode-line-inactive spec)
;;   (setq doom-modeline-spc-face-overrides
;;         (list :family (face-attribute 'fixed-pitch :family))))

;;;; Search

(require 'helheim-consult)  ; A set of search commands with preview
(require 'helheim-deadgrep) ; Interface for Ripgrep in Emacs

;;;; IDE

(require 'helheim-xref)     ; Go to definition framework
(require 'helheim-eglot)    ; Built-in LSP client

;;;; Version control

(setup helheim-magit
  (:require t)
  (setopt magit-diff-refine-hunk 'all
          magit-repository-directories '(("~/.config/emacs/" . 0))))

(require 'helheim-diff-hl)  ; git gutter
(require 'helheim-ediff)

;;;; Terminal emulators
;; Requires shell-side configuration!

; (require 'helheim-eat)   ; written in emacs-lisp
; (require 'helheim-vterm) ; libvterm C library
(require 'helheim-ghostel)

;;;; LLM

(setup agent-shell
  (require 'helheim-agent-shell)
  (setopt agent-shell-preferred-agent-config 'claude-code
          agent-shell-session-strategy 'prompt
          ;; agent-shell-prefer-viewport-interaction nil
          ;; agent-shell-tool-use-expand-by-default nil
          ;; agent-shell-user-message-expand-by-default nil
          )
  (:after-load
    (:with-keymap agent-shell-mode-map
      ;; State-specific Enter behavior:
      ;; - insert state = newline
      ;; - normal state = send
      (:bind :state 'insert "RET" 'newline)
      (:bind :state 'normal "RET" 'comint-send-input))))

(setup mcp-server
  (require 'helheim-mcp-server)
  (setopt mcp-server-security-dangerous-functions '(kill-emacs)))

;;;; Other modules

(require 'helheim-notmuch)  ; Notmuch email client
(require 'helheim-browser)  ; Synchronize online text editor with Emacs buffer

(setup helheim-whisper ; Speech to text conversion
  (:require t)
  (setopt whisper-install-whispercpp 'manual
          whisper-install-directory "~/.local/src/"
          whisper-enable-speed-up t ;; WARNING
          whisper-model "small"))

(require 'helheim-edit-indirect) ; Alternative "zn" binding

(require 'helheim-chezmoi)  ; Integration with chezmoi dotfile manager

;;; My custom config

(setf (alist-get 'fullscreen initial-frame-alist) 'maximized)

(setopt user-full-name "Yuriy Artemyev"
        user-mail-address "anuvyklack@gmail.com"
        confirm-kill-emacs nil
        eldoc-echo-area-use-multiline-p t
        ibuffer-expert t
        what-cursor-show-names t
        ;; nobreak-char-display nil
        )

;; ;; See also `search-invisible'
;; (global-reveal-mode)

;; (setup imenu-list (:install t))

;;;; cape

;; (setup cape
;;   (:global-bind :state 'insert
;;     ;; Emulate Vim's omni-completion keybinds
;;     "C-x"   'cape-prefix-map)
;;   (:with-keymap cape-prefix-map
;;     (:bind
;;       "C-o" 'completion-at-point ;; C-x C-o is Vim's omni-completion keybinding
;;       ;; "C-e" 'cape-elisp-block
;;       ;; "C-s" 'cape-elisp-symbol
;;       "/"   'cape-tex
;;       "C-/" 'cape-tex
;;       "C-h" 'cape-history
;;       "C-l" 'cape-line
;;       "C-k" 'cape-keyword
;;       "C-f" 'cape-file
;;       "C-t" 'complete-tag
;;       "C-w" 'cape-dict
;;       "C-r" 'cape-rfc1345
;;       ;; "s"   'cape-dict
;;       ;; "C-s" 'yasnippet-capf
;;       "C-a" 'cape-abbrev
;;       "C-d" 'cape-dabbrev
;;       "C-n" 'cape-dabbrev
;;       ;; "C-p" '+corfu/dabbrev-this-buffer
;;       )))

;;;; dired

;; My custom version.
(define-advice helheim-dired-do-add-id (:override () custom-version)
  "Add timestamp based ID in front of the files name, unless it's already there."
  (dolist (file (dired-get-marked-files))
    (unless (helheim-dired-file-id file)
      (let ((filename (file-name-nondirectory file)))
        (cond
         ;; Files from Reddit app on android. They have timestamp in their name,
         ;; like this: RDT_20220820_0858002573777192519160821.jpg
         ((string-match "^RDT_\\([0-9]\\{8\\}\\)_\\([0-9]\\{6\\}\\)" filename)
          (let* ((date (match-string-no-properties 1 filename))
                 (time (match-string-no-properties 2 filename))
                 (extension (file-name-extension file))
                 (newname (format "%sT%s.%s" date time extension)))
            (rename-file file newname)))
         (t
          (let* ((id (helheim-dired-generate-file-id file))
                 (newname (format "%s--%s" id filename)))
            (rename-file file newname)))))))
  (dired-revert))

;;;; DISABLED pandoc-mode

;; (setup pandoc-mode
;;   (:install t)
;;   (:when (executable-find "pandoc"))
;;   (:hook markdown-mode-hook pandoc-mode)
;;   ;; (:hook markdown-mode-hook conditionally-turn-on-pandoc)
;;   (:with-keymap pandoc-mode-map
;;     (:unbind "C-c /")
;;     (:bind ", /" '("pandoc" . pandoc-main-transient))))

;;;; project.el

(hel-keymap-set project-prefix-map
  "b" 'project-list-buffers)

;;;; rainbow-mode

;; Colorize strings that represent colors
(setup rainbow-mode
  (:install t)
  (:blackout t)
  (:hook (emacs-lisp-mode-hook
          conf-mode-hook
          fish-mode-hook
          toml-ts-mode-hook)))

;;;; repeat-mode

;; Evaluate `describe-repeat-maps' to see all repeatable commands.
(setup repeat
  (:hook emacs-startup-hook repeat-mode) ; run in startup hook to show message
  (setopt repeat-exit-key "<escape>"
          ;; repeat-exit-timeout 5
          repeat-check-key nil)
  ;; ;; Disable repeating for following commands
  ;; (put 'tab-next     'repeat-map nil)
  ;; (put 'tab-previous 'repeat-map nil)
  )

;;;; russian language

(setopt default-input-method 'russian-computer)
(prefer-coding-system 'cp1251)
(prefer-coding-system 'utf-8)
(keymap-global-set "C-v" 'toggle-input-method)

;;;; separedit

(setup separedit
  (:install t)
  (setopt separedit-default-mode 'org-mode ;; 'markdown-mode
          separedit-preserve-string-indentation t
          separedit-continue-fill-column t
          separedit-write-file-when-execute-save nil
          separedit-remove-trailing-spaces-in-comment t)
  ;; Key binding for modes you want edit or simply bind ‘global-map’ for all.
  (:global-bind :state 'normal
    "z '" 'separedit)
  ;; (:with-keymaps (prog-mode-map
  ;;                 minibuffer-local-map
  ;;                 help-mode-map)
  ;;   (:bind :state 'normal
  ;;     "z '" 'separedit))
  ;; (:with-feature helpful
  ;;   ;; helpful-mode-map
  ;;   (:after-load
  ;;     (:bind :state 'normal
  ;;       "z '" 'separedit)))
  ;; (:with-feature obsidian
  ;;   (:after-load
  ;;     ;; obsidian-mode-map
  ;;     (:bind :state 'normal
  ;;       "z '" 'separedit)))
  (:after-load
    (:with-keymap separedit-mode-map
      (:bind
        [remap edit-indirect-commit] 'separedit-commit
        [remap edit-indirect-abort]  'separedit-abort
        [remap save-buffer]          'separedit-save))))

;;;; emacs-server

(setup server
  (:require t)
  (:when (display-graphic-p))
  (unless (server-running-p) (server-start)))

;;;; fill-paragraph

(setup my-commands
  (:global-bind :state 'normal
    [remap fill-paragraph] '+flex-fill-paragraph))

(setup inflow
  (:install inflow :host github :repo "eshrh/inflow.el")
  (setopt inflow-fill-paragraph-width 80)
  (:command inflow-fill-paragraph))

(setup fancy-fill-paragraph
  (:install t)
  (setopt fancy-fill-paragraph-split-weights '( :em-dash 10
                                                :en-dash 10
                                                :space 10))
  (:command fancy-fill-paragraph))

;;;; DISABLE treesit-auto

;; (setup treesit-auto
;;   (:straight t)
;;   (:require t)
;;   (setopt treesit-auto-install 'prompt)
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;;; Org-mode
;;;; org-mode

(setup org
  ;; Following variables must be set before `org' is loaded!
  (setopt org-directory (expand-file-name "~/notes/")
          ;; Which modules to load.
          ;; Place cursor on variable and press "M" to see all possible values.
          org-modules '(ol-bibtex ol-docview ol-info)
          org-mem-watch-dirs '("~/notes/" "~/Private/"))
  (:require helheim-org
            helheim-daily-notes)
  (:hook org-mode-hook (lambda ()
                         (auto-fill-mode +1) ;; Hard wrap long lines.
                         (display-line-numbers-mode -1)
                         (visual-line-mode -1)))
  (:after-load
    (hel-keymap-set org-mode-map :state 'normal
      "M" 'helpful-at-point))
  ;; (setopt org-M-RET-may-split-line '((default . t)))
  (setopt org-src-tab-acts-natively t
          org-src-preserve-indentation nil
          org-src-content-indentation 0
          org-src-window-setup 'current-window
          ;; org-indirect-buffer-display 'current-window
          org-list-allow-alphabetical t
          org-use-property-inheritance t ; Properties apply also for sublevels.
          ;; org-log-into-drawer t
          ;; org-log-done 'time ; Track time when tasks were finished.
          org-log-redeadline 'note
          org-log-reschedule nil
          org-deadline-warning-days 14
          org-blank-before-new-entry '((heading . auto)
                                       (plain-list-item . auto)))
  (setopt org-startup-folded 'show2levels ; Initial visibility
          org-startup-indented t
          org-tags-column 0 ;; -80 ;; Прижимать тэги к 80 колонке справа.
          ;; Enclose text in "{}" after "_" to make it treated as subscript.
          org-use-sub-superscripts '{}
          ;; org-fontify-whole-heading-line t
          ;; org-fontify-quote-and-verse-blocks nil
          ;; org-level-color-stars-only nil
          )
  ;; (setopt org-todo-keywords
  ;;         '((sequence "󰒅" "󰄱" "󰡖" "" "|" "󰄵" "󱈎" "󰅘") ;; 󰔌 󱗝 󰜄 󰤌
  ;;           ;; (sequence "󰃃" "" "|" "󱍻")
  ;;           ))
  ;;
  ;; BUG: `org-priority-valid-value-p' is defined on 11376 line, but used for
  ;;   the first time on 2500 line in the org.el file.
  (:after-load
    ;; Make priority signs be integers from 1 to 5, with 3 as default.
    ;; Default priorities are: #A, #B, #C, with #B as default.
    (setopt org-priority-highest ?A
            org-priority-lowest  ?D
            org-priority-default ?C))
  ;; Consider all nested entries in the subtree for cookies.
  ;; [[info:org#Breaking Down Tasks]]
  (setopt org-hierarchical-todo-statistics nil)
  ;; --- tags ---
  ;; (setopt org-use-tag-inheritance nil)
  (setopt org-tags-match-list-sublevels nil)
  (:after-load
    (add-to-list 'org-tags-exclude-from-inheritance "00")
    ;; (cl-callf -snoc org-tags-exclude-from-inheritance "00")
    )
  ;; --- footnotes ---
  (setopt org-footnote-define-inline nil
          org-footnote-auto-adjust t)
  ;; --- LaTeX preview ---
  (setopt org-preview-latex-default-process 'xelatex)
  ;; --- org-attach ---
  (setopt org-file-apps '((system . "xdg-open %s")
                          ("\\.pdf\\'" . system)
                          ("\\.djvu?\\'" . system)
                          (directory . system)
                          (auto-mode . emacs)
                          ("\\.x?html?\\'" . default)))
  ;; --- babel ---
  (setopt org-babel-load-languages '((sql . t)
                                     (shell . t)
                                     (emacs-lisp . t)
                                     (python . t)
                                     (plantuml . t))
          ;; Allow babel code execution without confirming it every time.
          org-confirm-babel-evaluate nil
          ;; Use PlantUML executable instead of `.jar' file together with Java.
          org-plantuml-exec-mode 'plantuml
          org-plantuml-jar-path (expand-file-name "~/.nix-profile/lib/plantuml.jar"))
  ;; --- Capture templates ---
  ;; (setopt org-capture-templates '(("j" "journal" plain
  ;;                                  (file+olp+datetree +org-capture-journal-file)
  ;;                                  "%?"
  ;;                                  :empty-lines-before 1
  ;;                                  ;; :kill-buffer t
  ;;                                  )))
  )

;;;; Org appearence

(setup org-superstar
  (:install t)
  (:after org)
  (:hook org-mode-hook org-superstar-mode)
  (setopt org-superstar-remove-leading-stars nil
          org-superstar-headline-bullets-list '("●")
          ;; org-superstar-leading-bullet
          ;; • ◦ ‣ ￭ ■ ⋄ ○ □ ▬ ▶ ▸ ◂ ◆
          org-superstar-item-bullet-alist '((?- . ?•)
                                            (?+ . ?◦)
                                            (?* . ?‣))))
(setup org-pretty-tags
  (:install t)
  (:after org)
  (:blackout t)
  (:hook org-mode-hook org-pretty-tags-mode)
  (:after-load
    ;; :attach: 󰏢  󰁦
    ;; :link:     󰌷    󰌹 
    ;; :emacs:   
    ;; :cpp:      󰙲
    ;; :git:     󰊢
    (setopt org-pretty-tags-surrogate-strings '(("attach" . "󰏢")
                                                ("ATTACH" . "󰏢")
                                                ;; ("emacs" . "")
                                                ("link" . "")
                                                ("cpp" . "󰙲")))))
(setup org-appear
  (:install t)
  (:after org)
  (:hook org-mode-hook org-appear-mode)
  (setopt org-hide-emphasis-markers t))

;; Prettify symbols mode
;; ("TODO" . "")
;; ("WAIT" . "")
;; ("NOPE" . "")
;; ("DONE" . "")
;; ("[#A]" . "")
;; ("[#B]" . "")
;; ("[#C]" . "")
;; ("[ ]" . "")
;; ("[X]" . "")
;; ("[-]" . "")
;; ("#+STARTUP:" . "")
;; ("#+TITLE: " . "")
;; ("#+ROAM_TAGS:" . "")
;; ("#+FILETAGS:" . "")
;; ("#+HTML_HEAD:" . "")
;; ("#+SUBTITLE:" . "")
;; ("#+AUTHOR:" . "")
;; (":Effort:" . "")
;; ("SCHEDULED:" . "")
;; ("DEADLINE:" . "")
;; ("#+header:" . ?) ;; 
;; ("#+name:" . ?) ;; 
;; ("#+results:" . ?) ;;    󰂓 󰐟 󰩷
;; ("#+call:" . ?)
;; (":properties:" . ?) ;;  
;; (":logbook:" . ?)
;; (":end:" . "―")

;;;; LaTeX

;; LaTeX previews
(setup org-fragtog
  (:install t)
  (:after org)
  (:hook org-mode-hook org-fragtog-mode)
  (setopt org-startup-with-latex-preview t
          org-format-latex-options (-> org-format-latex-options
                                       (plist-put :scale 0.8)
                                       ;; (plist-put :foreground 'auto)
                                       ;; (plist-put :background 'auto)
                                       )))

;;;; org-tempo

;; Org 9.2 introduced a new template expansion mechanism, combining
;; `org-insert-structure-template' bound to "z," (default binding "C-c C-,").
;; The previous `easy-templates' mechanism (<s Tab) should be enabled manualy.
;; For more information, refer to the commentary section in `org-tempo.el'.
;;
;; Type `<se Tab' to insert emacs-lisp source code block,
;; type `<sh Tab' to insert bash source block and so on.
(setup org-tempo
  (:after org)
  (:require t)
  ;; Elements of length one have a tab appended. Elements of length two are
  ;; kept as is. Longer elements are truncated to length two. If an element
  ;; cannot be made unique, an error is raised.
  (setopt org-structure-template-alist `(("se" . "src emacs-lisp")
                                         ("sh" . "src sh")
                                         ("sc" . "src cpp")
                                         ("sf" . "src fennel")
                                         ("sl" . "src common-lisp")
                                         ("sm" . "src markdown")
                                         ;; ("sr" . "src rust")
                                         ("sp" . "src python")
                                         ("su" . "src lua")
                                         ,@org-structure-template-alist)))

;;;; org-node

(setup org-node
  (require 'helheim-org-node)
  (setopt org-mem-do-warn-title-collisions nil)
  (setq my-private-directory (expand-file-name "~/Private/"))
  (:global-bind
    "C-c n n"  '("notes" . my-org-node-find)
    "C-c n p"  '("private notes" . my-org-node-private-find)))

;;;; org-journal

(setup org-journal
  (:install t)
  (setopt
   ;; When switching from daily to weekly, monthly, yearly, or from weekly,
   ;; monthly, yearly to daily, you need to invalidate the cache. This has
   ;; currently to be done manually by calling `org-journal-invalidate-cache'.
   org-journal-file-type 'monthly
   org-extend-today-until 4
   org-journal-date-format "%x, %A")) ;; "DATE, WEEKDAY"

;;;; org-auto-tangle

(setup org-auto-tangle
  (:install t)
  (:blackout t)
  (:hook org-mode-hook org-auto-tangle-mode))

;;;; zotero integration

;; Redirect `zotero:' links to the system for handling
(with-eval-after-load 'org
  (org-link-set-parameters
   "zotero"
   :follow (lambda (zpath)
             (browse-url (format "zotero:%s" zpath)))))

;;;; DISABLED scrolling over images

;; (setup org-sliced-images
;;   (:install t)
;;   (:after org)
;;   (org-sliced-images-mode)) ;; global minor mode

;;;; DISABLED org-supertag

;; (setup posframe (:install t))
;;
;; (setup org-supertag
;;   (:install org-supertag :host github :repo "yibie/org-supertag")
;;   ;; Single vault
;;   (setopt org-supertag-sync-directories '("~/Private/"))
;;   ;; ;; Multiple vaults (separate DB/state per directory)
;;   ;; (setopt org-supertag-sync-directories '("~/notes/" "~/Private/")
;;   ;;         org-supertag-sync-directories-mode 'vaults)
;;   )

;;; Major modes

(require 'helheim-emacs-lisp)
(require 'helheim-json)
(require 'helheim-rust)

(setup text-mode
  (:hook text-mode-hook (lambda ()
                          (setq fill-column 76))))

(setup markdown
  (:require helheim-markdown)
  (setopt
   ;; Command to call standalone Markdown previewer
   markdown-open-command nil
   ;; Command to open image link via `markdown-follow-*' commands
   markdown-open-image-command nil
   markdown-asymmetric-header nil
   ;; markdown-marginalize-headers t
   markdown-list-item-bullets '("●" "◎" "○" "◆" "◇" "►" "•")
   ;; markdown-code-lang-modes
   ;; markdown-link-space-sub-char " "
   markdown-enable-math t
   markdown-reference-location 'subtree
   ;; markdown-hide-markup t
   markdown-hide-urls t
   ;; markdown-enable-wiki-links t
   ;; markdown-wiki-link-fontify-missing t
   ;; markdown-wiki-link-search-type 'project
   ))

(setup sh
  (:require helheim-sh)
  (setopt sh-basic-offset 2)
  (:hook sh-base-mode-hook (lambda () (setq tab-width 2))))

(setup fish-mode
  (:install t))

(setup yaml-pro
  (:install t)
  (:hook yaml-ts-mode-hook yaml-pro-ts-mode))

(setup add-log
  (setopt add-log-keep-changes-together t
          add-log-dont-create-changelog-file nil)
  (:global-bind
    "C-c p a" '("Add ChangeLog" . add-change-log-entry-other-window)) ; "C-x 4 a"
  (:after-load
    (:with-keymap change-log-mode-map
      (:bind :state 'normal
        "] c" 'add-log-edit-next-comment
        "[ c" 'add-log-edit-prev-comment))))

;;; Keybindings

(require 'helheim-keybindings)
(require 'helheim-disable-isearch)

(setup emacs
  (:global-unbind
    "C-k"  ;; `kill-line'
    "M-j") ;; `default-indent-new-line'
  (:global-bind
    "M-;"   'eval-expression
    "C-M-;" 'repeat-complex-command))

(setup hel
  (:global-bind :state '(normal motion)
    "<backspace>" 'execute-extended-command)
  (:global-bind :state 'normal
    "M-;"  nil ;; unbind `hel-exchange-point-and-mark'
    "C-;" 'hel-exchange-point-and-mark
    "g s" 'hel-beginning-of-line-command
    "g h" 'hel-first-non-blank)
  ;; (:global-bind :state 'insert
  ;;   "C-"h   'delete-backward-char
  ;;   "C-/" 'dabbrev-expand)
  (:with-keymap hel-window-map ;; "C-w"
    (:bind "N" 'other-tab-prefix))
  (:with-keymaps (prog-mode-map
                  text-mode-map)
    (:bind :state 'insert
      "C-h" 'backward-delete-char-untabify)))

(with-eval-after-load 'corfu
  (hel-keymap-set corfu-map
    "C-l" 'corfu-insert-separator))

;;; init.el ends here

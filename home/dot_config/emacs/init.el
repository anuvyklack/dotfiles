;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Fonts
;;
;; Set up fonts before other configuration so error messages during startup stay
;; readable.
;;
;; Place cursor before character and press "ga" to see information about it.
;; Press "<F1> k g a" to find out which command is bound to "ga".

(require 'cl-macs)

(cl-defun helheim-set-fontset-font (font charsets &key (fontset t) add)
  "Force some code point diapasons to use particular FONT."
  (declare (indent 1))
  (dolist (charset charsets)
    (set-fontset-font fontset charset font nil add)))

(progn
  (setq use-default-font-for-symbols t)
  (let ((font
         (font-spec :family "PragmataPro Liga" :size 13.9)
         ;; (font-spec :family "Hack" :size 13.0)
         ;; (font-spec :family "Cascadia Code" :size 13.0 :weight 'normal)
         ;; (font-spec :family "TX-02" :size 12.8)
         ))
    (set-face-font 'default font)
    (set-face-font 'fixed-pitch font)

    ;; General Punctuation Unicode Block
    ;; ---------------------------------
    ;;   When text is bold or italic, Emacs falls back to other fonts if the
    ;; main one doesn’t have the required glyphs for those styles. Force Emacs
    ;; to use the main font for punctuation.
    ;;
    ;;   These are all the glyphs, so you can quickly see which ones your font
    ;; supports:
    ;;  ‐ ‑ ‒ – — ― ‖ ‗
    ;; ‘ ’ ‚ ‛ “ ” „ ‟
    ;; † ‡ • ‣ ․ ‥ … ‧ ‰ ‱ ′ ″ ‴ ‵ ‶ ‷ ‸ ‹ ›
    ;; ※ ‼ ‽ ‾ ‿ ⁀ ⁁ ⁂ ⁃ ⁄ ⁅ ⁆ ⁇ ⁈ ⁉ ⁊ ⁋ ⁌ ⁍
    ;; ⁎ ⁏ ⁐ ⁑ ⁒ ⁓ ⁔ ⁕ ⁖ ⁗ ⁘ ⁙ ⁚ ⁛ ⁜ ⁝ ⁞
    (helheim-set-fontset-font font '((#x2010 . #x205e)))))

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
(helheim-set-fontset-font "Symbols Nerd Font"
  `(;; Powerline Symbols
    (#xe0a0 . #xe0a2) ;;  
    (#xe0b0 . #xe0b3) ;;  
    ;; Powerline Extra Symbols
    (#xe0b4 . #xe0c8) ;;  
    (#xe0cc . #xe0d7) ;;  
    #xe0a3 #xe0ca))   ;;  

;; Restore some icons.
(helheim-set-fontset-font "PragmataPro Liga"
  ;; Font Awesome
  `(#xf0c5   ;; 
    #xf114   ;; 
    #xf115)) ;; 

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

;;; Helheim core

;; In case you use VPN. Emacs populates `url-proxy-services' variable from:
;; `https_proxy', `socks_proxy', `no_proxy' environment variables.
(setq url-proxy-services '(("socks" . "127.0.0.1:10808")
                           ("https" . "127.0.0.1:10809"))
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'helheim-core)

;;; Color theme

(use-package helheim-modus-themes
  ;; :config
  ;; (load-theme 'modus-operandi t)
  )

;; I can recommend `leuven' theme for org-mode work. It has so many nice little
;; touches to spruce up org-mode elements that some users switch to it from
;; their usual dark doom or modus themes when working on org-mode projects.
;;   You may try it with ": load-theme" then type "leuven".
(use-package leuven-theme :ensure t)

(use-package helheim-ef-themes
  :config
  (load-theme 'ef-light t))

;;; Helheim modules

(require 'helheim-keybindings)
(require 'hel-leader)

(require 'helheim-emacs-lisp)

(require 'helheim-tab-bar) ; Each tab represents a set of windows, as in Vim.
(require 'helheim-dired)   ; File-manager
(require 'helheim-ibuffer) ; Opened buffers menu.
(require 'helheim-outline-mode) ; See "Outline Mode" in Emacs manual.
(require 'helheim-git)

(require 'helheim-corfu)   ; Code completion
(require 'helheim-vertico) ; Emacs version of command pallet
(require 'helheim-consult) ; A set of search commands with preview
(require 'helheim-embark)  ; Hard to explain :)

(require 'helheim-deadgrep) ; Interface to Ripgrep
;; (require 'helheim-edit-indirect) ; Alternative "zn" binding
(require 'helheim-chezmoi) ; Integration with chezmoi dotfile manager

;;;; Org mode

;; The `org-directory' variable must be set before `helheim-org' loaded!
(setopt org-directory (expand-file-name "~/notes/"))

;; Which modules to load. Place cursor on variable and press "M" to see
;; all possible values.
(setq org-modules '(ol-bibtex ol-docview ol-info))

(setopt org-cliplink-max-length nil
        org-cliplink-ellipsis "…"
        org-cliplink-secrets-path (expand-file-name ".org-cliplink-secrets.el"
                                                    org-directory))

(require 'helheim-org)
(require 'helheim-org-node)
(require 'helheim-daily-notes)

;;; My config

;; User credentials. Some functionality uses this to identify you,
;; e.g. GPG configuration, email clients, file templates and snippets.
(setopt user-full-name "Yuriy Artemyev"
        user-mail-address "anuvyklack@gmail.com")

(setopt confirm-kill-emacs nil
        what-cursor-show-names t
        ibuffer-expert t)

;; ;; See also `search-invisible'
;; (global-reveal-mode)

;;;; Appearance
;;;;; Colorize strings that represent colors

(use-package rainbow-mode
  :ensure t
  :blackout t
  :hook (emacs-lisp-mode-hook
         conf-mode-hook
         fish-mode-hook
         toml-ts-mode-hook))

;;;;; DISABLED show time in tab bar

;; (use-package time
;;   :custom
;;   (display-time-24hr-format t)
;;   (display-time-use-mail-icon t)
;;   :hook (elpaca-after-init-hook . display-time-mode))

;;;; Window managment

;; (add-to-list 'display-buffer-alist
;;              '((major-mode . org-mode)
;;                (display-buffer-reuse-mode-window display-buffer-same-window)
;;                (mode . org-mode)
;;                (body-function . select-window)))

;;;; Minibuffer & Completion
;;;;; cape

;; (hel-keymap-global-set :state 'insert
;;   ;; Emulate Vim's omni-completion keybinds
;;   "C-x" #'cape-prefix-map)
;;
;; (hel-keymap-set cape-prefix-map
;;   "C-o" 'completion-at-point ;; C-x C-o is Vim's omni-completion keybinding
;;   ;; "C-e" 'cape-elisp-block
;;   ;; "C-s" 'cape-elisp-symbol
;;   "/" 'cape-tex
;;   "C-/" 'cape-tex
;;   "C-h" 'cape-history
;;   "C-l" 'cape-line
;;   "C-k" 'cape-keyword
;;   "C-f" 'cape-file
;;   "C-t" 'complete-tag
;;   "C-w" 'cape-dict
;;   "C-r" 'cape-rfc1345
;;   ;; "s"   'cape-dict
;;   ;; "C-s" 'yasnippet-capf
;;   "C-a" 'cape-abbrev
;;   "C-d" 'cape-dabbrev
;;   "C-n" 'cape-dabbrev
;;   ;; "C-p" '+corfu/dabbrev-this-buffer
;;   )

;;;; Extra facilities
;;;;; project.el

(hel-keymap-set project-prefix-map
  "b" 'project-list-buffers)

;;;;; dired

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

;;;;;;  Extra highlighting

;; (use-package dired-rainbow
;;   :ensure t
;;   :after dired
;;   :config
;;   (progn
;;     (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
;;     (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
;;     (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
;;     (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
;;     (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
;;     (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
;;     (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
;;     (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
;;     (dired-rainbow-define log "#c17d11" ("log"))
;;     (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
;;     (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
;;     (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
;;     (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
;;     (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
;;     (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
;;     (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
;;     (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
;;     (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
;;     (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
;;     (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
;;     ))

;;;;; magit

(setopt magit-diff-refine-hunk 'all)

;;;;; repeat-mode

;; Evaluate `describe-repeat-maps' to see all repeatable commands.
(use-package repeat
  :hook (emacs-startup-hook . repeat-mode) ; run in startup hook to show message
  :custom
  (repeat-exit-key "<escape>")
  ;; (repeat-exit-timeout 5)
  (repeat-check-key nil)
  ;; :config
  ;; ;; Disable repeating for following commands
  ;; (put 'tab-next     'repeat-map nil)
  ;; (put 'tab-previous 'repeat-map nil)
  )

;;;;; Russian language

(setopt default-input-method 'russian-computer)

(hel-keymap-global-set
  "C-v" 'toggle-input-method)

;;;; Org-mode
;;;;; Variables
;;;;;; General settings

(setopt org-mem-watch-dirs '("~/notes/" "~/notes-old/" "~/Private/"))

(setopt
 ;; org-M-RET-may-split-line '((default . t))

 ;; Indentation for the content of a source code block.
 org-src-tab-acts-natively t
 org-src-preserve-indentation nil
 org-edit-src-content-indentation 0

 ;; org-log-state-notes-into-drawer t
 ;; org-log-into-drawer t

 ;; org-indirect-buffer-display 'current-window
 org-list-allow-alphabetical t

 org-use-property-inheritance t ; Properties apply also for sublevels.

 ;; org-log-done 'time ; Track time when tasks were finished.
 org-deadline-warning-days 14
 org-log-redeadline 'note
 org-log-reschedule nil
 org-blank-before-new-entry '((heading . auto)
                              (plain-list-item . auto)))

;;;;;; appearence

(setopt org-startup-folded 'show2levels ; Initial visibility
        org-startup-indented t)

;; (setopt org-tags-column -80) ;; Прижимать тэги к 80 колонке справа.
(setopt org-tags-column 0)

;; (setopt org-fontify-whole-heading-line t
;;         org-fontify-quote-and-verse-blocks nil)

;; Enclose text in "{}" after "_" to make it treated as subscript.
(setopt org-use-sub-superscripts '{})

;;;;;; org-attach

(setopt org-file-apps '((system . "xdg-open %s")
                        ("\\.pdf\\'" . system)
                        ("\\.djvu?\\'" . system)
                        (directory . system)
                        (auto-mode . emacs)
                        ("\\.x?html?\\'" . default)))

;;;;;; Capture templates

(setq org-capture-templates
      '(("j" "journal" plain
         (file+olp+datetree +org-capture-journal-file)
         "%?"
         :empty-lines-before 1
         ;; :kill-buffer t
         )))

;;;;;; babel

(setopt
 ;; Open source block with `org-edit-special' in the same window.
 org-src-window-setup 'current-window

 ;; Allow babel code execution without confirming it every time.
 org-confirm-babel-evaluate nil

 ;; Available embedded languages for babel.
 org-babel-load-languages '((sql . t)
                            (shell . t)
                            (emacs-lisp . t)
                            (python . t)
                            (plantuml . t))

 ;; Use PlantUML executable instead of `.jar' file together with Java.
 org-plantuml-exec-mode 'plantuml
 org-plantuml-jar-path (expand-file-name "~/.nix-profile/lib/plantuml.jar"))

;;;;;; footnotes

(setq org-footnote-define-inline nil
      org-footnote-auto-adjust t)

;;;;;; TODO keywords and Priorities

;; (setq org-todo-keywords
;;       '((sequence "󰒅" "󰄱" "󰡖" "" "|" "󰄵" "󱈎" "󰅘") ;; 󰔌 󱗝 󰜄 󰤌
;;         ;; (sequence "󰃃" "" "|" "󱍻")
;;         ))

;; Make priority signs be integers from 1 to 5, with 3 as default.
;; Default priorities are: #A, #B, #C, with #B as default.
(setq org-priority-highest ?A
      org-priority-lowest  ?D
      org-priority-default ?C)

;; Consider all nested entries in the subtree for cookies.
;; [[info:org#Breaking Down Tasks]]
(setq org-hierarchical-todo-statistics nil)

;;;;;; tags

;; (setq org-use-tag-inheritance nil)
(setq org-tags-match-list-sublevels nil)

(with-eval-after-load 'org
  (cl-callf append org-tags-exclude-from-inheritance
    '("00")))

;;;;; Org files appearence

;; (setq org-level-color-stars-only t)

;; • ◦ ‣ ￭ ■ ⋄ ○ □ ▬ ▶ ▸ ◂ ◆
(use-package org-superstar
  :ensure t
  :hook (org-mode-hook . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars nil)
  (org-superstar-headline-bullets-list '("●"))
  ;; (org-superstar-leading-bullet)
  (org-superstar-item-bullet-alist '((?- . ?•)
                                     (?+ . ?◦)
                                     (?* . ?◆))))

(use-package org-pretty-tags
  :ensure t
  :blackout t
  :hook (org-mode-hook . org-pretty-tags-mode)
  :custom
  ;; :attach: 󰏢  󰁦
  ;; :link:     󰌷    󰌹 
  ;; :emacs:   
  ;; :cpp:      󰙲
  ;; :git:     󰊢
  (org-pretty-tags-surrogate-strings '(("attach" . "󰏢")
                                       ("ATTACH" . "󰏢")
                                       ;; ("emacs" . "")
                                       ("link" . "")
                                       ("cpp" . "󰙲"))))

(use-package org-appear
  :ensure t
  :hook (org-mode-hook . org-appear-mode)
  :custom (org-hide-emphasis-markers t))

;;;;;; Prettify symbols mode

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

;;;;; LaTeX

;; LaTeX previews
(use-package org-fragtog
  :ensure t
  :hook (org-mode-hook . org-fragtog-mode)
  :config
  (setopt org-startup-with-latex-preview t
          org-format-latex-options (-> org-format-latex-options
                                       (plist-put :scale 0.8)
                                       ;; (plist-put :foreground 'auto)
                                       ;; (plist-put :background 'auto)
                                       )))

;;;;; org-tempo

;; Org 9.2 introduced a new template expansion mechanism, combining
;; `org-insert-structure-template' bound to "z," (default binding "C-c C-,").
;; The previous `easy-templates' mechanism (<s Tab) should be enabled manualy.
;; For more information, refer to the commentary section in `org-tempo.el'.
;;
;; Type `<se Tab' to insert emacs-lisp source code block,
;; type `<sh Tab' to insert bash source block and so on.
(use-package org-tempo
  :after org
  :config
  ;; Elements of length one have a tab appended. Elements of length two are
  ;; kept as is. Longer elements are truncated to length two. If an element
  ;; cannot be made unique, an error is raised.
  (setq org-structure-template-alist `(("se" . "src emacs-lisp")
                                       ("sh" . "src sh")
                                       ("sc" . "src cpp")
                                       ("sf" . "src fennel")
                                       ("sl" . "src common-lisp")
                                       ("sm" . "src markdown")
                                       ;; ("sr" . "src rust")
                                       ("sp" . "src python")
                                       ("su" . "src lua")
                                       ,@org-structure-template-alist)))

;;;;; org-node

(setopt helheimg-org-node-visit-backlink-in-another-window nil)

;;;;; org-journal

(use-package org-journal
  :ensure t
  :defer t
  :custom
  ;; When switching from daily to weekly, monthly, yearly, or from weekly,
  ;; monthly, yearly to daily, you need to invalidate the cache. This has
  ;; currently to be done manually by calling `org-journal-invalidate-cache'.
  (org-journal-file-type 'monthly)
  (org-extend-today-until 4)
  (org-journal-date-format "%x, %A")) ;; "DATE, WEEKDAY"

;;;;; org-auto-tangle

(use-package org-auto-tangle
  :ensure t
  :hook (org-mode-hook . org-auto-tangle-mode))

;;;;; zotero integration

;; Redirect `zotero:' links to the system for handling
(with-eval-after-load 'org
  (org-link-set-parameters "zotero"
                           :follow (lambda (zpath)
                                     (browse-url (format "zotero:%s" zpath)))))

;;;;; DISABLED scrolling over images
;; Doesn't work properly.

;; (use-package org-sliced-images
;;   :ensure t
;;   :after org
;;   :config (org-sliced-images-mode)) ;; It is global minor mode.

;;;; Major modes
;;;;; org-mode

(add-hook 'org-mode-hook #'auto-fill-mode) ; Hard wrap long lines.

(add-hook 'org-mode-hook (lambda ()
                           (display-line-numbers-mode -1)
                           (visual-line-mode -1)))

;;;;; fish

(use-package fish-mode :ensure t)

;;;; Keybindings

(use-package hel-leader
  :custom
  (hel-leader-send-C-x-with-control-modifier nil))

(hel-keymap-global-set
  "M-;"   'eval-expression
  "C-M-;" 'repeat-complex-command)

(hel-keymap-global-set :state '(normal motion)
  "<backspace>" 'execute-extended-command)

(hel-keymap-global-set :state 'normal
  "M-;"  nil ; unbind `hel-exchange-point-and-mark'
  "C-;" 'hel-exchange-point-and-mark)

;; "C-w"
(hel-keymap-set hel-window-map
  "N" 'other-tab-prefix)

(hel-keymap-global-set :state 'insert
  "C-w" 'backward-kill-word ; along with "C-backspace"
  ;; "C-h"   'delete-backward-char
  ;; "C-/" 'dabbrev-expand
  )

(hel-keymap-set corfu-map
  "C-l" 'corfu-insert-separator)

(hel-keymap-set org-mode-map :state 'normal
  "M" 'helpful-at-point)

;; (hel-keymap-global-set
;;   "<remap> <find-file>" 'find-file-at-point
;;   "<remap> <find-file-read-only>" 'ffap-read-only
;;   "<remap> <find-alternate-file>" 'ffap-alternate-file
;;   "<remap> <dired>" 'dired-at-point
;;   "<remap> <dired-other-window>" 'ffap-dired-other-window)

;;; init.el ends here

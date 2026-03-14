;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; GUI

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;;; Fonts
;;
;; Set up fonts before anything else so error messages during startup were
;; readable.
;;
;; Place cursor before character and press "ga" to see information about it.
;; Press "<F1> k g a" to find out which command is bound to "ga".

;; `face-font-family-alternatives'
(let* ((font "PragmataPro Liga")
       (spec (font-spec :family font :size 13.9 :weight 'normal))
       ;; (font "Hack")
       ;; (spec (font-spec :family "Hack" :size 13.0))
       ;; (font "Cascadia Code")
       ;; (spec (font-spec :family font :size 13.0 :weight 'normal))
       ;; (font "TX-02")
       ;; (spec (font-spec :family "TX-02" :size 12.8))
       )
  (set-face-font 'default spec)
  (set-face-font 'fixed-pitch spec)
  ;; Prepend our font to the "fontset-default" to make it the first fallback
  ;; candidate for itself. This matters when text is bold or italic and the
  ;; default font lacks glyphs for those styles but does provide them for the
  ;; regular style. With this change, Emacs will use the regular glyphs from
  ;; the default font when bold or italic variants are unavailable, instead of
  ;; falling back to a different font.
  ;;   BUG: Using `font-spec' with `set-fontset-font' doesn't work, despite
  ;; documentation claims it is.
  (set-fontset-font t 'unicode font nil 'prepend))

;; General Punctuation Unicode Block
;;  ‐ ‑ ‒ – — ― ‖ ‗
;; ‘ ’ ‚ ‛ “ ” „ ‟
;; † ‡ • ‣ ․ ‥ … ‧ ‰ ‱ ′ ″ ‴ ‵ ‶ ‷ ‸ ‹ ›
;; ※ ‼ ‽ ‾ ‿ ⁀ ⁁ ⁂ ⁃ ⁄ ⁅ ⁆ ⁇ ⁈ ⁉ ⁊ ⁋ ⁌ ⁍
;; ⁎ ⁏ ⁐ ⁑ ⁒ ⁓ ⁔ ⁕ ⁖ ⁗ ⁘ ⁙ ⁚ ⁛ ⁜ ⁝ ⁞
;; №

(require 'cl-macs)

(cl-defun helheim-set-fontset-font (font charsets &key (fontset t) add)
  "Force some code point diapasons to use particular FONT."
  (declare (indent 1))
  (dolist (charset charsets)
    (set-fontset-font fontset charset font nil add)))

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

;; In case you use VPN. Also Emacs populates `url-proxy-services' variable
;; from: `https_proxy', `socks_proxy', `no_proxy' environment variables.
(setq url-proxy-services '(("socks" . "127.0.0.1:10808")
                           ("https" . "127.0.0.1:10809"))
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(when (< emacs-major-version 31)
  (load-file (expand-file-name "prepare-user-lisp.el" user-lisp-directory))
  (prepare-user-lisp))

(require 'helheim-straight)
(require 'helheim-core)

;;; Color theme

(leaf helheim-modus-themes
  :require t
  ;; :config (load-theme 'modus-operandi t)
  )

(leaf leuven-theme :straight t)

;; (leaf helheim-ef-themes
;;   :require t
;;   :config (load-theme 'ef-light t))

(leaf my-ef-themes
  :require t
  :config (load-theme 'ef-light t))

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

;; (leaf imenu-list
;;   :straight t
;;   :defer t)

;;; Helheim modules

(require 'helheim-emacs-lisp)
(require 'helheim-outline-mode) ; See "Outline Mode" in Emacs manual.

(require 'helheim-tab-bar)  ; Each tab represents a set of windows, as in Vim.
(require 'helheim-xref)     ; Go to defenition framework
(require 'helheim-dired)    ; File-manager
(require 'helheim-ibuffer)  ; Opened buffers menu.

(require 'helheim-chezmoi)  ; Integration with chezmoi dotfile manager
(require 'helheim-notmuch)
;; (require 'helheim-edit-indirect) ; Alternative "zn" binding

;;; Appearance
;;;; Colorize strings that represent colors

(leaf rainbow-mode
  :straight t
  :blackout t
  :hook (emacs-lisp-mode-hook
         conf-mode-hook
         fish-mode-hook
         toml-ts-mode-hook))

;;; Window managment

;; (add-to-list 'display-buffer-alist
;;              '((major-mode . org-mode)
;;                (display-buffer-reuse-mode-window display-buffer-same-window)
;;                (mode . org-mode)
;;                (body-function . select-window)))

;;; Minibuffer & Completion

(require 'helheim-corfu)    ; Code completion menus
(require 'helheim-vertico)  ; Emacs version of command pallet
(require 'helheim-consult)  ; A set of search commands with preview
(require 'helheim-deadgrep) ; Interface to Ripgrep
(require 'helheim-embark)   ; Context-aware action menus

;;;; cape

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

;;; IDE

(setopt eldoc-echo-area-use-multiline-p t)

(require 'helheim-eglot)

;;; Extra facilities
;;;; project.el

(hel-keymap-set project-prefix-map
  "b" 'project-list-buffers)

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

;;;;;  Extra highlighting

;; (leaf dired-rainbow
;;   :straight t
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

;;;; magit

(require 'helheim-diff-hl)

(leaf helheim-magit
  :require t
  :init
  (setopt magit-diff-refine-hunk 'all
          magit-repository-directories '(("~/.config/emacs/" . 0))))

;;;; repeat-mode

;; Evaluate `describe-repeat-maps' to see all repeatable commands.
(leaf repeat
  :hook (emacs-startup-hook . repeat-mode) ; run in startup hook to show message
  :custom
  (repeat-exit-key . "<escape>")
  ;; (repeat-exit-timeout . 5)
  (repeat-check-key . nil)
  ;; :config
  ;; ;; Disable repeating for following commands
  ;; (put 'tab-next     'repeat-map nil)
  ;; (put 'tab-previous 'repeat-map nil)
  )

;;;; separedit

(leaf separedit
  :straight t
  :init
  (setopt separedit-default-mode 'org-mode ;; 'markdown-mode
          separedit-preserve-string-indentation t
          separedit-continue-fill-column t
          separedit-write-file-when-execute-save nil
          separedit-remove-trailing-spaces-in-comment t)
  ;; :commands separedit
  :config
  ;; Key binding for modes you want edit or simply bind ‘global-map’ for all.
  (hel-keymap-global-set :state 'normal
    "z '" 'separedit)
  ;; (dolist (keymap (list prog-mode-map
  ;;                       minibuffer-local-map
  ;;                       help-mode-map
  ;;                       helpful-mode-map))
  ;;   (hel-keymap-set keymap :state 'normal
  ;;     "z '" 'separedit))
  ;; (with-eval-after-load 'obsidian
  ;;   (hel-keymap-set obsidian-mode-map :state 'normal
  ;;     "z '" 'separedit))
  :defer-config
  (hel-keymap-set edit-indirect-mode-map :state 'normal
    "Z Z" 'edit-indirect-commit
    "Z Q" 'edit-indirect-abort)
  (hel-keymap-set separedit-mode-map
    "<remap> <edit-indirect-commit>" 'separedit-commit
    "<remap> <edit-indirect-abort>"  'separedit-abort
    "<remap> <save-buffer>"          'separedit-save))

;;;; Russian language

(prefer-coding-system 'cp1251)
(prefer-coding-system 'utf-8)

(setq default-input-method 'russian-computer)

(hel-keymap-global-set
  "C-v" 'toggle-input-method)

;;; Org mode

(leaf org
  :require (helheim-org
            helheim-org-node
            helheim-daily-notes)
  :init
  (setopt org-directory (expand-file-name "~/notes/")
          ;; Which modules to load.
          ;; Place cursor on variable and press "M" to see all possible values.
          org-modules '(ol-bibtex ol-docview ol-info)
          org-mem-watch-dirs '("~/notes/" "~/Private/"))
  :config
  (hel-keymap-set org-mode-map :state 'normal
    "M" 'helpful-at-point))

;;;; Variables
;;;;; General settings

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

;;;;; appearence

(setopt org-startup-folded 'show2levels ; Initial visibility
        org-startup-indented t)

;; (setopt org-tags-column -80) ;; Прижимать тэги к 80 колонке справа.
(setopt org-tags-column 0)

;; (setopt org-fontify-whole-heading-line t
;;         org-fontify-quote-and-verse-blocks nil)

;; Enclose text in "{}" after "_" to make it treated as subscript.
(setopt org-use-sub-superscripts '{})

;;;;; org-attach

(setopt org-file-apps '((system . "xdg-open %s")
                        ("\\.pdf\\'" . system)
                        ("\\.djvu?\\'" . system)
                        (directory . system)
                        (auto-mode . emacs)
                        ("\\.x?html?\\'" . default)))

;;;;; Capture templates

(setq org-capture-templates
      '(("j" "journal" plain
         (file+olp+datetree +org-capture-journal-file)
         "%?"
         :empty-lines-before 1
         ;; :kill-buffer t
         )))

;;;;; babel

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

;;;;; footnotes

(setopt org-footnote-define-inline nil
        org-footnote-auto-adjust t)

;;;;; TODO keywords and Priorities

;; (setopt org-todo-keywords
;;         '((sequence "󰒅" "󰄱" "󰡖" "" "|" "󰄵" "󱈎" "󰅘") ;; 󰔌 󱗝 󰜄 󰤌
;;           ;; (sequence "󰃃" "" "|" "󱍻")
;;           ))

;; Make priority signs be integers from 1 to 5, with 3 as default.
;; Default priorities are: #A, #B, #C, with #B as default.
(setopt org-priority-highest ?A
        org-priority-lowest  ?D
        org-priority-default ?C)

;; Consider all nested entries in the subtree for cookies.
;; [[info:org#Breaking Down Tasks]]
(setopt org-hierarchical-todo-statistics nil)

;;;;; tags

;; (setopt org-use-tag-inheritance nil)
(setopt org-tags-match-list-sublevels nil)

(with-eval-after-load 'org
  (cl-callf append org-tags-exclude-from-inheritance '("00")))

;;;; Org files appearence

;; (setq org-level-color-stars-only t)

;; • ◦ ‣ ￭ ■ ⋄ ○ □ ▬ ▶ ▸ ◂ ◆
(leaf org-superstar
  :straight t
  :after org
  :hook (org-mode-hook . org-superstar-mode)
  :config
  (setopt org-superstar-remove-leading-stars nil
          org-superstar-headline-bullets-list '("●")
          ;; org-superstar-leading-bullet
          org-superstar-item-bullet-alist '((?- . ?•)
                                            (?+ . ?◦)
                                            (?* . ?◆))))

(leaf org-pretty-tags
  :straight t
  :after org
  :blackout t
  :hook (org-mode-hook . org-pretty-tags-mode)
  ;; :defer-config
  :config
  ;; :attach: 󰏢  󰁦
  ;; :link:     󰌷    󰌹 
  ;; :emacs:   
  ;; :cpp:      󰙲
  ;; :git:     󰊢
  (setopt org-pretty-tags-surrogate-strings '(("attach" . "󰏢")
                                              ("ATTACH" . "󰏢")
                                              ;; ("emacs" . "")
                                              ("link" . "")
                                              ("cpp" . "󰙲"))))

(leaf org-appear
  :straight t
  :after org
  :hook (org-mode-hook . org-appear-mode)
  :custom (org-hide-emphasis-markers . t))

;;;;; Prettify symbols mode

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
(leaf org-fragtog
  :straight t
  :after org
  :hook (org-mode-hook . org-fragtog-mode)
  :config
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
(leaf org-tempo
  :after org
  :require t
  :config
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

;;;; org-journal

(leaf org-journal
  :straight t
  :custom
  ;; When switching from daily to weekly, monthly, yearly, or from weekly,
  ;; monthly, yearly to daily, you need to invalidate the cache. This has
  ;; currently to be done manually by calling `org-journal-invalidate-cache'.
  (org-journal-file-type . 'monthly)
  (org-extend-today-until . 4)
  (org-journal-date-format . "%x, %A")) ;; "DATE, WEEKDAY"

;;;; org-auto-tangle

(leaf org-auto-tangle
  :straight t
  :hook (org-mode-hook . org-auto-tangle-mode))

;;;; zotero integration

;; Redirect `zotero:' links to the system for handling
(with-eval-after-load 'org
  (org-link-set-parameters "zotero"
                           :follow (lambda (zpath)
                                     (browse-url (format "zotero:%s" zpath)))))

;;;; DISABLED scrolling over images
;; Doesn't work properly.

;; (leaf org-sliced-images
;;   :straight t
;;   :after org
;;   :global-minor-mode org-sliced-images-mode) ;; It is global minor mode.

;;;; org-supertag

;; (leaf posframe :straight t)
;;
;; (leaf org-supertag
;;   :straight (org-supertag :host github :repo "yibie/org-supertag")
;;   :custom
;;   ;; Single vault
;;   (org-supertag-sync-directories . '("~/Private/"))
;;
;;   ;; ;; Multiple vaults (separate DB/state per directory)
;;   ;; (org-supertag-sync-directories . '("~/notes/" "~/Private/"))
;;   ;; (org-supertag-sync-directories-mode . 'vaults)
;;   )

;;; Major modes
;;;; change-log-mode

(hel-keymap-global-set
  "C-c p a" '("Add ChangeLog" . add-change-log-entry-other-window)) ;; "C-x 4 a"

(leaf add-log
  :config
  (setopt add-log-keep-changes-together t
          add-log-dont-create-changelog-file nil)
  ;; :defer-config
  ;; (hel-keymap-set change-log-mode-map :state 'normal
  ;;   "] c" 'add-log-edit-next-comment
  ;;   "[ c" 'add-log-edit-prev-comment)
  )

;;;; org-mode

(add-hook 'org-mode-hook #'auto-fill-mode) ; Hard wrap long lines.
(add-hook 'org-mode-hook (lambda ()
                           (display-line-numbers-mode -1)
                           (visual-line-mode -1)))

;;;; shell

(require 'helheim-sh)

;;;; markdown

(leaf helheim-markdown
  :require t
  :config (setopt
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

;;;; rust

(require 'helheim-rust)

;;;; yaml

(leaf yaml-pro
  :straight t
  :hook (yaml-ts-mode-hook . yaml-pro-ts-mode))

;;; Keybindings

(require 'hel-leader)
(require 'helheim-keybindings)
(require 'helheim-disable-isearch)

(hel-keymap-global-set
  "C-k"   nil ;; unbind `kill-line'
  "M-j"   nil ;; unbind `default-indent-new-line'
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
  ;; "C-"h   'delete-backward-char
  ;; "C-/" 'dabbrev-expand
  )

(hel-keymap-set corfu-map
  "C-l" 'corfu-insert-separator)

(hel-keymap-set lisp-mode-shared-map :state 'insert
  "C-h" 'backward-delete-char-untabify)

;;; init.el ends here

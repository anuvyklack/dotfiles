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

;; Use the Mono variant, rather than "PragmataPro Liga": the latter draws
;; horizontal arrows (→ ← ⇒) double-width while `char-width' still reports 1,
;; which silently shifts every column after them in an ASCII diagram.
(let* ((font "PragmataPro Mono Liga")
       (spec (font-spec :family font :size 13.9 :weight 'normal))
       ;; (font "PragmataPro Liga")
       ;; (spec (font-spec :family font :size 13.9 :weight 'normal))
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

;;;;; Nerd Icons

;; (setq nerd-icons-scale-factor 0.9)
(setq nerd-icons-default-adjust 0.1)

;; To pick PragmataPro built-in nerd icons uncomment `:add' argument.
;;
;; Above we have already prepend PragmataPro to every range of the default
;; fontset, so appending installs Symbols Nerd Font Mono behind it as the
;; fallback for the icons PragmataPro lacks. Emacs walks that list per
;; character, so each icon comes from PragmataPro when the font has one and
;; from the Nerd font otherwise.
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
    (#xf0001 . #xf1af0)) ;; Material Design Icons 󰀁 󱫰
  ;; :add 'append
  )

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

;;;;; Box drawing

;; Box-drawing rules and arrowheads are drawn on two different vertical axes:
;; ─ is centred on the box grid, ► and → on the text/math axis.  Naively they
;; are ~1.3px apart, so the head of ───► hangs below its shaft.
;;
;; PragmataPro corrects this itself with a GPOS rule (upstream issue #159,
;; shipped in v0.829).  Since v0.9 the rule is *unconditional*: a pointer is
;; raised 149 font units whenever it is shaped, not only when a box-drawing
;; character sits next to it.  The outlines themselves were never moved, so
;; the raise is still what does the aligning — `hb-shape --show-extents'
;; puts the ink of ─ at 689..833 (centre 761) and of ► at 235..988 (centre
;; 611); only after +149 does ►'s centre land on 760.
;;
;; HarfBuzz applies the rule happily — but Emacs only hands a run to
;; HarfBuzz when `composition-function-table' has an entry for the run's
;; leading character, and box drawing has none.  So the two glyphs are
;; shaped in isolation and the correction never fires.
;;
;; A side effect of the rule becoming unconditional: a pointer with no
;; box-drawing character beside it is never composed here, so it stays 1px
;; lower in Emacs than in a HarfBuzz terminal.  Composing lone pointers too
;; would fix that; it is not done, since the diagrams are what matter.
;;
;; Teaching Emacs to shape these pairs is all that is needed.  It is a no-op
;; on fonts that lack the GPOS rule (Liberation Mono, Adwaita Mono, Noto Sans
;; Mono were checked — none of them correct it), so this is safe to keep even
;; if the default font changes.
(let* ((rules "─━╌╍┄┅┈┉│┃╎╏┆┇┊┋═║┌┐└┘├┤┬┴┼")
       (heads "►◄▶◀▸◂→←⇒⇐▼▲▾▿▴▵↓↑⇣⇡")
       (rule-rx (concat "[" rules "]"))
       (head-rx (concat "[" heads "]")))
  ;; rule, then head:  ───►
  (dolist (c (string-to-list rules))
    (set-char-table-range composition-function-table c
                          `([,(concat rule-rx "+" head-rx) 0 font-shape-gstring])))
  ;; head, then rule:  ◄───
  (dolist (c (string-to-list heads))
    (set-char-table-range composition-function-table c
                          `([,(concat head-rx rule-rx "+") 0 font-shape-gstring]))))

;;;; Helheim core

;; In case you use VPN. Also Emacs populates `url-proxy-services' variable
;; from: `https_proxy', `socks_proxy', `no_proxy' environment variables.
(setq url-proxy-services '(("socks" . "127.0.0.1:10808")
                           ("https" . "127.0.0.1:10809"))
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq helheim-package-manager 'elpaca) ;; or 'straight
(require 'helheim-core)

;;;; Color theme

(setopt custom-safe-themes t)

;; Helheim's own themes live next to their configuration rather than in
;; `var/themes', so `load-theme' has to be told where to look.
;; Note: `user-emacs-directory' has been redirected to var/ by this point,
;; so the path is built from `user-lisp-directory' instead.
(add-to-list 'custom-theme-load-path
             (expand-file-name "helheim/color-themes/" user-lisp-directory))

;; (setup helheim-modus-themes
;;   (:require t)
;;   ;; (load-theme 'modus-operandi t)
;;   (load-theme 'modus-vivendi t)
;;   )

(setup helheim-ef-light
  (:require t)
  (load-theme 'ef-light t))

;; ;; Self-contained: pulls in neither ef-themes nor modus-themes.  The palette
;; ;; lives in color-themes/blasphemous-source/ -- the theme file itself is
;; ;; generated and should not be edited by hand.
;; (load-theme 'blasphemous-light t)

;; Was `(require 'helheim-ef-light)'.  That file both installs and configures
;; ef-themes and ends by enabling `ef-light', so requiring it here would
;; enable a second theme underneath this one.  To go back to `ef-light',
;; restore the require in place of the `load-theme' above.

;; I can recommend `leuven' theme for org-mode work. It has so many nice little
;; touches to spruce up org-mode elements that some users switch to it from
;; their usual dark doom or modus themes when working on org-mode projects.
;;   You may try it with ": load-theme" then type "leuven".
(setup leuven-theme (:install t))

;; (setup pixel-themes
;;   (:install pixel-themes :host github :repo "lucasobx/pixel-themes")
;;   (pixel-themes-mode 1)
;;   (load-theme 'pixel-themes-alia16 t)
;;   (load-theme 'pixel-themes-gray-weather t)
;;   (load-theme 'pixel-themes-gothic-temple t))

;;;; Essentials

(require 'helheim-minibuffer) ; Emacs version of command palette
(require 'helheim-completion)

(require 'helheim-ibuffer)  ; Buffers menu
(require 'helheim-dired)    ; File-manager
(require 'helheim-embark)   ; Context-aware action menus
(require 'helheim-outline)  ; See "Outline Mode" in Emacs manual
(require 'helheim-tab-bar)  ; Each tab represents a set of windows, as in Vim

;;;;; Ligatures

;; PragmataPro's ligatures are width-preserving — each character of a
;; ligature maps to a private-use glyph that still advances exactly one cell
;; — so they cannot disturb the alignment of an ASCII diagram or an org
;; table.
;;
;; They are not fixed-length pairs, though.  The font's `calt' chain
;; rewrites a whole run of operator characters at once — "=|<$" is one
;; substitution, not "=|" followed by "<$" — so enumerating them is
;; hopeless: over the 28 characters involved, v0.9 has 59 ligatures two
;; characters long, 155 of three, 41 of four and 146 of five (brute-forced
;; with `hb-shape' against every sequence over those characters).  Hence a
;; regexp per starting character rather than a list of literals.  This is
;; the whole ligature set of the font, not a sample of it.
;;
;; What it still does not buy: the font caps plain arrows at three
;; characters, so "-->" ligates and "--->" does not.
;;
;; A different font would want a different rule — Fira Code and friends do
;; use fixed-length ligatures, and for those the literal-string form is the
;; right one:
;;
;;   (ligature-set-ligatures t '("->" "-->" "=>" "!=" ...))
;;
;; `ligature-generate-ligatures' installs a buffer-local
;; `composition-function-table' whose *parent* is the global one, so the box
;; drawing rules under "Fonts" keep working here through inheritance.

(defconst helheim-ligature-characters "!\"#$%&()*+-./:<=>?@[\\]^_{|}~"
  "Characters the main font can draw as part of a ligature.")

(defconst helheim-ligature-starting-characters "!#$%&(*+-./:<=>?[\\]^_{|~"
  "Characters that can begin a ligature.
The rest of `helheim-ligature-characters' only ever continue one.")

;; The display engine picks the characters of a run without consulting the
;; `invisible' property, so a run that begins on a visible character and reaches
;; into hidden text eats the first character past the hidden part.  With
;; `org-hide-emphasis-markers' the "*" of "(*VPN*)" is hidden, "(*" is a run,
;; and the "V" is never drawn:
;;
;;   buffer   ( * V P N        buffer   ( ) = ,
;;   screen   ( *   P N        screen   ( ) =
;;
;; Both ends of an emphasis are hidden, and the closing one is the harder half:
;; it only needs a non-blank character before it, so every "=mmap()=" or
;; "~arr[i]~" puts a hidden marker straight after punctuation, where a run is
;; already in progress.
;;
;; This cannot be fixed from `auto-composition-function': Emacs looks the run up
;; in a cache keyed by font and characters *before* calling it, so once "(*" has
;; been shaped anywhere — any buffer, any position — every later "(*" composes
;; without asking.  The fix has to be the pattern itself, which is consulted
;; first and per buffer.
;;
;; Hence the blunt rule: in org, a run may not contain a marker after its first
;; character.  Nothing else is decidable from a regexp — the character before a
;; hidden closing marker can be anything, so "!=" and "=foo!=" are the same two
;; characters and only one of them may compose.
;;
;; A marker may still *begin* a run, which is what keeps "=>" and "~>"; the
;; price is that a hidden *opening* marker still draws its half of the ligature,
;; the long-known "=>" that shows up as a bare arrowhead.
;;
;; Emphasis markers are not the only text org hides: with `org-link-descriptive'
;; a descriptive link hides "[[URL][" and the closing "]]", and "]]" is itself a
;; ligature in this font ("[[" "]]" "[|" "|]" "#[" are the bracket ligatures,
;; per `hb-shape').  So the closing brackets get pulled into a run started by
;; the last characters of the description, and are drawn along with it.  Below,
;; the tail of "…C++]]" and of "…/Instruction tables/]]":
;;
;;   buffer   C + + ] ]        buffer   s / ] ]
;;   drawn    C + + ] ]        drawn    s / ] ]
;;   wanted   C + +            wanted   s
;;                  ▲ ▲                   ▲ ▲ ▲
;;
;; On the left the run is "+]]", begun by the visible "+"; on the right it is
;; "/]]", begun by the hidden closing emphasis marker, which is why that "/"
;; shows up too.
;;
;; Brackets therefore have to be treated exactly like markers.  They stay in the
;; *starting* character list even though nothing can follow them any more: a
;; character absent from org's rule falls through to the global one (see
;; `ligature-generate-ligatures' — it fills one char-table from every matching
;; entry), which would put the unrestricted run back for "[" and "]".
;;
;; What this costs, per `hb-shape': in org buffers only, "<=" ">=" "!=" "=="
;; "+=" "-=" "/=" "~=" "<~", "=/=", and the five bracket ligatures above stop
;; ligating.  Kept: "->" "-->" "<-" "<->" "|->" "=>" "(|" "{|" "&&" "##" "?."
;; and everything else with no marker or bracket past its first character;
;; "//" and "**" were never ligatures in this font.
(defun helheim-ligature-restrict-org ()
  "Keep org's hidden text out of ligature runs.
In org buffers a ligature run may not contain an emphasis marker or a
link bracket after its first character."
  (let* ((hidden (append (-map (lambda (e) (string-to-char (car e)))
                               org-emphasis-alist)
                         (list ?\[ ?\])))
         (run (concat (regexp-opt-charset
                       (-difference (string-to-list helheim-ligature-characters)
                                    hidden))
                      "+")))
    (ligature-set-ligatures
     'org-mode
     (mapcar (lambda (c) (list (string c) run))
             (string-to-list helheim-ligature-starting-characters)))
    ;; `ligature-generate-ligatures' walks `ligature-composition-table' letting
    ;; later entries win, while `ligature-set-ligatures' pushes each new mode
    ;; onto the front — so move org's entry to the end rather than depending on
    ;; the order the two calls happened to run in.
    (when-let* ((entry (assq 'org-mode ligature-composition-table)))
      (setq ligature-composition-table
            (append (delq entry ligature-composition-table) (list entry))))))

(setup ligature
  (:install t)
  ;; A run is any of these characters followed by one or more of them.
  (let ((run (concat (regexp-opt-charset
                      (string-to-list helheim-ligature-characters))
                     "+")))
    (ligature-set-ligatures
     t (mapcar (lambda (c) (list (string c) run))
               (string-to-list helheim-ligature-starting-characters))))
  ;; Must run before `global-ligature-mode' builds any buffer's table below;
  ;; org buffers created later are safe because loading org runs this first.
  (with-eval-after-load 'org
    (helheim-ligature-restrict-org))
  (global-ligature-mode t))

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

;; (require 'helheim-eglot)    ; eglot + flymake (both built-in)
;; or
(require 'helheim-lsp-mode)
(require 'helheim-flycheck)

;;;; Version control

(setup helheim-magit
  (:require t)
  (setopt magit-diff-refine-hunk 'all
          magit-repository-directories '(("~/.config/emacs/" . 0)))
  (with-eval-after-load 'magit
    (magit-add-section-hook 'magit-status-sections-hook
                            #'magit-insert-assume-unchanged-files
                            #'magit-insert-untracked-files
                            :append)
    (magit-add-section-hook 'magit-status-sections-hook
                            #'magit-insert-skip-worktree-files
                            #'magit-insert-untracked-files
                            :append)))

(require 'helheim-diff-hl)  ; git gutter
(require 'helheim-ediff)

;;;; Terminal emulators
;; Requires shell-side configuration!

; (require 'helheim-eat)   ; written in emacs-lisp
; (require 'helheim-vterm) ; libvterm C library

(setup ghostel
  (require 'helheim-ghostel)
  (:after-load
    (:keymap ghostel-semi-char-mode-map
      (:bind
        "C-<escape>" 'hel-ghostel--send-escape
        ;; physical Esc keys
        ;; -> kanata translate to F14
        ;; -> GNOME translates to X86Launch5
        "<Launch5>" 'hel-ghostel--send-escape))))

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
    (:keymap agent-shell-mode-map
      ;; State-specific Enter behavior:
      ;; - insert state = newline
      ;; - normal state = send
      (:bind :state insert "RET" 'newline)
      (:bind :state normal "RET" 'comint-send-input))))

(setup mcp-server
  (require 'helheim-mcp-server)
  (setopt mcp-server-security-dangerous-functions '(kill-emacs)))

;;;; Other modules

;; (require 'helheim-notmuch)  ; Notmuch's own interface
(require 'helheim-gnus)     ; Gnus, with notmuch as indexer and search engine

(require 'helheim-browser)  ; Synchronize online text editor with Emacs buffer

(setup helheim-whisper ; Speech to text conversion
  (:require t)
  (setopt whisper-install-whispercpp 'manual
          whisper-install-directory "~/.local/src/"
          whisper-enable-speed-up t ;; WARNING
          whisper-model "small"))

(require 'helheim-edit-indirect) ; Alternative "zn" binding

(require 'helheim-chezmoi)  ; Integration with chezmoi dotfile manager

(require 'helheim-latex)    ; Fast LaTeX-math entry in Org notes
;; Needs the `typst' and `tinymist' binaries on PATH.
;; (require 'helheim-typst)    ; Typst: tree-sitter mode, tinymist LSP, live preview

;;; My custom config

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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

;;;; astroid (notmuch gui)

;; Astroid draft files are ~/.cache/astroid/<msg-id> with no extension,
;; so Emacs won't auto-pick a message-mode mode.
(add-to-list 'auto-mode-alist '("/\\.cache/astroid/" . message-mode))

;;;; cape

;; (hel-keymap-global-set :state 'insert
;;   ;; Emulate Vim's omni-completion keybinds
;;   "C-x"   'cape-prefix-map)
;;
;; (setup cape
;;   (:after-load
;;     (:keymap cape-prefix-map
;;       (:bind
;;         "C-o" 'completion-at-point ;; C-x C-o is Vim's omni-completion keybinding
;;         ;; "C-e" 'cape-elisp-block
;;         ;; "C-s" 'cape-elisp-symbol
;;         "/"   'cape-tex
;;         "C-/" 'cape-tex
;;         "C-h" 'cape-history
;;         "C-l" 'cape-line
;;         "C-k" 'cape-keyword
;;         "C-f" 'cape-file
;;         "C-t" 'complete-tag
;;         "C-w" 'cape-dict
;;         "C-r" 'cape-rfc1345
;;         ;; "s"   'cape-dict
;;         ;; "C-s" 'yasnippet-capf
;;         "C-a" 'cape-abbrev
;;         "C-d" 'cape-dabbrev
;;         "C-n" 'cape-dabbrev
;;         ;; "C-p" '+corfu/dabbrev-this-buffer
;;         ))))

;;;; DISABLED keycast

;; (setup keycast
;;   (:install t)
;;   (keycast-mode-line-mode) ;; in mode-line
;;   ;; (keycast-header-line-mode) ;; in header-line
;;   )

;;;; DISABLED pandoc-mode

;; (setup pandoc-mode
;;   (:install t)
;;   (:when (executable-find "pandoc"))
;;   (:hook markdown-mode-hook pandoc-mode)
;;   ;; (:hook markdown-mode-hook conditionally-turn-on-pandoc)
;;   (:after-load
;;     (:keymap pandoc-mode-map
;;       (:unbind "C-c /")
;;       (:bind ", /" '("pandoc" . pandoc-main-transient)))))

;;;; project.el

(hel-keymap-set project-prefix-map
  "b" 'project-list-buffers)

;;;; rainbow-mode

;; Colorize strings that represent colors
(setup rainbow-mode
  (:install t)
  (:blackout t)
  (:hook (emacs-lisp-mode-hook
          help-mode-hook
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

(setopt default-input-method "russian-computer")
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
  (:global-bind :state normal
    "z '" 'separedit)
  ;; (:keymap (prog-mode-map
  ;;           minibuffer-local-map
  ;;           help-mode-map)
  ;;   (:bind :state normal
  ;;     "z '" 'separedit))
  ;; (with-eval-after-load 'helpful
  ;;   (:keymap helpful-mode-map
  ;;     (:bind :state normal
  ;;       "z '" 'separedit)))
  ;; (with-eval-after-load 'obsidian
  ;;   (:keymap obsidian-mode-map
  ;;     (:bind :state normal
  ;;       "z '" 'separedit)))
  (:after-load
    (:keymap separedit-mode-map
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
  ;; Which modules to load.
  (setq org-modules '(ol-bibtex ol-docview ol-info))
  ;; Following variables must be set before `org' is loaded!
  (defvar my-private-directory (expand-file-name "~/Private/"))
  (setopt org-directory (expand-file-name "~/notes/")
          org-mem-watch-dirs (list org-directory my-private-directory))
  (:require helheim-org
            helheim-daily-notes)
  (:hook org-mode-hook
         (lambda ()
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
    ;; Make priority signs be integers from 1 to 5, with 4 as default.
    ;; Default priorities are: #A, #B, #C, with #B as default.
    (setopt org-priority-highest 1
            org-priority-default 4
            org-priority-lowest  5))
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
  (:after-load
    ;; Elements of length one have a tab appended. Elements of length two are
    ;; kept as is. Longer elements are truncated to length two. If an element
    ;; cannot be made unique, an error is raised.
    (setopt org-structure-template-alist `(("se" . "src emacs-lisp")
                                           ("sh" . "src sh")
                                           ("sc" . "src cpp")
                                           ("sl" . "src common-lisp") ;; "l" for Lisp
                                           ("sm" . "src markdown")
                                           ("sr" . "src rust")
                                           ("sp" . "src python")
                                           ("su" . "src lua")
                                           ,@org-structure-template-alist)))
  (:hook org-src-mode-hook
         (lambda ()
           (setq-local flycheck-disabled-checkers '(emacs-lisp
                                                    emacs-lisp-checkdoc))))
  ;; --- Capture templates ---
  ;; (setopt org-capture-templates '(("j" "journal" plain
  ;;                                  (file+olp+datetree +org-capture-journal-file)
  ;;                                  "%?"
  ;;                                  :empty-lines-before 1
  ;;                                  ;; :kill-buffer t
  ;;                                  )))
  )

;;;; org agenda

(setup org-agenda
  (setopt org-agenda-window-setup 'current-window
          org-agenda-restore-windows-after-quit t))

;; (setopt org-agenda-inhibit-startup t)

;; org-agenda-prefix-format
;; org-agenda-remove-tags
;; org-agenda-remove-times-when-in-prefix

;; (setopt org-agenda-category-icon-alist)

;;;;; org-agenda custom commands

;; (setq org-agenda-custom-commands
;;       '(("w" "Week agenda"
;;          ((agenda "" ((org-agenda-span 7)
;;                       (org-agenda-start-on-weekday nil) ; starting today
;;                       (org-agenda-entry-types '(:deadline :scheduled :sexp))
;;                       (org-deadline-warning-days 30)))))
;;         ("a" "Single day agenda"
;;          ((agenda "" ((org-agenda-span 1)
;;                       (org-agenda-files '("~/org/study.org"))
;;                       (org-agenda-entry-types '(:deadline :scheduled :sexp))
;;                       (org-agenda-format-date "")
;;                       (org-agenda-overriding-header "   Academic/Office")
;;                       (org-deadline-warning-days 30)))
;;           (agenda "" ((org-agenda-span 1)
;;                       (org-agenda-files '("~/org/private.org"))
;;                       (org-agenda-format-date "")
;;                       (org-agenda-entry-types '(:deadline :scheduled :sexp))
;;                       (org-deadline-warning-days 30)
;;                       (org-agenda-overriding-header "   Private")))))))

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

;;;; org-node

(setup org-node
  (require 'helheim-org-node)
  (setopt org-mem-do-warn-title-collisions nil)
  (remove-hook 'org-mem-post-full-scan-functions 'helheim-set-agenda-files)
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
   org-journal-date-format "%x, %A") ;; "DATE, WEEKDAY"

  ;; At load time org-journal unconditionally binds "j m", "j r", "j d",
  ;; "j n", "j s ..." and "[" / "]" in `calendar-mode-map'.  Hel binds "j"
  ;; there to `calendar-forward-week' and builds the "[ [" / "] ]" year
  ;; motions, so loading org-journal while `calendar' is already loaded
  ;; signals
  ;;
  ;;     Key sequence j m starts with non-prefix key j
  ;;
  ;; and aborts the load — which happens on any `M-x calendar' or org date
  ;; prompt, since org-journal autoloads itself onto
  ;; `calendar-today-visible-hook'.  Load it here with those keys free, then
  ;; move the prefix map it built to "J" and restore the Hel motions.
  (:defer
    (require 'calendar)
    (let* ((keys '("j" "[" "]"))
           (saved (mapcar (lambda (key)
                            (cons key (keymap-lookup calendar-mode-map key)))
                          keys)))
      (dolist (key keys)
        (keymap-unset calendar-mode-map key t))
      (require 'org-journal)
      (let ((journal-map (keymap-lookup calendar-mode-map "j"))
            (next (keymap-lookup calendar-mode-map "]"))
            (prev (keymap-lookup calendar-mode-map "[")))
        (dolist (key keys)
          (keymap-unset calendar-mode-map key t))
        (pcase-dolist (`(,key . ,def) saved)
          (when def (keymap-set calendar-mode-map key def)))
        (when journal-map
          (keymap-set calendar-mode-map "J" journal-map)
          (when next (keymap-set journal-map "]" next))
          (when prev (keymap-set journal-map "[" prev)))))))

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
(require 'helheim-cpp)
(require 'helheim-json)
(require 'helheim-lua)
(require 'helheim-rust)

(setup prog-mode
  (:hook prog-mode-hook (lambda ()
                          (setq-local line-move-visual nil))))

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
    (:keymap change-log-mode-map
      (:bind :state normal
        "] c" 'add-log-edit-next-comment
        "[ c" 'add-log-edit-prev-comment))))

;;; Keybindings

(require 'helheim-keybindings)

(setup emacs
  (:global-unbind
    "C-k"  ;; `kill-line'
    "M-j") ;; `default-indent-new-line'
  (:global-bind
    "M-;"   'eval-expression
    "C-M-;" 'repeat-complex-command))

(setup hel
  (:global-bind :state (normal emacs)
    "<backspace>" 'execute-extended-command)
  (:global-bind :state normal
    "M-;"  nil ;; unbind `hel-exchange-point-and-mark'
    "C-;" 'hel-exchange-point-and-mark
    "g s" 'hel-beginning-of-line-command
    "g h" 'hel-first-non-blank)
  ;; (:global-bind :state insert
  ;;   "C-"h   'delete-backward-char
  ;;   "C-/" 'dabbrev-expand)
  (:keymap hel-window-map ;; "C-w"
    (:bind "N" 'other-tab-prefix))
  (:keymap (prog-mode-map
            text-mode-map)
    (:bind :state insert
      "C-h" 'backward-delete-char-untabify)))

(with-eval-after-load 'corfu
  (hel-keymap-set corfu-map
    "C-l" 'corfu-insert-separator))

;;; init.el ends here

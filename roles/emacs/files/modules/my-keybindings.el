;; -*- lexical-binding: t -*-

(require 'general)
(require 'hydra)

;;* General keybindings

(general-def
  :states '(normal visual)
  "M-u" 'universal-argument
  "SPC" '(:keymap leader-map) ;; use 'Space' as leader key
  ;; "<backspace>" 'evil-ex ;; evil command (:) state
  "<backspace>" 'execute-extended-command ;; emacs M-x
  "g h" 'evil-first-non-blank
  "g l" 'evil-end-of-line
  ;; "g b" 'ibuffer-jump
  "g b" 'bufler
  "C-s" 'evil-write
  "C-p" 'consult-yank-from-kill-ring
  "-" 'dired-jump
  "/" 'consult-line
  "?" 'evil-search-forward
  "z n" 'narrow-to-region
  "z w" 'widen
  "Z Z" 'my/evil-save-modified-buffer-and-kill-it
  "z =" 'flyspell-correct-wrapper)

(general-def
  :keymaps 'universal-argument-map
  "M-u" 'universal-argument-more)

(general-def
  :states 'motion
  ";" '(:keymap semicolon-leader-map))

(general-def
  :states 'insert
  "C-l" 'right-char)

;;* Leader map

(general-def
  :keymaps 'leader-map
  "h"  '(:keymap help-map :which-key "help")
  "n"  '(:keymap my/notes-map :which-key "notes")
  "b"  'consult-buffer
  "ff" 'find-file
  "fo" 'consult-outline
  "fg" 'consult-grep
  "fi" 'consult-imenu
  "fr" 'consult-recent-file
  "fb" 'consult-bookmark)

;;* Minibuffer

(add-hook 'vertico-mode-hook
          (lambda ()
            (general-def
              :keymaps 'vertico-map
              :states '(normal visual)
              "C-j" 'vertico-next
              "C-k" 'vertico-previous
              "C-n" 'vertico-next-group
              "C-p" 'vertico-previous-group
              "C-f" 'vertico-scroll-up
              "C-b" 'vertico-scroll-down
              "q" 'abort-recursive-edit)

            (general-def
              :keymaps 'vertico-map
              :states 'insert
              "C-y" 'yank
              "C-j" 'vertico-next
              "C-k" 'vertico-previous
              "C-n" 'vertico-next-group
              "C-p" 'vertico-previous-group
              "C-f" 'vertico-scroll-up
              "C-b" 'vertico-scroll-down)))

;; :bind (:map vertico-map
;;        ("C-j" . vertico-next)
;;        ("C-k" . vertico-previous)
;;        ("C-n" . vertico-next-group)
;;        ("C-p" . vertico-previous-group)
;;        ("C-l" . vertico-insert)
;;        ("C-f" . vertico-scroll-up)
;;        ("C-b" . vertico-scroll-down)
;;        ;; ("C-g" . vertico-exit)
;;        :map minibuffer-local-map
;;        ("C-h" . backward-kill-word))

;;* Dired

(defhydra hydra-dired (:hint nil :color pink)
  "
_Y_ rel symlink    _O_pen marked
_S_ymlink          _n_ narrow
_z_ compress-file  _m_ chmod        _s_ort           _E_ ediff
_Z_ compress 			 _g_ chgrp    	  _e_xtension mark   _=_ pdiff
"
  ("+" dired-maybe-create-dirs)
  ("=" diredp-ediff) ;; smart diff
  ("e" dired-mark-extension)
  ("E" dired-ediff-files)
  ("g" dired-do-chgrp)
  ("m" dired-do-chmod)
  ("n" dired-narrow)
  ("O" dired-do-find-marked-files) ;; open in Emacs all marked files
  ;; ("o" dired-display-file)
  ;; ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue)
  ("<escape>" nil :color blue))

(general-def
  :keymaps 'dired-mode-map
  :states 'normal
  ;; "<backspace>" 'dired-up-directory
  "<backspace>" 'execute-extended-command ;; emacs M-x
  ;; "<tab>" 'dired-subtree-cycle
  "l" 'dired-find-file
  "h" 'dired-up-directory
  "f" 'find-file
  "r" 'dired-do-rename
  "y" 'dired-ranger-copy
  "p p" 'dired-ranger-paste
  "p m" 'dired-ranger-move
  ;; "P" 'dired-ranger-move
  "w" 'dired-display-file
  ")" 'dired-omit-mode
  "<" 'dired-prev-marked-file
  ">" 'dired-next-marked-file
  "." 'hydra-dired/body)

(general-def
  :keymaps 'dired-mode-map
  :states 'visual
  "d" 'dired-flag-file-deletion
  "u" 'dired-unmark)

;;* Outline

(general-def
  :keymaps 'outline-mode-map
  :states 'normal
  "<tab>" 'outline-cycle
  "z j" 'outline-next-visible-heading
  "z k" 'outline-previous-visible-heading)

;;* Orgmode

(general-def
  :keymaps 'my/orgmode-leader-map
  "SPC" 'org-ctrl-c-ctrl-c
  "/" 'org-sparse-tree
  ";" 'org-toggle-comment
  "l" 'org-insert-link
  "d" 'org-deadline
  "s" 'org-schedule
  "t" 'org-time-stamp
  "T" 'org-time-stamp-inactive
  "L" 'org-cliplink)

(defun my/org-mode-keybindings ()
  (general-def
    :keymaps 'local
    ;; :keymaps 'org-mode-map
    :states '(normal visual)
    "g h" 'evil-org-beginning-of-line
    "g l" 'evil-org-end-of-line
    ;; "H" 'org-up-element
    ;; "L" 'org-down-element
    "g x" 'org-open-at-point
    "z n" 'org-narrow-to-subtree
    "SPC" '(:keymap my/orgmode-leader-map)
    "K" 'helpful-at-point)

  (general-def
    :keymaps 'local
    ;; :keymaps 'org-mode-map
    :states 'insert
    "C-t" 'evil-shift-right-line
    "C-d" 'evil-shift-left-line))

(general-def
  :keymaps 'my/notes-map
  "a" 'org-agenda
  "s" 'org-store-link
  "n" 'org-roam-node-find
  "c" 'org-roam-capture
  "w" 'org-roam-buffer-toggle
  "b" 'org-switchb
  "i" 'org-roam-node-insert
  "l" 'org-roam-node-insert
  "u" 'org-roam-ui-mode)

;;** Org transclusion

(general-def
  :keymaps 'org-transclusion-map
  :states 'normal
  ;; "e" #'org-transclusion-live-sync-start
  ;; "g" #'org-transclusion-refresh
  ;; "d" #'org-transclusion-remove
  ;; "C-d" #'org-transclusion-detach
  ;; "P" #'org-transclusion-promote-subtree
  ;; "D" #'org-transclusion-demote-subtree
  ;; "o" #'org-transclusion-open-source
  ;; "O" #'org-transclusion-move-to-source
  "gi" #'org-transclusion-live-sync-start
  )

(general-def
  :keymaps 'org-transclusion-live-sync-map
  :states 'normal
  ;; "C-c C-c" #'org-transclusion-live-sync-exit
  ;; "C-y" #'org-transclusion-live-sync-paste
  "q" #'org-transclusion-live-sync-exit)


;;* Provide

(provide 'my-keybindings)

;; Local Variables:
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-sublevels 5)
;; End:

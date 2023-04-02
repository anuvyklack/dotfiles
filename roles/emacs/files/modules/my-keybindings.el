;; -*- lexical-binding: t; outline-minor-mode: t -*-

(require 'general)
(require 'hydra)

;;; General

(general-def
  :states '(normal visual)
  "SPC" '(:keymap leader-map)           ; use 'Space' as leader key
  ;; "<backspace>" 'evil-ex                 ; evil command (:) state
  "<backspace>" 'execute-extended-command ; emacs M-x
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
  :states 'motion
  ";" '(:keymap semicolon-leader-map))

(general-def
  :keymaps 'leader-map
  "h" '(:keymap help-map :which-key "help"))

(general-def
  :states 'insert
  "C-l" 'right-char)

;;; Dired

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
	"f"	'find-file
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

(provide 'my-keybindings)

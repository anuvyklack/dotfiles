(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/work/platform/README.org" "/home/anuvyklack/dotfiles/roles/emacs/files/README.org" "/home/anuvyklack/notes/20231127021517-initi.org" "/home/anuvyklack/notes/20231001165947-cpp_notes.org" "/home/anuvyklack/notes/20231020134416-medical.org" "/home/anuvyklack/notes/20230919165106-my_file_manager.org" "/home/anuvyklack/notes/20230427224234-cpp.org" "/home/anuvyklack/notes/20230501023150-personal.org" "/home/anuvyklack/notes/20230415235539-holidays.org" "/home/anuvyklack/notes/20230810181055-окр.org" "/home/anuvyklack/notes/20230413224540-todo.org"))
 '(package-selected-packages
   '(xkb-mode lemon-mode fish-mode visual-fill-column separedit obsidian edit-indirect doom-gruvbox-material-theme doom-themes evil-markdown eglot form-feed form-feed-st nushell-ts-mode page-break-lines symex eglot-booster ox-pandoc difftastic forge restart-emacs ws-butler fzf breadcrumb nix-mode nix-ts-mode org flymake-diagnostic-at-point flycheck-eglot flycheck-inline sxiv flycheck consult-lsp lsp-ui lsp-mode dired-preview dirvish leerzeichen yaml-pro virtual-auto-fill adaptive-wrap xenops auctex magit org-auto-tangle org-transclusion org-cliplink org-appear org-ql org-super-agenda org-contrib org-roam-ui consult-org-roam org-roam org-pretty-tags org-superstar vundo rg rainbow-delimiters origami zoxide expand-region consult-eglot flyspell-correct-popup flyspell-correct evil-matchit evil-surround better-jumper evil-nerd-commenter evil-anzu evil-goggles evil-visualstar easy-escape doom-modeline eldoc-box evil-xkbswitch evil-fringe-mark info-colors dtrt-indent cape kind-icon corfu affe git-modes fennel-mode evil-cleverparens plantuml-mode yaml-mode lua-mode cmake-mode markdown-mode telega ef-themes djvu fb2-reader nov org-pdftools pdf-tools inform info-rename-buffer rainbow-mode consult-projectile projectile bufler diredfl dired-rainbow dired-collapse dired-toggle-sudo dired-open dired-narrow dired-subtree dired-ranger good-scroll orderless nerd-icons-completion marginalia consult-dir consult vertico evil-snipe evil-easymotion evil-collection evil treesit-auto helpful vc-use-package better-defaults no-littering general which-key gcmh dash auto-compile hydra diminish))
 '(package-vc-selected-packages
   '((evil-markdown :vc-backend Git :url "https://github.com/Somelauw/evil-markdown")
     (evil-xkbswitch :vc-backend Git :url "https://github.com/linktohack/evil-xkbswitch")
     (nushell-ts-mode :vc-backend Git :url "https://github.com/herbertjones/nushell-ts-mode")
     (doom-gruvbox-material-theme :vc-backend Git :url "https://github.com/Cardoso1994/doom-gruvbox-material-theme")
     (vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package")))
 '(safe-local-variable-values
   '((evil-shift-width . 4)
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (projectile-project-test-cmd . "./build/debug/tests")
     (projectile-project-compilation-cmd . "./my-build.sh")
     (projectile-project-test-cmd . "./build/debug/bin/tests")
     (projectile-project-run-cmd . "./build/debug/bin/smola-scratch")
     (projectile-project-compilation-cmd . "./my_build")
     (projectile-project-compilation-cmd . "./build.sh -j 15 -bt Debug")
     (projectile-project-run-cmd . "./bin")
     (projectile-project-compilation-cmd . "g++ -lm -O2 -fno-stack-limit -std=c++20 -x c++ main.cc -o bin")
     (org-attach-id-dir . ~/Documents/books/0_hash)
     (org-attach-use-inheritance)
     (projectile-project-run-cmd . "./a.out")
     (projectile-project-compilation-cmd . "g++ -lm -O2 -fno-stack-limit -std=c++20 -x c++ main.cc")
     (org-roam-db-location . "/home/anuvyklack/.config/emacs/var/org/org-roam-personal.db")
     (org-roam-directory . "/home/anuvyklack/notes-personal")
     (eval setq-local org-attach-id-dir
           (file-name-as-directory
            (file-name-concat org-roam-directory "data")))
     (evil-shift-width . 2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(safe-local-variable-values
   '((org-roam-db-location . "./.org-roam.db")
     (org-roam-directory . ".")
     (org-attach-use-inheritance)
     (eval outline-hide-sublevels 5)
     (org-roam-db-location . "/home/anuvyklack/.config/emacs/var/org/org-roam-personal.db")
     (org-roam-directory . "/home/anuvyklack/notes-personal")
     (elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (thread-first . 0)
      (cl-flet . 1)
      (cl-flet* . 1)
      (org-element-map . defun)
      (org-roam-dolist-with-progress . 2)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (projectile-project-run-cmd . "./build/debug/bin/smola-scratch")
     (projectile-project-test-cmd . "./build/debug/tests")
     (projectile-project-compilation-cmd . "./my-build.sh")
     (evil-shift-width . 2)
     (c-ts-mode-indent-offset . 2)
     (eval progn
      (setq-local org-attach-id-dir
                  (file-name-as-directory
                   (file-name-concat org-roam-directory "data"))
                  org-roam-directory
                  (file-name-directory
                   (or
                    (buffer-file-name)
                    default-directory))
                  org-roam-db-location "/home/anuvyklack/.config/emacs/var/org/org-roam-old.db")))))
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

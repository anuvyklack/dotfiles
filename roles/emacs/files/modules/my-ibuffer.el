; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))

(use-package ibuffer
  :after projectile
  :custom
  (ibuffer-expert t) ;; Do not ask for confirmation to delete the unmodified buffer.
  (ibuffer-truncate-lines t)
  (ibuffer-show-empty-filter-groups nil) ;; Don't show emtpy filter groups
  (ibuffer-display-summary nil)
  ;; (ibuffer-read-only-char "%")
  ;; (ibuffer-modified-char "*")
  ;; (ibuffer-marked-char ">")
  ;; (ibuffer-locked-char "L")
  ;; (ibuffer-deletion-char "D")
  ;; (ibuffer-formats)
  ;; (define-ibuffer-column)
  (ibuffer-formats '((mark modified read-only locked
                           " "
                           (name 25 25 :left :elide)
                           "   "
                           ;; (filename-and-process 75 -1 :left :elide)
                           (project-relative-filename-or-process 75 -1 :left :elide)
                           " "
                           mode)))
  :hook
  (ibuffer . (lambda ()
               (ibuffer-auto-mode) ;; Automatically update the ibuffer
               (hl-line-mode)
               ;; (setq truncate-lines t) ;; do not wrap long lines
               ;; (ibuffer-switch-to-saved-filter-groups "home")
               ))
  :config
  (define-ibuffer-column project-relative-filename-or-process
    (:name "Filename/Process"
     :header-mouse-map ibuffer-filename/process-header-map
     :summarizer
     (lambda (strings)
       (setq strings (delete "" strings))
       (let ((procs 0)
             (files 0))
         (dolist (string strings)
           (when (get-text-property 1 'ibuffer-process string)
             (setq procs (1+ procs)))
           (setq files (1+ files)))
         (concat (cond ((zerop files) "No files")
                       ((= 1 files) "1 file")
                       (t (format "%d files" files)))
                 ", "
                 (cond ((zerop procs) "no processes")
                       ((= 1 procs) "1 process")
                       (t (format "%d processes" procs)))))))
    (let ((proc (get-buffer-process buffer))
          (filename (ibuffer-make-column-filename buffer mark)))
      (if proc
          (concat (propertize (format "(%s %s)" proc (process-status proc))
                              'font-lock-face 'italic
                              'ibuffer-process proc)
                  (if (> (length filename) 0)
                      (format " %s" filename)
                    ""))
        (let ((root-dir (cdr (ibuffer-projectile-root buffer))))
          (if root-dir
              (file-relative-name filename root-dir)
            (abbreviate-file-name filename)
            ;; filename
            ))))))

; (use-package all-the-icons-ibuffer
;   :elpaca t
;   :after all-the-icons
;   :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
;   :custom
;   (all-the-icons-ibuffer-human-readable-size t))

(provide 'my-ibuffer)

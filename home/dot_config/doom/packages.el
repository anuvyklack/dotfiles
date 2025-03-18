;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

;; Libraries
(package! dash)
(package! posframe)
(package! page-break-lines)
;; (package! treesit-auto)

;; color themes
(package! modus-themes)
(package! ef-themes :recipe (:host github :repo "anuvyklack/ef-themes")) ;; my fork

;; colors
(package! rainbow-mode)
(package! colorful-mode)

(package! easy-escape)
(package! eldoc-box)
(package! evil-cleverparens)
(package! evil-goggles)
(package! evil-xkbswitch :recipe (:host github :repo "linktohack/evil-xkbswitch"))
;; (package! filladapt)
(package! aggressive-indent)
;; flycheck
(package! flycheck-inline)
(disable-packages! flycheck-popup-tip) ;; or (package! flycheck-popup-tip :disable t)
;; ibuffer
;; (package! nerd-icons-ibuffer)
(package! ibuffer-projectile)
(package! ibuffer-vc)

;; dired
(package! diredfl)
(package! dired-copy-paste :recipe (:host github :repo "jsilve24/dired-copy-paste"))
(package! dired-du)
(package! dired-preview)
;; (package! dired-rainbow)
(package! dired-open)
(package! dired-open-with)
(package! dired-narrow)
(package! dired-subtree)
(package! dired-filter)

;; windows
(package! popper)

;; org
(package! org-appear)
(package! org-superstar)
(package! org-pretty-tags)
(package! org-auto-tangle)
(package! org-web-tools)
(package! org-sliced-images)
(package! org-fragtog)
(package! org-bookmarks)
(package! pandoc-mode)

;; org-roam
(package! org-roam-ql)
(package! org-roam-ui)
(package! consult-org-roam)
(package! denote)
(package! nursery :recipe (:host github :repo "chrisbarrett/nursery"))
(package! org-roam-tags :recipe (:host github :repo "simoninireland/org-roam-tags"))
;; obsidian
(package! obsidian)

;; (package! better-jumper :disable t)

(package! casual)
(package! eval-in-repl)
(package! fish-mode)
(package! chezmoi)

(package! scroll-on-jump)

;; Packages for meow based config development
(package! leaf)
(package! leaf-keywords)
(package! meow :recipe (:local-repo "~/code/emacs/meow/"))
(package! ultra-scroll :recipe (:host github :repo "jdtsmith/ultra-scroll"))

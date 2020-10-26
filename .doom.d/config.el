;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Global configuration
(setq user-full-name "Mattias Lundberg"
      user-mail-address "me@mlundberg.se")

;; Doom
(setq doom-font (font-spec :family "Fira Code" :size 14.0))
(setq doom-big-font (font-spec :family "Fira Code" :size 28.0))
(setq doom-theme 'doom-one-light)

(setq org-directory "~/Documents/org/")
(setq display-line-numbers-type nil)

;; Modeline
(setq doom-modeline-buffer-file-name-style 'relative-from-project
      doom-modeline-buffer-encoding nil
      doom-modeline-buffer-modification-icon nil
      doom-modeline-env-version nil)

;; Don't scroll to the edge of the window
(setq scroll-margin 3)
(setq scroll-conservatively 3)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Format errors in popup
(set-popup-rule! "^\\*format-all-errors" :size 0.3 :ttl 0)
(setq +format-with-lsp nil)

;; LSP
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-file-watch-threshold 10000)
(setq lsp-file-watch-ignored
      '("[/\\\\]\\.git$"
        "[/\\\\]\\.eunit$"
        "[/\\\\]node_modules$"
        "[/\\\\]__pycache__$"
        "[/\\\\]\\.mypy_cache$"
        "[/\\\\]\\.fslckout$"
        "[/\\\\]\\.tox$"
        "[/\\\\]dist$"
        "[/\\\\]dist-newstyle$"
        "[/\\\\]\\.stack-work$"
        "[/\\\\]\\.bloop$"
        "[/\\\\]\\.metals$"
        "[/\\\\]target$"
        "[/\\\\]\\.ccls-cache$"
        "[/\\\\]\\.vscode$"
        "[/\\\\]\\.deps$"
        "[/\\\\]\\.reference$"))

;; Treemacs
(setq +treemacs-git-mode 'extended)
(setq treemacs-indentation 2)
(setq treemacs-width 25)
(setq treemacs-show-cursor nil)
(treemacs-resize-icons 16)
(add-hook 'treemacs-mode-hook (lambda () (treemacs-fringe-indicator-mode -1)))
(add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)

;; etags
(setq tags-add-tables nil)

;; Magit
(after! magit
  (setq magit-prefer-push-default t
        magit-revision-show-gravatars nil
        magit-display-buffer-function 'magit-display-buffer-traditional))

;; Company
(after! company
  (setq company-idle-delay 0.1)
  (define-key! company-active-map
    "TAB"       #'company-complete-selection
    [tab]       #'company-complete-selection
    [backtab]   nil))

;; Projectile
(setq projectile-project-search-path
      (append
       (doom-files-in "~/projects" :depth 0 :type 'dirs :full t)
       `("~/tmp")))

;; IVY
(after! counsel
  (setq counsel-find-file-ignore-regexp "\\(?:^#\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

;;; Eshell
(load-file "~/.doom.d/eshell.el")

;;; Global keybindings
;; Magit
(evil-define-key 'normal with-editor-mode-map
  ",k" 'with-editor-cancel
  ",c" 'with-editor-finish
  ",," 'with-editor-finish)

(evil-define-key 'normal magit-status-mode-map
  "z" #'magit-stash)

(map! :leader
      (:desc "Blame" "g b" #'magit-blame-addition))

;; Lookup in dash
(map! :leader (:prefix-map ("d" . "custom")
               :desc "Dash lookup" "d" #'dash-at-point))

;; Vagrant
(map! :leader (:prefix-map ("d v" . "Vagrant")
               (:desc "up" "u" #'vagrant-up)
               (:desc "ssh" "s" #'vagrant-ssh)
               (:desc "halt" "x" #'vagrant-halt)
               (:desc "status" "?" #'vagrant-status)
               (:desc "edit" "e" #'vagrant-edit)))
(set-popup-rule! "^\\*Vagrant" :size 0.3 :ttl 0)

;; Doom
(map! :leader
      (:desc "doom/upgrade" "h r u" #'doom/upgrade))

;; Kubernetes
(map! :leader
      (:desc "Kubernetes overview" "d k" #'kubernetes-overview))

;; Format current buffer
(map! :leader
      (:desc "Format buffer" "f ." #'format-all-buffer)
      (:desc "Toogle format-all-mode" "f ," #'format-all-mode))

;; Regenerate tags
(map! :leader
      (:desc "Regenerate tags" "p [" #'projectile-regenerate-tags))

;;; Language specific
(let ((cfs (doom-files-in "~/.doom.d/lang")))
  (dolist (cf cfs)
    (load-file cf)))

;;; System specific
;; MacOS
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil)
  (setq shell-file-name "/usr/local/bin/bash")
  (setq projectile-tags-command "/usr/local/bin/ctags -R -e")
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  (setq ns-use-native-fullscreen t)
  (set-frame-parameter nil 'fullscreen 'fullboth))

;; Linux
(when (string= system-type "gnu/linux")
  (setq shell-file-name "/bin/bash")
  (after! projectile
    (menu-bar-mode -99)))

(if (file-exists-p "/snap/bin/universal-ctags")
    (setq projectile-tags-command "/snap/bin/universal-ctags -R -e --exclude=.git --exclude=node_modules --exclude=elm-stuff --exclude=_build --exclude=deps --exclude=dist --exclude=bundles --exclude=collected --exclude=js_bundles --exclude=transpiled_js --exclude=__pycache__ --exclude=bundles --exclude=.cov --exclude=.cache --exclude=.terraform --exclude=.mypy_cache"))

;; Local setup
(if (file-exists-p "~/.emacs.local") (load-file "~/.emacs.local"))

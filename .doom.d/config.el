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

;; LSP
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-file-watch-threshold 10000)
(setq lsp-file-watch-ignored
  '("[/\\\\]\\.git$"
    "[/\\\\]\\.hg$"
    "[/\\\\]\\.bzr$"
    "[/\\\\]_darcs$"
    "[/\\\\]\\.svn$"
    "[/\\\\]_FOSSIL_$"
    "[/\\\\]\\.idea$"
    "[/\\\\]\\.ensime_cache$"
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
    "[/\\\\]build-aux$"
    "[/\\\\]autom4te.cache$"
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
(defadvice! fix-magit-revert-buffer (buffer)
  :override #'+magit--revert-buffer
  (with-current-buffer buffer
    (setq +magit--stale-p nil)
    (when buffer-file-name
      (revert-buffer t (not (buffer-modified-p))))))

;; Company
(add-hook 'company-mode-hook
          (lambda()
            (local-set-key (kbd "<right>") 'company-complete)))

(after! company-tng
  (define-key! company-active-map
    "TAB"       #'company-complete-selection
    [tab]       #'company-complete-selection
    [backtab]   nil))

(after! company
  (setq company-idle-delay 0.1))

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
;; Python
(map! :after python
      :localleader
      :map python-mode-map
      "t a" #'python-pytest
      "t x" #'python-pytest-last-failed)
(set-popup-rule! "^\\*pytest" :size 0.3 :ttl 0)

;; Javascript
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

(defun custom-npm-test ()
  (interactive)
  (npm-mode--exec-process "npm run test"))

(map! :after js2-mode
      :localleader
      :map js2-mode-map
      (:desc "Run all tests" "t a" #'custom-npm-test))

(set-popup-rule! "^\\*npm:" :size 0.3 :ttl 0)

;; Fennel
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

;; Elixir
(map! :after alchemist
      :localleader
      :map alchemist-mode-map
      "t a" #'alchemist-mix-test
      "b r" #'alchemist-iex-run
      "b p" #'alchemist-iex-project-run
      "b l" #'alchemist-mix-rerun-last-task)
(set-popup-rule! "^\\*Alchemist" :size 0.3 :ttl 0)
(set-popup-rule! "^\\*alchemist" :size 0.3 :ttl 0)

;; Rust
(set-popup-rule! "^\\*cargo" :size 0.3 :ttl 0)

;;; System specific
;; MacOS
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil)
  (setq shell-file-name "/usr/local/bin/bash")
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  (setq ns-use-native-fullscreen t)
  (set-frame-parameter nil 'fullscreen 'fullboth))

;; Linux
(when (string= system-type "gnu/linux")
  (setq shell-file-name "/bin/bash")
  (after! projectile
    (menu-bar-mode -99)))

;; Local setup
(if (file-exists-p "~/.emacs.local") (load-file "~/.emacs.local"))

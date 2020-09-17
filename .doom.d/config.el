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
(setq doom-modeline-buffer-file-name-style 'buffer-name
      doom-modeline-buffer-encoding nil
      doom-modeline-github t)

;; Don't scroll to the edge of the window
(setq scroll-margin 3)
(setq scroll-conservatively 3)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Read more from output (Recommended from LSP mode)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Format errors in popup
(set-popup-rule! "^\\*format-all-errors" :size 0.3 :ttl 0)

;; Treemacs
(setq +treemacs-git-mode 'deferred)
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
  (setq magit-prefer-push-default t)
  (setq magit-revision-show-gravatars nil))
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

;;; Eshell
;; Aliases
(setq +eshell-aliases
      (append
       +eshell-aliases
       '(
         ("g" "git $*")
         ("dc" "docker-compose $*")
         ("d" "docker $*")
         ("vs" "vagrant ssh $*")
         ("reload" "eshell-read-aliases-list"))))

;; Use emacs as editor
(add-hook 'eshell-mode-hook 'with-editor-export-editor)

;; Prompt
(defvar-local eshell-current-command-start-time nil)

(defun eshell-current-command-start ()
  (setq eshell-current-command-start-time (current-time)))

(defun command-time ()
  (if eshell-current-command-start-time
      (let ((took (float-time
                   (time-subtract (current-time)
                                  eshell-current-command-start-time))))
        (setq eshell-current-command-start-time nil)
        (if (> took 3)
            (format-seconds "%H %M %z%.2ss " took)
          ""))
    ""))

(defun eshell-current-command-time-track ()
  (add-hook 'eshell-pre-command-hook #'eshell-current-command-start nil t))

(add-hook 'eshell-mode-hook #'eshell-current-command-time-track)

(defun eshell-prompt-fn ()
  (concat "\n"
          (propertize (command-time) 'face '((t)))
          (propertize (number-to-string eshell-last-command-status)
                      'face '((t)))
          " "
          (propertize (format-time-string "%H:%M:%S" (current-time))
                      'face '((t)))
          (if (bobp) " ")
          (+eshell-default-prompt-fn)))

(after! eshell
  (setq eshell-prompt-function #'eshell-prompt-fn))

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
(after! flycheck
  (flycheck-add-next-checker 'python-flake8 'python-mypy t))

(after! flycheck
  (setq-default flycheck-disabled-checkers
                '(
                  python-flake8 python-pylint emacs-lisp-checkdoc
                  )))

;; Make sure pipenv correctly activates
(after! python
  (add-hook 'projectile-after-switch-project-hook (lambda ()
                                                    (pipenv-deactivate)
                                                    (cd (projectile-project-root))
                                                    (when (pipenv-project?)
                                                      (pipenv-activate)))))
;; Remap test running
(map! :after python
      :localleader
      :map python-mode-map
      "t a" #'python-pytest
      "t x" #'python-pytest-last-failed)

(set-popup-rule! "^\\*pytest" :size 0.3 :ttl 0)

;; Javascript
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

;; Custom keyboard mappings
(defun custom-npm-test ()
  (interactive)
  (npm-mode--exec-process "npm run test"))

(map! :after js2-mode
      :localleader
      :map js2-mode-map
      (:desc "Run all tests" "t a" #'custom-npm-test))

;; ReasonML
(add-hook 'reason-mode-hook (lambda ()
                              (add-hook 'before-save-hook #'refmt-before-save)))

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

;; Rust
(set-popup-rule! "^\\*cargo" :size 0.3 :ttl 0)

;;; System specific
;; MacOS
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil)
  (setq shell-file-name "/usr/local/bin/bash")
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  (setq ns-use-native-fullscreen t))

;; Local setup
(if (file-exists-p "~/.emacs.local") (load-file "~/.emacs.local"))

;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Extra packages
;; (package! company-tabnine)
;; (package! emoji-cheat-sheet-plus)
;;(package! reason-mode)
;; (package! protobuf-mode
;;    :recipe (:host github :repo "emacsmirror/protobuf-mode" :files (:defaults "*")))

;; (package! vagrant)
;; (package! vagrant-tramp
;;   :recipe (:host github :repo "mattiaslundberg/vagrant-tramp"))
(when IS-LINUX
  (package! systemd))
(package! polymode)
(package! git-auto-commit-mode
  :recipe (:host github :repo "ryuslash/git-auto-commit-mode"))
(package! flymake-eslint)

(package! shell-maker)
(package! acp)
(package! agent-shell)

;; Disable packages
(package! nose :disable t)
(package! pipenv :disable t)
(package! alchemist :disable t)
(package! alchemist-company :disable t)
(package! code-review :disable t)
(package! tide :disable t)

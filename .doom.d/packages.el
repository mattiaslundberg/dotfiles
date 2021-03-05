;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Extra packages
;; (package! company-tabnine)
;; (package! emoji-cheat-sheet-plus)
(package! dash-at-point)
;;(package! reason-mode)
;; (package! protobuf-mode
;;    :recipe (:host github :repo "emacsmirror/protobuf-mode" :files (:defaults "*")))

;; (package! vagrant)
;; (package! vagrant-tramp
;;   :recipe (:host github :repo "mattiaslundberg/vagrant-tramp"))
(package! systemd)
(package! counsel-tramp)
(package! polymode)
(package! apheleia)

(when (string-equal system-configuration "x86_64-apple-darwin20.3.0")
  (package! tree-sitter)
  (package! tree-sitter-langs))

;; Disable packages
(package! nose :disable t)
(package! pipenv :disable t)
(package! alchemist :disable t)
(package! alchemist-company :disable t)

;; Bumps (use github instead of repo)
(package! spinner
  :recipe (:host github :repo "Malabarba/spinner.el"))

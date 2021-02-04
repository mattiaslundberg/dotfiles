;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Extra packages
;; (package! company-tabnine)
;; (package! emoji-cheat-sheet-plus)
(package! dash-at-point)
;;(package! reason-mode)
;; (package! protobuf-mode
;;    :recipe (:host github :repo "emacsmirror/protobuf-mode" :files (:defaults "*")))

(package! fennel-mode)
;; (package! vagrant)
;; (package! vagrant-tramp
;;   :recipe (:host github :repo "mattiaslundberg/vagrant-tramp"))
;; (package! kubernetes)
;; (package! kubernetes-evil)
(package! systemd)
(package! counsel-tramp)
(package! polymode)
(package! apheleia
   :recipe (:host github :repo "raxod502/apheleia"))

;; Disable packages
(package! nose :disable t)
(package! pipenv :disable t)
(package! alchemist :disable t)
(package! alchemist-company :disable t)

;; Bumps (use github instead of repo)
(package! spinner
  :recipe (:host github :repo "Malabarba/spinner.el"))

;; TMP: Until define-obsolete-variable-alias fixes are bumped in doom
(unpin! dockerfile-mode)

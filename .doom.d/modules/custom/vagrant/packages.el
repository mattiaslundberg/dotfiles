;; -*- no-byte-compile: t; -*-
;;; custom/vagrant/packages.el

(package! vagrant)
(package! vagrant-tramp
   :recipe (:host github :repo "mattiaslundberg/vagrant-tramp"))

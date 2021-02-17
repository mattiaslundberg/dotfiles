;;; custom/vagrant/packages.el -*- lexical-binding: t; -*-

(package! vagrant)
(package! vagrant-tramp
   :recipe (:host github :repo "mattiaslundberg/vagrant-tramp"))

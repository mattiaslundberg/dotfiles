;;; custom/vagrant/config.el -*- lexical-binding: t; -*-
(map! :leader (:prefix-map ("d v" . "Vagrant")
              (:desc "up" "u" #'vagrant-up)
              (:desc "ssh" "s" #'vagrant-ssh)
              (:desc "tramp-term" "S" #'vagrant-tramp-term)
              (:desc "halt" "x" #'vagrant-halt)
              (:desc "status" "?" #'vagrant-status)
              (:desc "edit" "e" #'vagrant-edit)
              (:desc "provision" "p" #'vagrant-provision)))

(set-popup-rule! "^\\*Vagrant" :size 0.3 :ttl 0)

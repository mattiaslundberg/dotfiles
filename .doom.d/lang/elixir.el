
(map! :after alchemist
      :localleader
      :map alchemist-mode-map
      "t a" #'alchemist-mix-test
      "b r" #'alchemist-iex-run
      "b p" #'alchemist-iex-project-run
      "b l" #'alchemist-mix-rerun-last-task)
(set-popup-rule! "^\\*Alchemist" :size 0.3 :ttl 0)
(set-popup-rule! "^\\*alchemist" :size 0.3 :ttl 0)

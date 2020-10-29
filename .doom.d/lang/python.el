(map! :after python
      :localleader
      :map python-mode-map
      "t x" #'python-pytest-last-failed)
(set-popup-rule! "^\\*pytest" :size 0.3 :ttl 0)

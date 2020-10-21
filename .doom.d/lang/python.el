(map! :after python
      :localleader
      :map python-mode-map
      "t a" #'python-pytest
      "t x" #'python-pytest-last-failed)
(set-popup-rule! "^\\*pytest" :size 0.3 :ttl 0)

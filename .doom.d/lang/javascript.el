(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

(defun ml/npm-test ()
  (interactive)
  (npm-mode--exec-process "npm run test"))

(map! :after js2-mode
      :localleader
      :map js2-mode-map
      (:desc "Run all tests" "t a" #'ml/npm-test))

(set-popup-rule! "^\\*npm:" :size 0.3 :ttl 0)

(add-hook! '(js2-mode typescript-mode)
  (set (make-local-variable 'prettify-symbols-alist)
       '(("&&" . ?∧)
         ("||" . ?∨))
       ))

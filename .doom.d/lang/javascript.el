(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

(defun ml/npm-test ()
  (interactive)
  (npm-mode--exec-process "npm run test"))

(defun ml/npm-test-file ()
  (interactive)
  (npm-mode--exec-process (format "npm run test -- %s" (buffer-file-name)) ))

(defun ml/yalc-push ()
  (interactive)
  (npm-mode--exec-process "yalc publish --push"))

;; Translation file to use, relative to project root
(setq-default ml/i18next-translation-file "/translations/en.json")

(defun ml/extract-selection-i18next ()
  "Extract selected text to i18next translation file, will ask for keyname"
  (interactive)
  (let* ((selection (buffer-substring (mark) (point)))
         (extracted (string-remove-prefix "'" selection))
         (extracted (string-remove-prefix "\"" selection))
         (extracted (string-remove-suffix "'" extracted))
         (extracted (string-remove-suffix "\"" extracted))
         (filename (format "%s%s" (projectile-project-root) ml/i18next-translation-file))
         (previous (json-read-file filename))
         (existing-entry (rassoc extracted previous))
         (keyname (if existing-entry
           (car existing-entry)
           (read-string "Enter key name: "))))

    (message (format "Extracting string '%s' as '%s' " extracted keyname))

    (kill-region (region-beginning) (region-end))
    (insert (format "t('%s')" keyname))
    (save-buffer)

    (when (eq () existing-entry)
      (let ((json-encoding-pretty-print t)
            (json-encoding-default-indentation "    ")
            (new (json-add-to-object previous keyname extracted)))
          (f-write (json-encode new) 'utf-8 filename)))))

(defun ml/lookup-string-i18next ()
  "Lookup key at point in translation file and show message"
  (interactive)
  (let* ((keyname (thing-at-point 'symbol))
         (filename (format "%s%s" (projectile-project-root) ml/i18next-translation-file))
         (json-object-type 'hash-table)
         (json-key-type 'string)
         (json (json-read-file filename)))
    (message (format "Translates to: %s" (gethash keyname json)))))

(map! :after js2-mode
      :localleader
      :map js2-mode-map
      (:desc "Extract string i18next" "e" #'ml/extract-selection-i18next)
      (:desc "Lookup string i18next" "l" #'ml/lookup-string-i18next)
      (:desc "Run all tests" "t a" #'ml/npm-test)
      (:desc "Run all tests" "t s" #'ml/npm-test-file)
      (:desc "Yalc push package" "p" #'ml/yalc-push))

(set-popup-rule! "^\\*npm:" :size 0.3 :ttl 0)

(add-hook! '(js2-mode typescript-mode)
  (set (make-local-variable 'prettify-symbols-alist)
       '(("&&" . ?∧)
         ("||" . ?∨))
       ))

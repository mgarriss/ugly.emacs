(setq enh-ruby-program "/Users/mgarriss/.rvm/rubies/ruby-1.9.2-p290/bin/ruby")
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.rjs$"     . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$"     . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ctl$"     . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"   . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$"  . ruby-mode))

(require 'ruby-mode)

(autoload 'test-case-mode "test-case-mode" nil t)
(autoload 'enable-test-case-mode-if-test "test-case-mode")
(autoload 'test-case-find-all-tests "test-case-mode" nil t)
(autoload 'test-case-compilation-finish-run-all "test-case-mode")

(add-hook 'find-file-hook 'enable-test-case-mode-if-test)
(add-hook 'compilation-finish-functions 'test-case-compilation-finish-run-all)

(add-to-list 'auto-mode-alist '("\\.feature$"  . feature-mode))

(autoload 'yaml-mode "yaml-mode" "YAML" t)
(add-to-list 'auto-mode-alist '("\\.yml$"  . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(add-hook 'yaml-mode-hook 'ugly-yaml-mode-hook)
(add-hook 'haml-mode-hook 'ugly-haml-mode-hook)
(add-hook 'rhtml-mode-hook 'ugly-rhtml-mode-hook)
(add-hook 'ruby-mode-hook 'ugly-ruby-mode-hook)

;; (require 'rinari)

(defun ugly-yaml-mode-hook ()
  (define-rails-function-keys yaml-mode-map)
  (ugly-programming-mode-hook))

(defun ugly-rhtml-mode-hook ()
  (define-rails-function-keys rhtml-mode-map)
  (ugly-programming-mode-hook))

(defun ugly-haml-mode-hook ()
  (inf-ruby-keys)
  (define-rails-function-keys haml-mode-map)
  (ugly-programming-mode-hook)
  (define-key map '[f5] 'rinari-rgrep))

(defun define-rails-function-keys (map)
  (define-key map '[f5] 'rinari-rgrep))

(defun define-ruby-function-keys (map) nil)

(defun ugly-ruby-mode-hook ()
  ;; (inf-ruby-keys)
  (define-rails-function-keys ruby-mode-map)
  (define-ruby-function-keys ruby-mode-map)
  (ugly-programming-mode-hook)
  (define-key ruby-mode-map (kbd "H-s") 'toggle-buffer)
)

(defun ugly-rinari-minor-mode-hook ()
  (define-rinari-function-keys rinari-minor-mode-map))

(setq ruby-deep-indent-paren '(?\( t))

;; (add-to-list 'interpreter-mode-alist '(("ruby" . ruby-mode)) t)
;; (setq inferior-ruby-first-prompt-pattern "^>>")
;; (autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
;; (autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

(defun rinari-root (&optional dir home)
  (or dir (setq dir default-directory))
  (if (file-exists-p (expand-file-name
		      "boot.rb" (expand-file-name "config" dir)))
      dir
    (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
      (unless (string-match "\\(^[[:alpha:]]:/$\\|^/$\\)" dir)
	(rinari-root new-dir)))))

(defvar rinari-rgrep-file-endings
  "*.[^l]*"
  "Ending of files to search for matches using `rinari-rgrep'")

(defun rinari-rgrep (&optional arg)
  "Search through the rails project for a string or `regexp'.
With optional prefix argument just run `rgrep'."
  (interactive "P")
  (grep-compute-defaults)
  (if arg (call-interactively 'rgrep)
    (let ((word (thing-at-point 'word)))
      (funcall 'rgrep (read-from-minibuffer "search for: " word)
	       rinari-rgrep-file-endings (rinari-root)))))

(defun pmade-rhtml-mode-hook ()
  "Stuff common across all RHTML buffers"
  (ugly-programming-mode-hook)
  (setq fill-column 120))

(autoload 'rhtml-mode "rhtml-mode" "RHTML" t)
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.html\.erb$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))
(add-hook 'rhtml-mode-hook 'pmade-rhtml-mode-hook)

(add-to-list 'exec-path "/usr/local/bin")
(setq magit-git-executable "/usr/local/bin/git")

(add-to-list 'load-path "~/.emacs.d/packages/magit")
(global-set-key '[f6] 'magit-status)

(add-hook 'ruby-mode-hook
          (lambda ()
            (modify-syntax-entry ?- "w")
            (modify-syntax-entry ?_ "w")
            ))

(require 'guard)

(add-to-list 'same-window-buffer-names "*grep*")
(add-to-list 'same-window-buffer-names "*info*")
(add-to-list 'same-window-buffer-names "*shell*")
(add-to-list 'same-window-buffer-names "*compilation*")
(add-to-list 'same-window-buffer-names "*Help*")

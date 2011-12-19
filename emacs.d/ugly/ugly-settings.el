(setq
  inhibit-startup-message              t
  ring-bell-function                   (lambda ())
  visible-bell                         nil
  column-number-mode                   t
  enable-recursive-minibuffers         t
  echo-keystrokes                      0.1
  disabled-command-hook                nil
  default-major-mode                   'text-mode
  truncate-partial-width-windows       nil
  show-trailing-whitespace             t
  default-frame-alist                  '((cursor-type . bar) (cursor-color . "yellow"))
  default-indicate-buffer-boundaries   'left
  default-indicate-empty-lines         t
  font-lock-maximum-decoration         t
  next-line-add-newlines               nil
  paren-priority                       'both
  paren-sexp-mode                      nil
  paren-match-face                     'show-paren-match-face
  mark-even-if-inactive                t
  server-visit-hook                    (quote (save-place-find-file-hook))
  cua-enable-cua-keys                  t
  backup-directory-alist               '(("." . "~/.emacs.d/backups"))
  delete-old-versions                  t
  kept-new-versions                    6
  kept-old-versions                    2
  version-control                      t
  backup-by-copying                    t
  message-log-max                      500
  max-mini-window-height               10
  interprogram-cut-function            'paste-to-osx
  interprogram-paste-function          'copy-from-osx
  css-indent-offset                    2
  sgml-basic-offset                    2
  sh-basic-offset                      2
  exec-path                            (append exec-path '("/usr/local/bin"))
  auto-mode-alist                      (cons '("\\.markdown" . markdown-mode) auto-mode-alist)
  ;; toggle-mapping-style                 'rails
  font-lock-maximum-decoration         t
  dired-listing-switches               "-ao"
)

(setq-default indent-tabs-mode nil)
(setq-default save-place t)
(set-default 'require-final-newline t)

(set-default 'truncate-lines nil)

;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;; (setenv "GEM_PATH" "/Users/mgarriss/.rvm/gems/ruby-1.8.7-p249@api_admin:/Users/mgarriss/.rvm/gems/ruby-1.8.7-p249@global")
;; (setenv "GEM_HOME" "/Users/mgarriss/.rvm/gems/ruby-1.8.7-p249@api_admin")

(autoload 'css-mode "css-mode" "CSS" t)
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(autoload 'js2-mode "js2" nil t)

(add-hook 'js2-mode-hook 'ugly-programming-mode-hook)
(add-hook 'css-mode-hook 'ugly-programming-mode-hook)
(add-hook 'html-mode-hook 'ugly-programming-mode-hook)
(add-hook 'sh-mode-hook 'ugly-programming-mode-hook)
(add-hook 'sql-mode-hook 'ugly-programming-mode-hook)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 4)
            (ugly-programming-mode-hook)))

(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; (fringe-mode 'default)
(global-font-lock-mode t)
(show-paren-mode t)
(cua-mode)
(cua-selection-mode t)
(transient-mark-mode t)
(set-frame-parameter nil 'fullscreen 'fullboth)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Tramp Fns

(setq tramp-default-user "root")
(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
	 ;; use a separate history list for "root" files.
	 (file-name-history find-file-root-history)
	 (name (or buffer-file-name default-directory))
	 (tramp (and (tramp-tramp-file-p name)
		     (tramp-dissect-file-name name)))
	 path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
	    dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(global-set-key [(control x) (control r)] 'find-file-root)

(defface find-file-root-header-face
  '((t (:foreground "white" :background "red3")))
  "*Face use to display header-lines for files opened as root.")

(defun find-file-root-header-warning ()
  "*Display a warning in header line of the current buffer.
   This function is suitable to add to `find-file-root-hook'."
  (let* ((warning "WARNING: EDITING FILE AS ROOT!")
	 (space (+ 6 (- (window-width) (length warning))))
	 (bracket (make-string (/ space 2) ?-))
	 (warning (concat bracket warning bracket)))
    (setq header-line-format
	  (propertize  warning 'face 'find-file-root-header-face))))

(add-hook 'find-file-root-hook 'find-file-root-header-warning)

(defvar find-file-root-log "~/system/root-log"
  "*ChangeLog in which to log changes to system files.")

(defun find-file-root-log-do-it()
  "Add an entry for the current buffer to `find-file-root-log'."
  (let ((add-log-mailing-address "root@localhost")
	(add-log-full-name "")
	(add-log-file-name-function 'identity)
	(add-log-buffer-file-name-function
	 (lambda () ;; strip tramp prefix
	   (tramp-file-name-localname
	    (tramp-dissect-file-name
	     (or buffer-file-name default-directory)))
	   )))
    (add-change-log-entry nil find-file-root-log 'other-window)))

(defun find-file-root-log-on-save ()
  "*Prompt for a log entry in `find-file-root-log' after saving a root file.
   This function is suitable to add to `find-file-root-hook'."
  (add-hook 'after-save-hook 'find-file-root-log-do-it 'append 'local))

(add-hook 'find-file-root-hook 'find-file-root-log-on-save)


;; --------

(set-default-font "-*-ProFont-normal-normal-normal-*-9-*-*-*-p-0-iso10646-1")
(modify-frame-parameters nil '((wait-for-wm . nil)))

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(and terminal-frame (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)

;; auto indent on yanks
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(defvar flymake-fringe-overlays nil)
(make-variable-buffer-local 'flymake-fringe-overlays)

(defadvice flymake-make-overlay (after add-to-fringe first
                                       (beg end tooltip-text face mouse-face)
                                       activate compile)
  (push (fringe-helper-insert-region
         beg end
         (fringe-lib-load (if (eq face 'flymake-errline)
                              fringe-lib-exclamation-mark
                            fringe-lib-question-mark))
         'left-fringe 'font-lock-warning-face)
        flymake-fringe-overlays))

(defadvice flymake-delete-own-overlays (after remove-from-fringe activate
                                              compile)
  (mapc 'fringe-helper-remove flymake-fringe-overlays)
  (setq flymake-fringe-overlays nil))

(setq grep-find-command 
      "find . -type f '!' -wholename '*/.svn/*' -print0 | xargs -0 -e grep -nH -e ")

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/ac-dict")
(ac-config-default)

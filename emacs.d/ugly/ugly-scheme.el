;; (autoload 'run-scheme "cmuscheme"
;;           "Run an inferior Scheme process."
;;           t)

;; (setq auto-mode-alist 
;;       (cons '("\\.scm$" . scheme-mode)  
;;             auto-mode-alist))

;; (setq scheme-program-name "guile")
;; (require 'cmuscheme)

;; (autoload 'scheme-smart-complete "scheme-complete" nil t)
;; (eval-after-load 'scheme
;;   '(progn (define-key scheme-mode-map "\e\t" 'scheme-smart-complete)))

;; ;; Alternately, you may want to just bind TAB to the
;; ;; `scheme-complete-or-indent' function, which indents at the start
;; ;;zsh of a line and otherwise performs the smart completion:

;; (eval-after-load 'scheme
;;   '(progn (define-key scheme-mode-map "\t" 'scheme-complete-or-indent)))

;; (require 'comint)
;; (require 'gds)

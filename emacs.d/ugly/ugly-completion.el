(defun tags-complete-tag (string predicate what)
  (save-excursion
    ;; If we need to ask for the tag table, allow that.
    (if (eq what t)
	(all-completions string (tags-completion-table) predicate)
      (try-completion string (tags-completion-table) predicate))))(defun he-tag-beg ()
  (let ((p
         (save-excursion 
           (backward-word 1)
           (point))))
    p))

(defun try-expand-tag (old)
  (unless  old
    (he-init-string (he-tag-beg) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string 'tags-complete-tag) 'string-lessp)))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

(defun try-my-dabbrev-substring (old)
  (let ((old-fun (symbol-function 'he-dabbrev-search)))
    (fset 'he-dabbrev-search (symbol-function 'my-dabbrev-substring-search))
    (unwind-protect
        (try-expand-dabbrev old)
      (fset 'he-dabbrev-search old-fun))))


(defun my-dabbrev-substring-search (pattern &optional reverse limit)
  (let ((result ())
	(regpat (cond ((not hippie-expand-dabbrev-as-symbol)
		       (concat (regexp-quote pattern) "\\sw+"))
		      ((eq (char-syntax (aref pattern 0)) ?_)
		       (concat (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+"))
		      (t
		       (concat (regexp-quote pattern)
			       "\\(\\sw\\|\\s_\\)+")))))
    (while (and (not result)
		(if reverse
                    (re-search-backward regpat limit t)
                  (re-search-forward regpat limit t)))
      (setq result (buffer-substring-no-properties (save-excursion
                                                     (goto-char (match-beginning 0))
                                                     (skip-syntax-backward "w_")
                                                     (point))
						   (match-end 0)))
      (if (he-string-member result he-tried-table t)
	  (setq result nil)))     ; ignore if bad prefix or already in table
    result))

(setq hippie-expand-verbose t)
(setq hippie-expand-max-buffers nil)

(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-my-dabbrev-substring
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        try-expand-line
        try-expand-line-all-buffers
        try-expand-tag
;;        try-expand-dcsh-command
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
))


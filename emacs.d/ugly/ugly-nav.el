(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list 
              (lambda (a b)
                (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                      (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                  (if (= (nth 0 ta) (nth 0 tb))
                      (> (nth 1 ta) (nth 1 tb))
                    (> (nth 0 ta) (nth 0 tb)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (if (string-equal (substring x 0 1) ".") x))
              ido-temp-list))))

(add-hook 'ido-setup-hook 
  (lambda ()
    (define-key ido-file-dir-completion-map "\C-n" 'ido-next-work-directory)
    (define-key ido-file-dir-completion-map "\C-p" 'ido-prev-work-directory)
    (define-key ido-file-completion-map     "\C-w" 'ido-delete-backward-word-updir)))
(setq
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ido-show-dot-for-dired t
  ido-enable-prefix nil
  ido-enable-flex-matching t
  ido-use-filename-at-point nil
  ido-use-url-at-point nil
  ido-confirm-unique-completionion nil
  ido-completion-buffer-all-completions t
  ido-case-fold  t
  ido-enable-last-directory-history t
  ido-default-file-method 'selected-window
  ido-enable-tramp-completion t
  ido-work-directory-match-only nil
  ido-auto-merge-delay-time 0.35
  ido-max-prospects 48
  ido-max-work-directory-list 16
  ido-max-work-file-list 20
  ido-max-dir-file-cache 400
  ido-auto-merge-work-directories-length 0)

(ido-mode 'both)
(ido-everywhere t)

(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

(setq tags-table-list
      '("~/develop/demandbase/admin"))

;; (defadvice find-tag (before c-tag-file activate)
;;    "Automatically create tags file."
;;    (let ((tag-file (concat default-directory "TAGS")))
;;      (unless (file-exists-p tag-file)
;;        (shell-command "etags *.[ch] -o TAGS 2>/dev/null"))
;;      (visit-tags-table tag-file)))

(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names))))

(define-key global-map (kbd "H-.") 'my-ido-find-tag)

(defun ugly-next-tag ()
  (interactive)
  (universal-argument)
  (find-tag last-tag t))

(define-key global-map (kbd "s-.") 'ugly-next-tag)

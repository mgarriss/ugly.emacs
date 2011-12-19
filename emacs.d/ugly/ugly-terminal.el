(setq ansi-term-color-vector [unspecified "#000000" "#963F3C" "#5FFB65" "#FFFD65"
                                          "#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"])

(setq multi-term-program "/usr/local/bin/zsh")

(defun ugly-term-toggle-mode ()
  (interactive)
  (if (term-in-line-mode) (term-char-mode) (term-line-mode)))

(defun ugly-term-yank ()
  (interactive)
  (term-line-mode)
  (yank)
  (term-char-mode))

(defun ugly-term-kill-ring-save ()
  (interactive)
  (kill-ring-save (region-beginning) (region-end))
  (term-char-mode))

(defun ugly-term-set-mark ()
  (interactive)
  (term-line-mode)
  (cua-set-mark))

 (defun ugly-term-mode-hook ()
   (define-key term-mode-map (kbd "H-j")   'ugly-term-toggle-mode)
   (define-key term-mode-map (kbd "M-w")   'ugly-term-kill-ring-save)
   
   (define-key term-raw-map  (kbd "H-j")   'ugly-term-toggle-mode)
   (define-key term-raw-map  (kbd "C-SPC") 'ugly-term-set-mark)
   (define-key term-raw-map  "\C-y"        'ugly-term-yank)
   (define-key term-raw-map  "\C-c"  (lambda () (interactive) (term-send-raw-string "")))
   (define-key term-raw-map [escape] (lambda () (interactive) (term-send-raw-string "")))
   (define-key term-raw-map  "\M-y"        'yank-pop))

;; (defun ugly-term-mode-hook ()
;;  (define-key term-raw-map [f1] (lambda () (interactive) (term-send-raw-string "")))
;;  (define-key term-raw-map [f2] (lambda () (interactive) (term-send-raw-string "")))
;;  (define-key term-raw-map [f3] (lambda () (interactive) (term-send-raw-string "")))
;;  (define-key term-raw-map [f4] (lambda () (interactive) (term-send-raw-string "")))


(add-hook 'term-mode-hook 'ugly-term-mode-hook)

(defun switch-to-terminal-0 ()
  (interactive)
  (switch-to-buffer "*terminal*"))

(defun switch-to-terminal-n (n)
  (let ((name (concat "*terminal*<" (number-to-string n) ">")))
    (if (get-buffer name)
      (switch-to-buffer name)
      (progn (multi-term) (rename-buffer name)))))

;; (define-key global-map (kbd "H-0") (lambda () (interactive) (switch-to-terminal-n 0))) 
;; (define-key global-map (kbd "H-1") (lambda () (interactive) (switch-to-terminal-n 1))) 
;; (define-key global-map (kbd "H-2") (lambda () (interactive) (switch-to-terminal-n 2))) 
;; (define-key global-map (kbd "H-3") (lambda () (interactive) (switch-to-terminal-n 3))) 
;; (define-key global-map (kbd "H-4") (lambda () (interactive) (switch-to-terminal-n 4))) 
;; (define-key global-map (kbd "H-5") (lambda () (interactive) (switch-to-terminal-n 5))) 
;; (define-key global-map (kbd "H-6") (lambda () (interactive) (switch-to-terminal-n 6))) 
;; (define-key global-map (kbd "H-7") (lambda () (interactive) (switch-to-terminal-n 7))) 
;; (define-key global-map (kbd "H-8") (lambda () (interactive) (switch-to-terminal-n 8))) 
;; (define-key global-map (kbd "H-9") (lambda () (interactive) (switch-to-terminal-n 9))) 

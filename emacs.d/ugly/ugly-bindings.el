(setq
 mac-function-modifier                'super
 mac-option-modifier                  'hyper
 mac-command-modifier                 'meta
)

(define-key global-map "\C-o"      'open-line-below-like-vim)
(define-key global-map "\M-o"      'open-line-above-like-vim)
(define-key global-map "\C-s"      'isearch-forward-regexp)
(define-key global-map "\C-r"      'isearch-backward-regexp)
(define-key global-map "\C-\M-s"   'isearch-forward)
(define-key global-map "\C-\M-r"   'isearch-backward)
(define-key global-map "\C-w"      'kill-region-or-backward-kill-word)
;; (define-key global-map "\t"        'pmade-smart-tab)
(define-key global-map (kbd "H-x") 'transpose-windows)
(define-key global-map "\C-c\M-w"  'save-to-kill-ring-and-normalize-whitespace)
(define-key global-map (kbd "H-;") 'comment-region)
(define-key global-map (kbd "H-'") 'uncomment-region)
(define-key global-map (kbd "H-d") 'igrek-delete-syntax-group)
(define-key global-map (kbd "H-f") 'toggle-fullscreen)
(define-key global-map (kbd "H-j") 'delete-indentation)
(define-key global-map "\C-v"	   'pager-page-down)
(define-key global-map "\M-v"	   'pager-page-up)
(define-key global-map '[M-up]     'pager-row-up)
(define-key global-map '[M-down]   'pager-row-down)
;; (define-key global-map '[H-left]   'iflipb-previous-buffer)
;; (define-key global-map '[H-right]  'iflipb-next-buffer)
(define-key global-map '[H-up]     'kill-current-buffer-iflipb-next-buffer)
(define-key global-map '[H-down]   'delete-window)
(define-key global-map (kbd "s-a") 'point-to-register)
(define-key global-map (kbd "s-`") 'jump-to-register)
(define-key global-map "\C-_"      'undo-only)
(define-key global-map (kbd "H-t") 'transpose-lines)
(define-key global-map [M-f1]      'bookmark-bmenu-list)
(define-key global-map (kbd "H-y") 'copy-line-below)
(define-key global-map '[C-up]     'windmove-up)
(define-key global-map '[C-down]   'windmove-down)
(define-key global-map (kbd "<home>") 'iflipb-previous-buffer)
(define-key global-map (kbd "<end>") 'iflipb-next-buffer)
;; (define-key global-map '[C-right]  'iflipb-next-buffer)

(define-key global-map (kbd "<f1>") 'rgrep)
(define-key global-map (kbd "S-<f1>") 'next-match)

(define-key global-map (kbd "<f1>") 'occur)
(define-key global-map (kbd "C-<f1>") 'rgrep)
(define-key global-map (kbd "S-<f1>") 'next-match)

(define-key global-map (kbd "<f2>") 'my-ido-find-tag)
(define-key global-map (kbd "C-<f2>") 'pop-tag-mark)
(define-key global-map (kbd "S-<f2>") 'ugly-tag-next)

(require 'flymake)

(global-set-key (kbd "C-<f3>") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "S-<f3>") 'flymake-goto-next-error)


(define-key global-map "\C-xf"     'recentf-open-files)
(define-key global-map "\C-z"      'replace-string)

(define-key ac-mode-map (kbd "<tab>") 'auto-complete)

(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.02)

(key-chord-define-global "mk" 'switch-to-previous-buffer)
(key-chord-define-global ";'" 'goto-line)
(key-chord-define-global "qw" 'kmacro-start-macro)
(key-chord-define-global "qr" 'kmacro-end-macro)
(key-chord-define-global "qe" 'kmacro-end-and-call-macro)


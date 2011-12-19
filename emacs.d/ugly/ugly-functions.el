(defun copy-line-below ()
  "Copy and insert line point is on one line below point."
  (interactive)
  (move-beginning-of-line nil)
  (push-mark (point))
  (forward-line 1)
  (copy-region-as-kill (mark) (point))
  (yank)
  (previous-line))

(defun open-line-below-like-vim ()
  "Open a line below the point, and move there"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun open-line-above-like-vim ()
  "Open a line above the point, and move there"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun switch-to-previous-buffer ()
  "Switch back to the previous buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun kill-region-or-backward-kill-word (arg)
  "If there is a region, kill it.  Otherwise kill the word before point"
  (interactive "*p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word (- arg)) (point)))))

(defun save-to-kill-ring-and-normalize-whitespace ()
  (interactive)
  (let ((text (buffer-substring (region-beginning) (region-end))))
    (kill-new
     (replace-regexp-in-string "^\s+" "" (replace-regexp-in-string "\n\s*" " " text))))
  (deactivate-mark))

(defvar pmade-inside-smart-tab nil)

(defun pmade-smart-org-cycle ()
  "Prevent recursion with org-mode"
  (if (not pmade-inside-smart-tab)
      (let ((pmade-inside-smart-tab t)) (org-cycle))
    (indent-for-tab-command)))

(defun pmade-smart-tab ()
  "Context based tab key.  If there is a region, indent the
  region.  Otherwise attempt to perform tab completion or
  indentation."
  (interactive)
  (cond
   ((minibufferp) (minibuffer-complete))
   ((string= major-mode "org-mode") (pmade-smart-org-cycle))
   (mark-active (indent-region (region-beginning) (region-end)))
   ((looking-at "\\_>") (hippie-expand nil))
   (t (indent-for-tab-command))))

(defun pmade-newline ()
  "Emulate newline-and-indent for modes that don't have it"
  (interactive)
  (newline)
  (indent-according-to-mode))

(defun ugly-programming-mode-hook ()
  (setq show-trailing-whitespace nil
        save-place t)
  (flyspell-prog-mode)
  (font-lock-add-keywords nil '(("\\<\\(FIXME:\\|TODO:\\|THINK:\\)" 1 pmade-fixme-face t)))
  (local-set-key "\C-m" 'pmade-newline)
  (local-set-key "\t"   'pmade-smart-tab))

(defun toggle-fullscreen () 
  (interactive) 
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun igrek-delete-syntax-group ()
  (interactive)
  (kill-region (point)
               (progn
                 (skip-syntax-forward (char-to-string (char-syntax (following-char))))
                 (point))))

(defun next-user-buffer () 
  "Switch to the next user buffer in cyclic order.\n 
User buffers are those not starting with *." 
  (interactive) 
  (next-buffer) 
  (let ((i 0)) 
    (while (and (string-match "^*|^TAGS" (buffer-name)) (< i 50)) 
      (setq i (1+ i)) (next-buffer) ))) 

(defun previous-user-buffer () 
  "Switch to the previous user buffer in cyclic order.\n 
User buffers are those not starting with *." 
  (interactive) 
  (previous-buffer) 
  (let ((i 0)) 
    (while (and (string-match "^*" (buffer-name)) (< i 50)) 
      (setq i (1+ i)) (previous-buffer) ))) 

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (buffer-name)))

(defun kill-current-buffer-next-user-buffer ()
  (interactive)
  (kill-current-buffer)
  (next-user-buffer))

(defun kill-current-buffer-iflipb-next-buffer ()
  (interactive)
  (kill-current-buffer)
  (iflipb-previous-buffer))

(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun pmade-comint-mode-hook()
  (setq comint-process-echoes t)
  (ansi-color-for-comint-mode-on))

(add-hook 'comint-mode-hook 'pmade-comint-mode-hook)
 
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

(defun ugly-four-column-split ()
  (interactive)
  (split-window-horizontally)
  (split-window-horizontally)
  (windmove-right)
  (windmove-right)
  (split-window-horizontally))

(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

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

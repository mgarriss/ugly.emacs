(require 'shell)

(defcustom guard-use-ui nil
  "Should we use test-unit's UI?"
  :group 'guard
  :type '(boolean))

(defcustom guard-command "guard"
  "Command name to use to execute guard."
  :group 'guard
  :type '(string))

(defun guard ()
  "Fire up an instance of guard in its own buffer with shell bindings and compile-mode highlighting and linking."
  (interactive)
  (let ((buffer (shell "*guard*")))

    (define-key shell-mode-map "\C-c\C-a" 'guard-switch)

    (set (make-local-variable 'comint-output-filter-functions)
	 '(comint-truncate-buffer
	   comint-postoutput-scroll-to-bottom
	   ansi-color-process-output
	   ))
    (set (make-local-variable 'comint-buffer-maximum-size) 5000)
    (set (make-local-variable 'comint-scroll-show-maximum-output) t)
    (set (make-local-variable 'comint-scroll-to-bottom-on-output) t)

    (set (make-local-variable 'compilation-error-regexp-alist)
         '(
           ("^ +\\(#{RAILS_ROOT}/\\)?\\([^(:]+\\):\\([0-9]+\\)" 2 3)
           ("\\[\\(.*\\):\\([0-9]+\\)\\]:$" 1 2)
           ("^ *\\([[+]\\)?\\([^:
]+\\):\\([0-9]+\\):" 2 3)
           ("^.* at \\([^:]*\\):\\([0-9]+\\)$" 1 2)
           ))
    (ansi-color-for-comint-mode-on)
    (compilation-shell-minor-mode)
    (comint-send-string buffer (concat guard-command "\n"))))

(defun guard-switch ()
  "Switch back and forth between guard and the previous buffer"
  (interactive)
  (if (equal "*guard*" (buffer-name))
      (switch-to-buffer (other-buffer))
    (switch-to-buffer "*guard*")))

(eval-when-compile
  (require 'unit-test nil t))

(if (and guard-use-ui (require 'unit-test nil t))
    (progn
      (message "starting emacs server for guard")
      (setq unit-test-colours (acons "gray" "#999999" unit-test-colours))
      (setq unit-test-colours (acons "dark-gray" "#666666" unit-test-colours))
      (setq unit-test-running-xpm (unit-test-dot "gray"))
      (server-start)
      (defun guard-update (status)
        "Updates all buffer's modeline with the current test status."
        (interactive "S")
        (let ((guard-map (make-sparse-keymap)))
          (define-key guard-map [mode-line mouse-1] 'guard-switch)
          (mapcar (lambda (buffer)
                    (with-current-buffer buffer
                      (if (eq status 'quit)
                          (show-test-none)
                        (progn
                          (show-test-status status)
                          (put-text-property
                           0 3
                           'keymap guard-map
                           (car mode-line-buffer-identification))))))
                  (remove-if 'minibufferp (buffer-list))))
        status))
  (message "unit-test not found, not starting guard/emacs integration"))

(provide 'guard)

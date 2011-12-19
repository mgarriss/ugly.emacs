(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/packages/modes")
(add-to-list 'load-path "~/.emacs.d/packages/ruby")
(add-to-list 'load-path "~/.emacs.d/packages/rhtml")
(add-to-list 'load-path "~/.emacs.d/packages/magit")
(add-to-list 'load-path "~/.emacs.d/ugly")

(load "ugly-settings")
(load "ugly-personal-settings")

(require 'color-theme)
(color-theme-initialize)
(require 'ugly-colors)
(ugly-colors)

(load "ugly-terminal")
(load "ugly-functions")
(load "ugly-nav")
(load "ugly-org")
(load "ugly-completion")

(require 'fringe)
(require 'saveplace)
(require 'unbound)
(require 'comint)
(require 'gist) 
(require 'pager)
(require 'iflipb)
(require 'key-chord)
(require 'etags)
(require 'ido)
(require 'ruby-electric)
(require 'fringe-helper)

(require 'haml-mode)
(require 'sass-mode)
(require 'feature-mode)
(require 'rhtml-mode)
(require 'yaml-mode)

(require 'magit)
;; (require 'autotest)
(require 'toggle)
(require 'dired-details)

(require 'yari)
(defun ri-bind-key ()
  (local-set-key [f1] 'yari-anything))

(add-hook 'ruby-mode-hook 'ri-bind-key)

(load-library "jj") ;; html output

(load-library "clang-completion-mode")
(dired-details-install)

(load "ugly-bindings")
(load "ugly-ruby")

(setq server-name "ugly")
(add-hook 'after-init-hook 'server-start)

;;; magit-topgit.el --- topgit plug-in for Magit

;; Copyright (C) 2010  Nathan Weizenbaum
;; Copyright (C) 2010  Yann Hodique
;;
;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This plug-in provides topgit functionality as a separate component of Magit

;;; Code:

(require 'magit)
(eval-when-compile
  (require 'cl))

(defcustom magit-topgit-executable "tg"
  "The name of the TopGit executable."
  :group 'magit
  :type 'string)

(defcustom magit-topgit-branch-prefix "t/"
  "Convention prefix for topic branch creation."
  :group 'magit
  :type 'string)

(defface magit-topgit-current
  '((t :weight bold :inherit magit-branch))
  "Face for section titles."
  :group 'magit)

;;; Topic branches (using topgit)

(defun magit-topgit-in-topic-p ()
  (and (file-exists-p ".topdeps")
       (executable-find magit-topgit-executable)))

(defun magit-topgit-create-branch (branch parent)
  (when (zerop (or (string-match magit-topgit-branch-prefix branch) -1))
    (magit-run* (list magit-topgit-executable "create"
                      branch (magit-rev-to-git parent))
                nil nil nil t)
    t))

(defun magit-topgit-pull ()
  (when (magit-topgit-in-topic-p)
    (magit-run* (list magit-topgit-executable "update")
                nil nil nil t)
    t))

(defun magit-topgit-push ()
  (when (magit-topgit-in-topic-p)
    (let* ((branch (or (magit-get-current-branch)
                       (error "Don't push a detached head.  That's gross")))
           (remote (magit-get "topgit" "remote"))
           (push-remote (if (or current-prefix-arg (not remote))
                            (magit-read-remote (format "Push %s to" branch))
                          remote)))
      (when (and (not remote)
                 (not current-prefix-arg))
        (magit-set push-remote "topgit" "remote"))
      (magit-run magit-topgit-executable "push" "-r" push-remote))
    t))

(defun magit-topgit-remote-update (&optional remote)
  (when (magit-topgit-in-topic-p)
    (let* ((remote (magit-get "topgit" "remote"))
           (remote-update (if (or current-prefix-arg (not remote))
                              (magit-read-remote)
                            remote)))
      (if (and (not remote)
               (not current-prefix-arg))
          (progn
            (magit-set remote-update "topgit" "remote")
            (magit-run magit-topgit-executable "remote"
                       "--populate" remote-update)))
      (magit-run magit-topgit-executable "remote" remote-update))
    ;; We return nil anyway, as we also want regular "git remote update" to
    ;; happen
    nil))

(defun magit-topgit-parse-flags (flags-string)
  (let ((flags (string-to-list flags-string))
        (void-flag ?\ ))
    (list :current (not (eq (nth 0 flags) void-flag))
          :empty (not (eq (nth 1 flags) void-flag)))))

(defun magit-topgit-wash-topic ()
  (let ((fmt "^\\(.\\{7\\}\\)\\s-\\(\\S-+\\)\\s-+\\(.*\\)"))
    (if (search-forward-regexp fmt (line-end-position) t)
        (let ((flags (magit-topgit-parse-flags (match-string 1)))
              (topic (match-string 2)))
          (goto-char (line-beginning-position))
          (delete-char 8)
          (insert "\t")
          (goto-char (line-beginning-position))
          (magit-with-section topic 'topic
            (magit-set-section-info topic)
            (let ((beg (1+ (line-beginning-position)))
                  (end (line-end-position))) 
              (when (plist-get flags :current)
                (put-text-property beg end 'face 'magit-topgit-current))
              (when (plist-get flags :empty)
                (put-text-property beg end 'face `(:strike-through t :inherit ,(get-text-property beg 'face)))))
            (forward-line)))
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    t))

(defun magit-topgit-wash-topics ()
  (let ((magit-old-top-section nil))
    (magit-wash-sequence #'magit-topgit-wash-topic)))

(defun magit-topgit-section (section title washer &rest args)
  (let ((magit-git-executable magit-topgit-executable)
        (magit-git-standard-options nil))
    (apply 'magit-git-section section title washer args)))

(magit-define-inserter topics ()
  (when (executable-find magit-topgit-executable)
      (magit-topgit-section 'topics
                            "Topics:" 'magit-topgit-wash-topics
                            "summary")))

(magit-add-action (item info "discard")
  ((topic)
   (when (yes-or-no-p "Discard topic? ")
     (magit-run* (list magit-topgit-executable "delete" "-f" info)
                 nil nil nil t))))

(magit-add-action (item info "visit")
  ((topic)
   (magit-checkout info)))

(add-hook 'magit-after-insert-stashes-hook 'magit-insert-topics)

(add-hook 'magit-create-branch-command-hook 'magit-topgit-create-branch)
(add-hook 'magit-pull-command-hook 'magit-topgit-pull)
(add-hook 'magit-remote-update-command-hook 'magit-topgit-remote-update)
(add-hook 'magit-push-command-hook 'magit-topgit-push)

(provide 'magit-topgit)
;;; magit-topgit.el ends here

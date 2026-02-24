;;; init.el

;;; Imports

(add-to-list 'load-path (expand-file-name "org-study" user-emacs-directory))
(load "org-study")

;;; My functions

(defun andy/open-init-file ()
  (interactive)
  (find-file user-init-file))

;;; Emacs

(setq make-backup-files nil)
(tool-bar-mode -1)

(set-frame-parameter nil 'fullscreen 'maximized) ;; Maximize Emacs window when I open it

;;; Org mode

(setq org-directory (expand-file-name "~/Documents/andy_workspace/org/"))

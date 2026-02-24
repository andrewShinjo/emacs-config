;;; init.el

;;; Imports

(add-to-list 'load-path (expand-file-name "org-study-api" user-emacs-directory))
(load "org-study-api")

;;; My functions

(defun andy/open-init-file ()
  (interactive)
  (find-file user-init-file))

;;; Emacs

(setq make-backup-files nil)
(global-display-line-numbers-mode 1) ;; Always show line number
(tool-bar-mode -1)

(set-frame-parameter nil 'fullscreen 'maximized) ;; Maximize Emacs window when I open it

;;; Org mode

(setq org-directory (expand-file-name "~/Documents/andy_workspace/org/"))

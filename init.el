;;; init.el

;; Package Manager

(setq package-install-upgrade-built-in t)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;;; Imports

(let ((config-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "org-study" config-dir))
  (load "org-study-api"))

;;; Emacs

(setq debug-on-error t)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq tab-width 4)
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)
(tool-bar-mode -1)

;; Set the default font size.
(set-face-attribute 'default nil :height 180)
(set-frame-parameter nil 'fullscreen 'maximized)

;;; Org mode
(setq org-startup-with-latex-preview t)
(setq org-startup-with-inline-images t)
(setq org-cycle-hide-drawer-startup t)

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.5))
(setq org-directory (expand-file-name "~/Documents/Org"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ghostel orderless org-bullets org-hide-drawers org-modern org-roam
			 org-tidy svg-tag-mode transient vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Org Roam

(setq org-roam-directory org-directory)
(org-roam-db-autosync-mode)

;;; Keybindings

(global-set-key (kbd "RET") 'newline)
(global-set-key (kbd "s-o") 'org-roam-node-find)
(global-set-key (kbd "s-p") 'execute-extended-command)

;;; orderless

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

;;; vertico

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

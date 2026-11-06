;; -*- lexical-binding: t; -*-

;;; init.el

;;; Package Manager

;; Built-in packages are eligible for upgrades from package archives.
(setq package-install-upgrade-built-in t)

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Global Keybindings

;; Press command-[ to open the command palette.
(global-set-key (kbd "s-p") 'execute-extended-command)
;; Press command+/ to open the Emacs init file.
(global-set-key (kbd "s-/") 'andy/open-init-file)
;; Press command+[ to search for or create a new Org roam node, and insert the link.
(global-set-key (kbd "s-[") 'org-roam-node-insert)
;; Press command+o to search for or create a new Org roam node, and open it.
(global-set-key (kbd "s-o") 'org-roam-node-find)
;; Press command+d to open today's daily note.
(global-set-key (kbd "s-d") 'org-roam-dailies-goto-today)

;;; Personal functions:

(defun andy/open-init-file()
  "Open the user's init file."
  (interactive)
  (find-file user-init-file))

;;; Emacs

;; Enable workspace tab bar.
(tab-bar-mode 1)

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

(require 'org)

(setq org-startup-with-latex-preview t)
(setq org-startup-with-inline-images t)
(setq org-cycle-hide-drawer-startup t)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-directory (expand-file-name "~/Documents/org"))

;;; Org roam

(setq org-roam-directory org-directory)

;;; Org study

(let ((config-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "src/org-study" config-dir))
  (require 'org-study-api))

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


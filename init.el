;;; init.el

;; Package Manager

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Theme

(load-theme 'modus-vivendi t t)
(enable-theme 'modus-vivendi)

;;; Imports

(let ((config-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "org-study" config-dir))
  (add-to-list 'load-path (expand-file-name "org-visual-outline" config-dir))
  (load "org-study-api")
  (require 'org-visual-indent)
  (require 'org-dynamic-bullets))

;;; My functions

(defun andy/open-init-file ()
  (interactive)
  (find-file user-init-file))

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

(set-face-attribute 'default nil :height 176)
(set-frame-parameter nil 'fullscreen 'maximized)

;;; Org mode
(setq org-startup-with-latex-preview t)
(setq org-startup-with-inline-images t)
(setq org-cycle-hide-drawer-startup t)

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.5))
(setq org-directory (expand-file-name "~/Documents/Org"))

(setq org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis "…")

(with-eval-after-load 'org
  (set-face-attribute 'org-document-title nil :inherit 'variable-pitch :weight 'bold)
  (set-face-attribute 'org-level-1 nil :inherit 'variable-pitch :weight 'bold)
  (set-face-attribute 'org-level-2 nil :inherit 'variable-pitch :weight 'bold)
  (set-face-attribute 'org-level-3 nil :inherit 'variable-pitch :weight 'bold)
  (set-face-attribute 'org-level-4 nil :inherit 'variable-pitch :weight 'bold))

;;; Org modern look

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-todo nil)
  (org-modern-tag nil)
  (org-modern-hide-stars nil)
  (org-modern-star 'replace)
  (org-modern-fold-stars 'symbol)
  (org-modern-block-name t)
  (org-modern-block-fringe t)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.5)
   (org-modern-list '((?+ . "•") (?- . "•") (?* . "•"))))

(use-package org-hide-drawers
  :ensure t
  :hook (org-mode . org-hide-drawers-mode)
  :config
  (setopt org-hide-drawers-display-strings '((all ""))))

(add-hook 'org-mode-hook #'org-visual-indent-mode)
(add-hook 'org-mode-hook #'org-dynamic-bullets-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(orderless org-bullets org-hide-drawers org-modern org-roam org-tidy
			   svg-tag-mode vertico)))
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

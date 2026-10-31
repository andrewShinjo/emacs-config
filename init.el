;; -*- lexical-binding: t; -*-

;;; init.el

;;; Package Manager

;; Built-in packages are eligible for upgrades from package archives.
(setq package-install-upgrade-built-in t)

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Keybindings

(global-set-key (kbd "s-d") 'org-roam-dailies-goto-today)
(global-set-key (kbd "s-[") 'org-roam-node-insert)
(global-set-key (kbd "s-/") 'andy/open-init-file)

;;; Personal functions:

(defun andy/open-init-file()
  "Open the user's init file."
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

;; Set the default font size.
(set-face-attribute 'default nil :height 180)
(set-frame-parameter nil 'fullscreen 'maximized)

;;; Org mode

;; Load Org before touching its variables.  `setq' on an unbound symbol is
;; harmless, but `plist-put' must *read* the old value, so setting
;; `org-format-latex-options' below signals `void-variable' unless Org is
;; already loaded.  This also makes `org-directory' bound for Org Roam.
(require 'org)

(setq org-startup-with-latex-preview t)
(setq org-startup-with-inline-images t)
(setq org-cycle-hide-drawer-startup t)

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.5))
(setq org-directory (expand-file-name "~/Documents/Org"))

;;; Org study

;; The `org-study' package lives next to this file rather than in
;; `package-user-dir', so put it on `load-path' before loading it.
;; Resolve the directory from the file being loaded rather than from
;; `default-directory', so the config works from any working directory.
;; Keep this in a `let': a bare top-level `require' is evaluated by the
;; byte-compiler, which would need the local package during a build.
(let ((config-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "org-study" config-dir))
  ;; Loads org-study plus its review-queue entry point (org-study-api).
  (require 'org-study-api))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(anki-editor ghostel orderless org-bullets org-hide-drawers
				 org-modern org-roam org-tidy svg-tag-mode transient
				 vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Org Roam

;; Declare the variable so the byte-compiler knows it is not a typo; Org
;; Roam itself is autoloaded on the next line and keeps the value set here.
(defvar org-roam-directory)
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

;;; anki-editor

;; Install anki-editor.
(unless (package-installed-p 'anki-editor)
  (package-vc-install
   '(anki-editor . (:url "https://github.com/anki-editor/anki-editor"))))

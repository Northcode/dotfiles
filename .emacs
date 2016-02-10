(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(package-initialize)

(defvar my-packages '())
(setq my-packages
      '(
	;; evil
	evil
	evil-surround
	evil-org
	evil-magit
	
	;; other keybind 
	bind-key
	key-chord
	expand-region
	mark-multiple

	;; auto completion 
	company
	company-c-headers
	company-jedi
	company-web
	yasnippet

	;; helm 
	helm
	helm-projectile
	helm-company
	helm-swoop

	;; ace 
	ace-window
	ace-isearch

	;; project management
	projectile
	magit

	auto-install

	;; theme 
	spacegray-theme
	powerline
	powerline-evil

	;; org-mode
	org
	org-bullets
	org-journal
	
	))

(defun package-list-installed ()
  "Check if all packages in my package list is installed."
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      'nil
      't
      )))

(defun update-packages ()
  "Check packages for updates and install if there are updates"
  (unless (package-list-installed)
    (message "Update package db")
    (package-refresh-contents)
    (message "Done")

    (message "Updating packages")
    (dolist (p my-packages)
      (when (not (package-installed-p p))
	(package-install p)))
    (message "Packages updated!")
    ))

(update-packages)

(dolist (p my-packages)
  (if (featurep p)
      (require p)
    ))

(require 'auto-install)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.emacs.saves"))))
 '(blink-cursor-mode nil)
 '(company-auto-complete (quote (quote company-explicit-action-p)))
 '(custom-enabled-themes (quote (spacegray)))
 '(custom-safe-themes
   (quote
    ("d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" default)))
 '(eshell-buffer-shorthand t)
 '(evil-indent-convert-tabs nil)
 '(evil-mode t)
 '(evil-want-C-u-scroll t)
 '(global-company-mode t)
 '(helm-mode t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/")
 '(initial-scratch-message "")
 '(menu-bar-mode nil)
 '(projectile-global-mode t)
 '(scroll-bar-mode nil)
 '(tab-always-indent t)
 '(tool-bar-mode nil)
 '(yas-global-mode t nil (yasnippet)))
(evil-magit-init)
;; set themes
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(powerline-evil-vim-theme)

;; custom defuns
(defun save-all ()
    ;;; documentation-string
  "Save all files."
  (interactive)
  (save-some-buffers t))

;; hooks
(add-hook 'focus-out-hook 'save-all)

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t)
  (setq tabs-width 4)
  (setq indent-tabs-mode t)
  (setq flycheck-clang-language-standard "c++11")
  )
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; set html files to load web-mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; enable isearch fuzzy searching
(setq search-whitespace-regexp ".*?")

;; set keybinds
(global-unset-key (kbd "M-2"))
(global-unset-key (kbd "M-3"))
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
(bind-keys
 ("M-2" . er/expand-region)
 ("M-3" . mark-next-like-this)

 ("C-c e" . eshell)
 ("C-c h" . helm-mini)
 ("C-c x" . helm-M-x)
 ("C-c g" . magit-status)
 ("C-c j" . org-journal-new-entry)
 ("C-c s" . helm-swoop)

 ("C-x C-b" . ibuffer)
 ("C-x C-f" . helm-find-files)
 )

(put 'narrow-to-region 'disabled nil)

(provide 'emacs)

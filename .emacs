;;; .emacs -- Emacs config
;;; Commentary:
;;; My Emacs config
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'bind-key)
(require 'diminish)
(setq use-package-always-ensure t)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package evil
  :init (evil-mode t))
(use-package evil-surround
  :diminish t)
(use-package evil-org)
(use-package evil-magit
  :init (evil-magit-init))
(use-package evil-commentary)

(use-package expand-region
  :bind (("M-2". er/expand-region)))
(use-package mark-multiple
  :bind (("M-3". mark-next-like-this)))

(use-package key-chord
  :init (key-chord-mode t))
(use-package yasnippet
  :init (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package company)
(use-package company-c-headers)
(use-package company-jedi)
(use-package company-web)

(use-package helm
  :init (helm-mode t)
  :diminish t
  :config
  (bind-keys
   :map helm-map
   ("C-j". helm-next-line)
   ("C-k". helm-previous-line)
   ("C-l". helm-execute-persistent-action)
   :map helm-find-files-map
   ("C-h". helm-find-files-up-one-level)))
(use-package helm-projectile)
(use-package helm-company)
(use-package helm-swoop)
(use-package ido-vertical-mode
  :init (ido-vertical-mode t))

(use-package projectile
  :init (projectile-global-mode t))
(use-package magit)

(use-package flycheck)
(use-package cmake-font-lock)
(use-package cmake-mode)
(use-package cmake-ide)
(use-package web-mode
  :mode ("\\.html\\'" "\\.php\\'"))
(use-package scss-mode)
(use-package cider)

(use-package tango-plus-theme)
(use-package spacegray-theme)
(enable-theme 'spacegray)
(use-package powerline)
(use-package powerline-evil
  :init (powerline-evil-vim-color-theme))

(use-package org)
(use-package org-bullets)
(use-package org-journal)

;; calendar stuff
(use-package calfw
  :init (require 'calfw-ical))

;; misc stuff
(use-package haste)
(use-package evil-mu4e)
(use-package helm-mu)
(use-package mingus)
(use-package highlight-parentheses)

(setq search-whitespace-regexp ".*?")

(diminish 'helm-mode)
(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)
(diminish 'company-mode)

(defun lastbuf () "Switch to last buffer instantly."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))
(defun save-all () "Save all files."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

 ;; autoinsert C/C++ header
(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C / C++ header")
  '(nil
    "// " (file-name-nondirectory buffer-file-name) "\n"
    "//\n"
    "// last-edit-by: <> \n"
    "//\n"
    "// Description:\n"
    "//\n"
    (make-string 70 ?/) "\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
	   (nopath (file-name-nondirectory noext))
	   (ident (concat (upcase nopath) "_H")))
      (concat "#ifndef " ident "\n"
	      "#define " ident  " 1\n\n\n"
	      "\n\n#endif // " ident "\n"))
    (make-string 70 ?/) "\n"
    "// $Log:$\n"
    "//\n"
    ))

;; auto insert C/C++
(define-auto-insert
  (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "My C++ implementation")
  '(nil
    "// " (file-name-nondirectory buffer-file-name) "\n"
    "//\n"
    "// last-edit-by: <> \n"
    "// \n"
    "// Description:\n"
    "//\n"
    (make-string 70 ?/) "\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
	   (nopath (file-name-nondirectory noext))
	   (ident (concat nopath ".h")))
      (if (file-exists-p ident)
	  (concat "#include \"" ident "\"\n")))
    (make-string 70 ?/) "\n"
    "// $Log:$\n"
    "//\n"
    ))

(defun inside-class-enum-p (pos)
  "Check if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t\n]+class[ \t\n]+"))))
      ;; (looking-back "enum[ \t\n]+class[ \t\n]+[^}]+"))))

(defun align-enum-class (langelem)
  "Align enum class content of LANGELEM."
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  "Align closing brace line of enum class LANGELEM."
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(bind-keys
 ("C-c e" . eshell)
 ("C-c h" . helm-mini)
 ("C-c x" . helm-M-x)
 ("C-c g" . magit-status)
 ("C-c j" . org-journal-new-entry)
 ("C-c s" . helm-swoop)
 ;; ("C-c c" . open-ical-calendar)
 ("C-c d" . mingus)

 ("C-x C-b" . lastbuf)
 ("C-x C-f" . helm-find-files)
 )


;; (defvar user-mailconf nil)
;; (setq user-mailconf
;;       '(:maildir "~/.mail"
;; 	:full-name "Andreas Larsen"
;; 	:accounts
;; 	'((:name "gmail" :maildir "/gmail/")
;; 	  (:name "northcode" :maildir "/northcode/")
;; 	  )))

;; (message (plist-get user-mailconf :maildir))

;; (defun load-mu4e-conf ()
;;   (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
;;   (require 'mu4e)
;;   (require 'smtpmail)

;;   (setq mu4e-maildir (plist-get user-mailconf :maildir))
;;   )

;; (if (file-exists-p "/usr/share/emacs/site-lisp/mu4e")
;;     (progn
;;       (message "loading mu4e config")
;;       (load-mu4e-conf)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" default)))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring scrolltobottom services stamp track)))
 '(erc-notify-list (quote ("okknor")))
 '(erc-notify-mode t)
 '(evil-want-C-u-scroll t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/.emacs")
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 108 :width normal :family "Inconsolata"))))
 '(mode-line ((t (:family "Liberation Mono"))))
 '(widget-field ((t (:box (:line-width 1 :color "#ffffff"))))))

(provide 'emacs)
;;; .emacs ends here

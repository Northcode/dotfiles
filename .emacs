;;; .emacs -- Emacs config
;;; Commentary:
;;; My Emacs config
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
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
(use-package evil-commentary
  :init (evil-commentary-mode)
  :diminish t)

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
   ("C-l". helm-execute-persistent-action)
   ("C-b". helm-find-files-up-one-level)))

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
(use-package calfw :init (progn
			   (require 'calfw)
			   (require 'calfw-ical))
			   )
(use-package calfw-gcal :init (require 'calfw-gcal))

;; misc stuff
(use-package haste)
(use-package evil-mu4e)
(use-package helm-mu)
(use-package mingus)
(use-package highlight-parentheses)
(use-package xkcd)

(setq search-whitespace-regexp ".*?"
      backup-by-copying t
      backup-directory-alist
      '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      )

(diminish 'helm-mode)
(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)
(diminish 'company-mode)
(diminish 'evil-commentary-mode)

(defun lastbuf () "Switch to last buffer instantly."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))
(defun save-all () "Save all files."
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)
;; from http://ergoemacs.org/emacs/elisp_read_file_content.html thanks!
(defun get-string-from-file (filePath) "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
(defun open-ical-calendar () "Open calendar stored in .calendar file"
  (interactive)
  (cfw:open-ical-calendar (get-string-from-file "~/.calendar")))
(defun revbuf () "Revert buffer without asking."
  (interactive)
  (revert-buffer t t))

(define-key evil-normal-state-map (kbd "gr") 'revbuf)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(bind-keys
 ("C-c e" . eshell)
 ("C-c h" . helm-mini)
 ("C-c x" . helm-M-x)
 ("C-c g" . magit-status)
 ("C-c j" . org-journal-new-entry)
 ("C-c s" . helm-swoop)
 ("C-c c" . open-ical-calendar)
 ("C-c d" . mingus)
 ("C-c m" . mu4e)

 ("C-x C-b" . lastbuf)
 ("C-x C-o" . other-window)
 ("C-x C-f" . helm-find-files)
 )


;; MU4E config
(defvar user-mailconf nil)
(setq user-mailconf
      '(:maildir "~/.mail/"
	:name "Andreas Larsen"
	:signature "----\nAndreas Larsen - northcode.no"
	:accounts
	((:name "gmail"
	   :email "northcode.no@gmail.com"
	   :smtp "smtp.gmail.com"
	   :inbox "Inbox"
	   :sent "[Gmail]/Sent Mail"
	   :archive "[Gmail]/All Mail"
	   :draft "drafts"
	   :trash "[Gmail]/Bin")
	  (:name "northcode"
	   :email "andreas@northcode.no"
	   :smtp "northcode.no"
	   :inbox "Inbox"
	   :sent "Sent"
	   :archive "Archive"
	   :draft "drafts"
	   :trash "Trash")
	  )
	))

;; ------------------- MOST STUFF BELOW THIS LINE IS FUNCTIONS AND CUSTOM GENERATED STUFF --------------------

(define-generic-mode
    'authinfo-mode
  '("#")
  '("machine" "login" "port" "password")
  '()
  '("\\.authinfo$" "\\.authinfo.gpg")
  nil
  "Mode for authinfo files"
  )

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

;; add hooks to mingus for emacs modes because apparently the mode list doesn't work for it....
(add-hook 'mingus-playlist-hooks (lambda () (evil-emacs-state t)))
(add-hook 'mingus-browse-hooks (lambda () (evil-emacs-state t))) ;; also mingus-browse does not seem to load hooks as of 2016-05-04... great

(defun load-mailconf (conf)
  (setq
   mu4e-maildir (plist-get conf :maildir)
   user-full-name (plist-get conf :name)
   mu4e-compose-signature (plist-get conf :signature)
   mu4e-get-mail-command "mbsync gmail northcode"
   mu4e-context-policy 'pick-first
   mu4e-contexts nil
   mu4e-sent-messages-behavior 'delete
   message-send-mail-function 'smtpmail-send-it
)

  (dolist (account (plist-get conf :accounts))
    (add-to-list 'mu4e-user-mail-address-list (plist-get account :emal))
    (add-to-list 'mu4e-contexts (make-mu4e-context
	:name (eval (plist-get account :name))
	:enter-func (lambda () (mu4e-message "Swiched contexts"))
	:match-func (lambda (msg)
		      (when msg
			(mu4e-message-contact-field-matches-me msg '(:to))))
	:vars `(
		(mu4e-inbox-folder      . , (concat "/" (plist-get account :name) "/" (plist-get account :inbox)))
		(mu4e-sent-folder       . , (concat "/" (plist-get account :name) "/" (plist-get account :sent)))
		(mu4e-refile-folder     . , (concat "/" (plist-get account :name) "/" (plist-get account :archive)))
		(mu4e-drafts-folder     . , (concat "/" (plist-get account :name) "/" (plist-get account :draft)))
		(mu4e-trash-folder      . , (concat "/" (plist-get account :name) "/" (plist-get account :trash)))
		(user-mail-address      . , (plist-get account :email))
		(mu4e-maildir-shortcuts    . (
					   (,(concat "/" (plist-get account :name) "/" (plist-get account :inbox)) . ?i)
					   (,(concat "/" (plist-get account :name) "/" (plist-get account :sent)) . ?s)
					   (,(concat "/" (plist-get account :name) "/" (plist-get account :draft)) . ?s)
					   (,(concat "/" (plist-get account :name) "/" (plist-get account :archive)) . ?a))
					)
		(smtpmail-smtp-server   . , (plist-get account :smtp))
		(smtpmail-smtp-service  . 587)
		(starttls-use-gnutls    . t)
		)
	))
    )
  )

(defun load-mu4e-conf ()
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (require 'mu4e)
  (require 'smtpmail)
  (require 'evil-mu4e)

  (load-mailconf user-mailconf)
  )

(if (file-exists-p "/usr/share/emacs/site-lisp/mu4e")
    (progn
      (message "loading mu4e config")
      (load-mu4e-conf)))

(put 'narrow-to-region 'disabled nil)

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
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring scrolltobottom stamp track)))
 '(erc-nick "northcode")
 '(erc-notify-list (quote ("okknor")))
 '(erc-notify-mode t)
 '(erc-user-full-name "Andreas Larsen")
 '(evil-want-C-u-scroll t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/")
 '(menu-bar-mode nil)
 '(mingus-use-mouse-p nil)
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

;;; .emacs -- Emacs config
;;; Commentary:
;;; My Emacs config
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
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
  :config
  (progn
    (bind-keys
     :map helm-map
     ("C-j". helm-next-line)
     ("C-k". helm-previous-line)
     ("C-l". helm-execute-persistent-action))
    
    (bind-keys
     :map helm-find-files-map
     ("C-l". helm-execute-persistent-action)
     ("C-b". helm-find-files-up-one-level))))

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
(use-package calfw-gcal)

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
      '(("." . "~/.emacs.saves"))
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
 ("C-c a" . org-agenda)
 ("C-c h" . helm-mini)
 ("C-c x" . helm-M-x)
 ("C-c g" . magit-status)
 ("C-c j" . org-journal-new-entry)


 ("C-c s" . helm-swoop)
 ("C-c c" . open-ical-calendar)
 ("C-c d" . mingus)
 ("C-c m" . mu4e)

 ("C-<tab>" . hippie-expand)

 ("C-x C-b" . lastbuf)
 ("C-x C-o" . other-window)
 ("C-x C-f" . helm-find-files)
 )

(defhydra space-key-popup
  (evil-normal-state-map "SPC")
  ("f" helm-find-files "Find files")
  ("b" lastbuf "switch to last buffer")
  ("B" switch-to-buffer "switch to buffer"))

(define-key evil-normal-state-map (kbd "SPC") 'space-key-popup)

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
;; (add-hook 'mingus-playlist-hooks (lambda () (evil-emacs-state t)))
;; (add-hook 'mingus-browse-hook (lambda () (evil-emacs-state t)))
(setq mingus-playlist-hooks '())
(setq mingus-browse-hook '())

(add-hook 'prog-mode-hook 'company-mode)

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

(evil-set-initial-state 'mingus-help-mode 'emacs)
(evil-set-initial-state 'mingus-browse-mode 'emacs)
(evil-set-initial-state 'mingus-playlist-mode 'emacs)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-enabled-themes (quote (tango-plus)))
 '(custom-safe-themes
   (quote
    ("0f98f9c2f1241c3b6227af48dc96e708ec023dd68363edb5d36dc7beaad64c23" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" default)))
 '(default-frame-alist (quote ((vertical-scroll-bars))))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring scrolltobottom stamp track)))
 '(erc-nick "northcode")
 '(erc-notify-list (quote ("okknor")))
 '(erc-notify-mode t)
 '(erc-user-full-name "Andreas Larsen")
 '(evil-commentary-mode t)
 '(evil-emacs-state-modes
   (quote
    (archive-mode bbdb-mode biblio-selection-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode mh-folder-mode monky-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode pdf-outline-buffer-mode pdf-view-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode mingus-playlist-mode mingus-browse-mode)))
 '(evil-mode t)
 '(evil-want-C-u-scroll t)
 '(fci-rule-color "#343d46")
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/")
 '(menu-bar-mode nil)
 '(mingus-use-mouse-p nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#bf616a")
     (40 . "#DCA432")
     (60 . "#ebcb8b")
     (80 . "#B4EB89")
     (100 . "#89EBCA")
     (120 . "#89AAEB")
     (140 . "#C189EB")
     (160 . "#bf616a")
     (180 . "#DCA432")
     (200 . "#ebcb8b")
     (220 . "#B4EB89")
     (240 . "#89EBCA")
     (260 . "#89AAEB")
     (280 . "#C189EB")
     (300 . "#bf616a")
     (320 . "#DCA432")
     (340 . "#ebcb8b")
     (360 . "#B4EB89"))))
 '(org-capture-templates
   (quote
    (("a" "Todo item" entry
      (file "~/Journal/todo.org")
      "* TODO "))))
 '(org-journal-dir "~/Journal/")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-journal-time-format "<%Y-%m-%d %R>")
 '(org-journal-time-prefix "** Journal Entry ")
 '(org-agenda-files (quote ("~/Journal")))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 108 :width normal :family "Inconsolata"))))
 '(mode-line ((t (:family "Liberation Mono"))))
 '(mu4e-unread-face ((t (:inherit font-lock-keyword-face :foreground "#11aaff" :weight bold))))
 '(widget-field ((t (:box (:line-width 1 :color "#ffffff"))))))

(provide 'emacs)
;;; .emacs ends here

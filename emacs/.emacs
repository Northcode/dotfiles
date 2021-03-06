;; -*- eval: (outline-minor-mode) -*-
;; .emacs -- Emacs config with straight.el

;;; Bootstrapping

(require 'cl)

;;;; Get the actual home directory on windows
(if (eq system-type 'windows-nt)
    (setq win-home (file-truename "~/../../")))

;;;; bootstrap straight.el

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; fetch use-package

(straight-use-package 'use-package)
(use-package diminish :straight t)

(add-to-list 'load-path (file-truename "~/.emacs.d/elisp/"))

;;;; Startup optimization

;;;;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook #'ambrevar/reset-gc-cons-threshold)

;;;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist default-file-name-handler-alist))
(add-hook 'after-init-hook #'ambrevar/reset-file-name-handler-alist)


;;; Base config

;;;; Mode line

(setq-default mode-line-format
	      '("    "
		(:eval (case evil-state
			 ('normal "N")
			 ('insert "I")
			 ('visual "V")
			 ('emacs  "E")))
		" %04l %n "
		"    "
		(:propertize (:eval (if buffer-read-only " RO: "))
			     face font-lock-warning-face)
		(:propertize "%b"
			     face font-lock-keyword-face)
		" "
		(:eval (if (buffer-modified-p) "(!!)"))
		" "
		;; (:propertize
		;;  (:eval (when (magit-get-current-branch)
		;; 	  (concat " [" (magit-get-current-branch) "]")))
		;;  face font-lock-string-face)
		" :: "
		(:propertize "%m"
			     face font-lock-constant-face)
		" %e "
		(:eval (format-time-string "%H:%M" (current-time)))
		" %-"))


;;;; Evil

(setq evil-want-keybinding nil)
(use-package evil :straight t
  :config
  (evil-mode t)
  :config
  (setq evil-move-cursor-back nil))

(use-package evil-surround :straight t
  :config (global-evil-surround-mode t))

(use-package evil-magit
  :straight t
  :config (evil-magit-init))

(use-package evil-commentary
  :straight t
  :config (evil-commentary-mode t))

;; (use-package evil-collection
;;   :straight t
;;   :config
;;   (setq evil-want-integration nil)
;;   (evil-collection-init))


;;;; Helm

(use-package helm
  :straight t
  :config (helm-mode t)
  :bind
  (("C-c x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   :map helm-map
   ("M-h" . helm-find-files-up-one-level)
   ("M-j". helm-next-line)
   ("M-k". helm-previous-line)
   ("M-l". helm-execute-persistent-action)
   :map helm-find-files-map
   ("M-l". helm-execute-persistent-action)))

(use-package helm-projectile 
  :straight t
  :bind
  (("C-c p p" . helm-projectile-switch-project)
   ("C-c p d" . projectile-dired)
   ("C-c p h" . helm-projectile)))

(use-package popup
  :straight t
  :bind
  (:map popup-menu-keymap
	("<escape>" . keyboard-quit)
	("M-j" . popup-next)
	("M-k" . popup-previous)))

;;;; Org mode

(use-package org
  :straight t
  :config
  (if (eq system-type 'windows-nt)
      (setq org-directory (concat win-home "Nextcloud/org/"))
    (setq org-directory (file-truename "~/Nextcloud/org/")))
  (setq org-agenda-files (list org-directory (concat org-directory "habits.org.gpg")) 
	org-clock-idle-time 15)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)))


;; (use-package org-trello
;;   :straight t
;;   :bind
;;   (:map org-trello-mode-map
;; 	("C-c o s" . org-trello-sync-buffer))
;;   :config
;;   (add-to-list 'org-agenda-files "~/org/bachelor/scrum.org"))


;;;; Set some defaults

;; Prevents issue where you have to press backspace twice when
;; trying to remove the first character that fails a search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

(setq search-whitespace-regexp ".*?"
      backup-by-copying t
      backup-directory-alist
      '(("." . "~/.emacs.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      evil-want-C-u-scroll t
      save-interprogram-paste-before-kill t
      )

(setq mouse-wheel-scroll-amount (if (string-equal "andreas-d0" (system-name)) '(5) '(1))
      mouse-wheel-progressive-speed nil)

;; only setup gnutls on unix systems
(if (not (memq system-type '(windows-nt ms-dos)))
    (progn
      (require 'tls)
      (setq tls-program '("openssl s_client -connect %h:%p -ign_eof
				       -CAfile /etc/ca-certificates/extracted/tls-ca-bundle.pem
				       -cert /home/andreas/user.pem"
			  "gnutls-cli --priority secure256 
				 --x509cafile /home/ootput/.private/certs/CAs.pem 
				 --x509certfile /home/ootput/.private/certs/nick.pem -p %p %h" 
			  "gnutls-cli --priority secure256 -p %p %h"))))

(put 'narrow-to-region 'disabled nil)

;;;; Epkg

(use-package epkg :straight t)

;;; Util functions

;;;; Interactive wrappers around emacs functions I use often

(defun lastbuf () "Switch to last buffer instantly."
       (interactive)
       (switch-to-buffer (other-buffer (current-buffer))))

(defun save-all () "Save all files."
       (interactive)
       (save-some-buffers t))

(defun revbuf () "Revert buffer without asking."
       (interactive)
       (revert-buffer t t))

(evil-define-key 'normal global-map (kbd "g r") 'revbuf)

(defun isearch-by-selection ()
  (interactive)
  (let ((selection (buffer-substring-no-properties (point) (mark))))
    (deactivate-mark)
    (isearch-mode t)
    (isearch-yank-string selection)))

(defun custom-isearch ()
  (interactive)
  (if mark-active
      (isearch-by-selection)
    (isearch-mode t)))

(global-set-key (kbd "C-s") 'custom-isearch)

;;;; Elisp scripting utilities

;; from http://ergoemacs.org/emacs/elisp_read_file_content.html thanks!
(defun get-string-from-file (filePath) "Return FILEPATH's file content."
       (with-temp-buffer
	 (insert-file-contents filePath)
	 (buffer-string)))

(defun get-lines-matching-from-file (f re)
  (-filter (lambda (s) (string-match-p re s))
	   (split-string (get-string-from-file f)
			 "\n")))

(defun regexp-replace-list (re rep seq)
  (mapcar (lambda (str) (replace-regexp-in-string re rep str nil)) seq))

;;;; Ediff stuff

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
		   (concat
		    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
		    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun add-d-to-ediff-mode-map () 
  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))

;;;; Custom editing functions

(defun calc-eval-region (beg end)
  "Calculate the region and display the result in the echo area.
With prefix ARG non-nil, insert the result at the end of region."
  (interactive "r")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (message "%s = %s" expr result)
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (insert result))))

;;;; Sudo stuff

(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-save-before-save-hook ()
  (unless (file-writable-p (buffer-file-name))
    (when (y-or-n-p "No write access, try with sudo?")
      (sudo-save))))

;;;; Mu4e config generation

(defun load-mailconf (conf)
  (setq
   mu4e-maildir (plist-get conf :maildir)
   user-full-name (plist-get conf :name)
   mu4e-compose-signature (plist-get conf :signature)
   mu4e-get-mail-command "mbsync -a"
   mu4e-context-policy 'pick-first
   mu4e-contexts nil
   mu4e-sent-messages-behavior 'delete
   message-send-mail-function 'smtpmail-send-it
   mu4e-change-filenames-when-moving t
   )

  (dolist (account (reverse (plist-get conf :accounts)))
    (add-to-list 'mu4e-user-mail-address-list (plist-get account :email))
    (add-to-list 'mu4e-contexts (make-mu4e-context
				 :name (eval (plist-get account :name))
				 :enter-func (lambda () (mu4e-message "Swiched contexts"))
				 :match-func (plist-get account :match-func)
				 ;; (lambda (msg)
				 ;; 	       (when msg
				 ;; 		 (message (concat "checking mail: " ,@(plist-get account :email)))
				 ;; 		 (mu4e-message-contact-field-matches-me msg '(:to ,@(plist-get account :email)))
				 ;; 		 ))
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
				 ))))

(defun load-mu4e-conf ()
  (use-package mu4e
    :init
    (require 'mu4e)
    :config
    (require 'smtpmail)
    (setq mu4e-html2text-command 'mu4e-shr2text
	  shr-color-visible-luminance-min 60
	  shr-color-visible-distance-min 5
	  shr-use-colors nil)

    (require 'mu4e-contrib)
    (setq mu4e-html2text-command 'mu4e-shr2text
	  shr-color-visible-luminance-min 60
	  shr-color-visible-distance-min 5
	  shr-use-colors nil)

    (bind-keys
     ("C-c m" . mu4e))

    (load-mailconf user-mailconf)

    (setq mu4e-bookmarks
	  '(("flag:unread AND NOT flag:trashed AND NOT maildir:/northcode/Junk" "Unread messages" 117)
	    ("date:today..now" "Today's messages" 116)
	    ("date:7d..now" "Last 7 days" 119)
	    ("mime:image/*" "Messages with images" 112)))

    )
  
  (use-package evil-mu4e :straight t)
  (use-package mu4e-maildirs-extension :straight t
    :config
    (mu4e-maildirs-extension)
    )
  ;; (use-package mu4e-alert :straight t
  ;;   :config
  ;;   (mu4e-alert-set-default-style 'libnotify)
  ;;   (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  ;;   (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  ;;   (mu4e-alert-enable-notifications)
  ;;   (mu4e-alert-enable-mode-line-display)
  ;;   )

  (use-package helm-mu :straight t)
  )

;;;; Erc tls auth source and formatting

(defun erc-tls-auth-source (&rest args)
  "Load erc-tls authentication info from .authinfo database."
  (print args)
  (let ((server (plist-get args :server))
	(port (number-to-string (plist-get args :port)))
	(nick (plist-get args :nick)))
    (let ((secret (nth 0 (auth-source-search :max 1 :host server :port port))))
      (if secret
	  (let ((pass (plist-get secret :secret)))
	    (let ((passwd (if (functionp pass) (funcall pass) pass)))
	      (message "Got password from authinfo!")
	      (erc-tls :server server :port port :nick nick :password (concat nick ":" passwd))))
	(progn
	  (message "Getting password from authinfo failed")
	  (apply 'erc-tls args))))))


(defun erc-format-privmessage (nick msg privp msgp)
  "Format a PRIVMSG in an insertable fashion."
  (let* ((mark-s (if msgp (if privp "*" "[") "-"))
	 (mark-e (if msgp (if privp "*" "]") "-"))
	 (str    (format "%s %s" (s-pad-left 15 " " (concat mark-s nick mark-e)) msg))
	 (nick-face (if privp 'erc-nick-msg-face 'erc-nick-default-face))
	 (msg-face (if privp 'erc-direct-msg-face 'erc-default-face)))
    ;; add text properties to text before the nick, the nick and after the nick
    (erc-put-text-property 0 (length mark-s) 'face msg-face str)
    (erc-put-text-property (length mark-s) (+ (length mark-s) (length nick))
			   'face nick-face str)
    (erc-put-text-property (+ (length mark-s) (length nick)) (length str)
			   'face msg-face str)
    str))

;;; Project management

(use-package projectile
  :straight t
  :init
  (projectile-mode t)
  :config
  ;; (progn
  ;;   (setq (alist-get 'gradlew  projectile-project-types)
  ;; 	  (plist-put (alist-get 'gradlew projectile-project-types)
  ;; 		     'run-command
  ;; 		     "./gradlew run")))
  (defun save-and-run () (interactive) (save-all) (projectile-run-project nil))
  (defun save-and-compile () (interactive) (save-all) (projectile-compile-project nil))
  (defun toggle-compilation-read-command () (interactive)
	 (if compilation-read-command
	     (setq compilation-read-command nil)
	   (setq compilation-read-command t)))
  :bind
  (:map projectile-mode-map
	("<f5>" . save-and-run)
	("<f6>" . save-and-compile)
	("<f7>" . toggle-compilation-read-command)
	("C-c p c" . save-and-compile)
	("C-c p r" . save-and-run)))


(defun projectile-get-first-file-matching (re)
  "Return the first file in a projectile project matching RE"
  (first (-filter (lambda (str) (string-match-p re str)) (directory-files-recursively (projectile-project-root) ".*"))))

(use-package magit
  :straight t
  :bind
  (("C-c g" . magit-status)))

;;; Editing tools

;;;; Mangar's stuff

(use-package expand-region
    :straight t
    :bind (("M-2" . er/expand-region)))

(use-package mark-multiple
    :straight t
    :bind (("M-3" . mark-next-like-this)
	   ("M-#" . mark-previous-like-this)))

;;;; Auto complete
;; (use-package auto-complete
;;   :straight t
;;   :config
;;   (require 'auto-complete)
;;   (require 'auto-complete-config)
;;   (ac-config-default)
;;   (add-hook 'prog-mode-hook 'auto-complete-mode))
;; (use-package ac-c-headers
;;   :straight t
;;   :config
;;   (require 'ac-c-headers)
;;   (let ((acc-hook (lambda () (add-to-list 'ac-sources 'ac-source-c-headers))))
;;     (add-hook 'c++-mode-hook acc-hook)
;;     (add-hook 'c-mode-hook acc-hook)))

(use-package company
  :straight t
  :config
  (add-hook 'prog-mode-hook 'company-mode))

;;;; Outlining

(use-package outline
  :straight t
  :config
  (evil-define-key 'normal outline-minor-mode-map (kbd "C-i") 'outline-cycle)
  (evil-define-key 'normal outline-minor-mode-map (kbd "<S-iso-leftab>") 'outshine-cycle-buffer
    (kbd "S-<tab>") 'outshine-cycle-buffer 
    (kbd "<backtab>") 'outshine-cycle-buffer)
  (add-hook 'outline-minor-mode-hook (lambda () (evil-normalize-keymaps))))


(use-package outshine
  :straight t
  :init
  (require 'outshine)
  (add-hook 'outline-minor-mode-hook 'outshine-mode))

;;;; Flycheck
(use-package flycheck
  :straight t)

;;;; Parinfer

(use-package parinfer
  :straight t)

;;;; Paredit

(use-package paredit
  :straight t
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package evil-paredit
  :straight t
  :config
  (add-hook 'paredit-mode-hook 'evil-paredit-mode)
  (evil-define-key 'normal paredit-mode-map (kbd "g s l") 'paredit-forward-slurp-sexp)
  (evil-define-key 'normal paredit-mode-map (kbd "g s h") 'paredit-forward-barf-sexp)
  (evil-define-key 'normal paredit-mode-map (kbd "g S l") 'paredit-backward-barf-sexp)
  (evil-define-key 'normal paredit-mode-map (kbd "g S h") 'paredit-backward-slurp-sexp))

;;;; LaTeX

;; (use-package auctex
;;   :straight t
;;   :config
;;   (setq TeX-auto-save t
;; 	TeX-parse-self t
;; 	LaTeX-indent-level 4)
;;   (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;   (add-hook  'LaTeX-mode-hook 'flyspell-mode)
;;   (add-hook  'LaTeX-mode-hook 'LaTeX-math-mode))

(straight-use-package 'auctex)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook  'LaTeX-mode-hook 'flyspell-mode)
(add-hook  'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook  'LaTeX-mode-hook 'yas-minor-mode)




;; (use-package latex-preview-pane
;;   :straight t
;;   :config
;;   (latex-preview-pane-enable))

;;;;; Functions for latex stuff

(defun latex-get-bibitems-from-file (f)
  "Get a list of all the bibitems in a file"
  (let ((re "\\\\bibitem{\\([a-zA-Z0-9_()\-]+\\)}")
	(rep "\\1"))
    (regexp-replace-list re rep (get-lines-matching-from-file f re))))

(defun latex-get-glossary-list-from-file (f)
  "Gets glossaries or acronyms from file F"
  (let ((re "\\\\\\(?:newglossaryentry\\|newacronym\\[see={\\[Glossary:]{[a-zA-Z0-9-_()]+}}]\\){\\([a-zA-Z0-9-_()]+\\)}.*")
	(rep "\\1"))
    (regexp-replace-list re rep (get-lines-matching-from-file f re))))

(defun latex-project-get-gls-file ()
  "Find a bib*.tex file in a latex project, must be used inside a projectile project, returns nil if no file is found"
  (projectile-get-first-file-matching ".*[^#]glossary\\.tex"))

(defun latex-project-get-bib-file ()
  "Find a bib*.tex file in a latex project, must be used inside a projectile project, returns nil if no file is found"
  (projectile-get-first-file-matching ".*bib.*\\.tex"))

;;;; Searching

(use-package ag
  :straight t
  :bind
  (("C-c p s" . projectile-ag)))

;;;; IX

(use-package ix
  :straight t)

;;;; Yas

(use-package yasnippet
  :straight t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-dropdown-prompt yas-completing-prompt yas-maybe-ido-prompt yas-no-prompt))

;;; Programming languages

;;;; Language Server Protocol

(use-package lsp-mode
  :straight t
  ;; :config
  ;; (setq lsp-rust-server "rust-analyzer")
  :bind
  (:map prog-mode-map
	("C-c f" . helm-imenu)
	("C-c C-c" . lsp-execute-code-action)))


(use-package lsp-ui
  :straight t)

(use-package company-lsp
  :straight t
  :config
  (add-to-list 'company-backends 'company-lsp))

;;;; Clojure

(use-package clojure-mode :straight t)
(use-package inf-clojure :straight t)
(use-package cider :straight t
  :config (add-to-list 'evil-emacs-state-modes 'cider-stacktrace-mode))


;;;; Rust

(use-package rust-mode
  :straight t)

(use-package flycheck-rust
  :straight t
  :config
  (add-hook 'rust-mode-hook 'flycheck-mode))

(use-package cargo :straight t)

;; (use-package lsp-rust
;;   :straight t
;;   :after lsp-mode
;;   :init
;;   (require 'lsp-rust)
;;   (add-hook 'rust-mode-hook #'lsp-rust-enable)
;;   :bind
;;   (:map rust-mode-map
;; 	("C-c C-c" . lsp-execute-code-action)))

   
;;;; Web
(use-package web-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)))

(use-package zencoding-mode
  :straight t
  :bind
  (:map zencoding-mode-keymap
	("C-c C-c" . zencoding-expand-line)))

;;;; C#
(use-package csharp-mode
  :straight t)

(use-package omnisharp
  :straight t
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  :bind
  (:map csharp-mode-map
	("C-c r" . omnisharp-run-code-action-refactoring)
	("<f8>" . omnisharp-start-omnisharp-server)))

;;;; Java

(use-package meghanada
  :straight t)

(defun my-java-mode-hook ()
  (meghanada-mode t)
  (flycheck-mode t))

(add-hook 'java-mode-hook 'my-java-mode-hook)

;;;; Groovy

(use-package groovy-mode :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.build.gradle" . groovy-mode)))

;;;; Js
(use-package indium
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
;;;; C++

(use-package cquery
  :straight t
  :config
  (add-to-list 'evil-emacs-state-modes 'cquery-tree-mode)
  :bind
  (:map c++-mode-map
	("C-c SPC c" . cquery-call-hierarchy)
	("C-c SPC i" . cquery-inheritance-hierarchy)))

;;;; Systemd units

(use-package systemd :straight t)

;;;; Cobol

(use-package cobol-mode :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.cob\\'" . cobol-mode)))

;;;; Yaml mode
(use-package yaml-mode
  :straight t)

;;;; Fish-shell

(use-package fish-mode
  :straight t)

(use-package fish-completion
  :straight t
  :init
  (when (and (executable-find "fish")
	     (require 'fish-completion nil t))
    (global-fish-completion-mode)))


;;; Organization tools
;;;; Org mode
(use-package org-bullets :straight t)

(use-package org-caldav 
  :straight t
  :config
  (setq org-caldav-url "https://cloud.northcode.no/remote.php/dav/calendars/andreas"
	org-caldav-calendar-id "personal"
	org-caldav-inbox "~/cal.org"
	org-caldav-files '("~/cal.org")))

(use-package evil-org
  :straight t
  :after org
  :config
  (defun nc-org-todo-cycle () (interactive) (org-todo 'right))
  (evil-define-key 'normal evil-org-mode-map (kbd "gt") 'nc-org-todo-cycle)
  (evil-define-key 'normal evil-org-mode-map (kbd "gT") 'org-todo)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(navigation insert textobjects additional calendar)))))

;; (load-file "~/.emacs.d/elisp/ox-s5.el")


;;;; Drawing stuff

(defun open-draw-temp-file ()
  (let ((filename (concat (make-temp-file "img") ".png")))
    (call-process-shell-command (concat "mypaint " filename) nil nil)
    (if (file-exists-p filename)
	filename
      nil)))

(defun insert-new-note-image ()
  (interactive)
  (if-let ((filename (open-draw-temp-file)))
      (progn
	(let ((to-file (concat (file-name-directory buffer-file-name) (file-name-nondirectory buffer-file-name) "-" (file-name-nondirectory filename))))
	  (copy-file filename to-file)
	  (save-excursion
	    (insert (concat "[[" to-file "]]")))))
    (message "failed to get image")))

(bind-keys
 :map org-mode-map
 ("C-c i i" . insert-new-note-image))

;;;; Ledger
(use-package hledger-mode 
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode)))

;;; UI/UX
;;;; Window management
(use-package ace-window
  :straight t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  aw-scope 'frame
  :bind ("C-x o" . ace-window))

;;;; Disable default fluff
(progn ;; disable gui fluff
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Dired sidebar

(use-package dired-sidebar
  :straight t
  :bind
  ("<f9>" . dired-sidebar-toggle-sidebar))

;;;; Theme

;; (use-package spacegray-theme
;;   :straight t)

;; (use-package nord-theme
;;   :straight t)

;; Load nord theme once a frame is created, not before as it makes it break, then remove the hook as it is no longer needed until next emacs startup
;; (defun nord-theme-hook (frame)
;;   (with-selected-frame frame (load-theme 'nord t))
;;   (remove-hook 'after-make-frame-functions 'nord-theme-hook))

;; (add-hook 'after-make-frame-functions 'nord-theme-hook)

;; just using the one from melpa
(use-package northcode-theme
  :straight t)

;; for developing northcode-theme
;; (add-to-list 'custom-theme-load-path (file-truename "~/projects/northcode-theme.el/northcode-theme.el"))
;; (load-file (file-truename "~/projects/northcode-theme.el/northcode-theme.el"))


;;; Management Tools
;;;; Eshell and term
(use-package eshell
  :config
  (setq eshell-aliases-file "~/.eshell.alias")
  :bind
  (("C-c e" . eshell)))

(use-package term
  :bind
  (("C-c E" . ansi-term)))

(if (file-exists-p "~/build/emacs-libvterm/vterm.el")
    (progn
      (add-to-list 'load-path "~/build/emacs-libvterm/")
      (require 'vterm)))

;;;; Dired

(use-package dired
  :config
  (evil-define-key 'normal dired-mode-map "gr" 'revbuf))
;;;; Ediff
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)
;;;; REST Client

(use-package restclient
  :straight t)

;;;; Epkg

;; (use-package epkg
;;   :straight t)



;;;; Docker

(use-package dockerfile-mode :straight t)

(use-package docker :straight t)

;;; General Emacs behaviour
;;;; Hooks to stuff
(add-hook 'focus-out-hook 'save-all)
(add-hook 'before-save-hook 'sudo-save-before-save-hook)

;;;; Keybinds

(defun quick-find-file ()
  (interactive)
  (find-file (case last-command-event
	       (?c user-init-file)
	       (?n "~/Documents/org/notes.org")
	       (?t "~/Documents/org/todo.org")
	       (t (error "Key %c not bound to a file yet." last-command-event)))))

(bind-keys
 ("M-z" . universal-argument)
 ("C-x C-b" . lastbuf)
 ("C-x b" . helm-buffers-list)
 ("C-x B" . ibuffer)

 ("C-x C-o" . other-window)

 ("M-h" . evil-window-left)
 ("M-j" . evil-window-down)
 ("M-k" . evil-window-up)
 ("M-l" . evil-window-right)
 
 :map evil-normal-state-map
 ("SPC a" . helm-M-x)
 ("SPC f" . helm-mini)
 ("SPC c" . quick-find-file)
 ("SPC s" . save-buffer)
 ("SPC e" . eval-last-sexp)
 ;; ("SPC n" . quick-find-file)
 ;; ("SPC t" . quick-find-file)
 ;; ("SPC f" . quick-find-file)

 ("M-h" . evil-window-left)
 ("M-j" . evil-window-down)
 ("M-k" . evil-window-up)
 ("M-l" . evil-window-right)
)

(evil-define-key 'visual global-map "gdc" 'calc-eval-region)
;;;; Major mode for authinfo files
(define-generic-mode
    'authinfo-mode
  '("#")
  '("machine" "login" "port" "password")
  '()
  '("\\.authinfo$" "\\.authinfo.gpg")
  nil
  "Mode for authinfo files"
  )
;;;; Override default settings
;;; Communication tools
;;;; Mu4e
(defvar user-mailconf nil)
(setq user-mailconf
      '(:maildir "~/.mail/"
		 :name "Andreas Larsen"
		 :signature "----\nAndreas Larsen - northcode.no"
		 :accounts
		 ((:name "northcode"
			 :email "andreas@northcode.no"
			 :smtp "northcode.no"
			 :inbox "Inbox"
			 :sent "Sent"
			 :archive "Archive"
			 :draft "drafts"
			 :trash "Trash"
			 :match-func (lambda (msg)
				       (when msg
					 (mu4e-message-contact-field-matches msg :to "andreas@northcode.no"))))
		  (:name "sopra"
			 :email "andreas.larsen@soprasteria.com"
			 :smtp "localhost"
			 :inbox "Inbox"
			 :sent "Sent"
			 :archive "Arkiv"
			 :draft "Drafts"
			 :trash "Junk"
			 :match-func (lambda (msg)
				       (when msg
					 (mu4e-message-contact-field-matches msg :to "andreas.larsen@soprasteria.com"))))
		  (:name "gmail"
			 :email "northcode.no@gmail.com"
			 :smtp "smtp.gmail.com"
			 :inbox "Inbox"
			 :sent "[Gmail]/Sent Mail"
			 :archive "[Gmail]/All Mail"
			 :draft "drafts"
			 :trash "[Gmail]/Bin"
			 :match-func (lambda (msg)
				       (when msg
					 (mu4e-message-contact-field-matches msg :to "northcode.no@gmail.com"))))
		  (:name "uit"
			 :email "ala107@uit.no"
			 :smtp "smtpserver.uit.no"
			 :inbox "Inbox"
			 :sent "Sent"
			 :archive "Archive"
			 :draft "Drafts"
			 :trash "Deleted Items"
			 :match-func (lambda (msg)
				       (when msg
					 (mu4e-message-contact-field-matches msg :to "ala107@post.uit.no"))))
		  )
		 ))
;; load mu4e if its installed
(if (file-exists-p "/usr/share/emacs/site-lisp/mu4e")
    (progn
      (message "loading mu4e config")
      (load-mu4e-conf)))
;;;; ZNC
(use-package znc
  :straight t
  :bind
  (("C-c i" . znc-all)))
(setq znc-erc-connector 'erc
      znc-erc-ssl-connector 'erc-tls-auth-source
      znc-servers
      (quote
       (("northcode.no" 8667 t
	 ((freenode "northcode/freenode" "")
	  (oftc "northcode/OFTC" "")
	  (snoonet "northcode/snoonet" ""))))))

;;; Customize-stuff
;;;; Vars
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-safe-themes
   (quote
    ("efa785ca9b6da184d934101900d741d60bf274b46ea68addbcd59585302861e3" "39fe48be738ea23b0295cdf17c99054bb439a7d830248d7e6493c2110bfed6f8" "f2209573c119616e65886982f68198297462410486064f533943d7bd725e213b" "60d675485a5582693ab8419e6525481cbc5b19e7a403430a4aa9e1d31d87d832" "3fa7d0fc26c8483c6fdffc9fa5eda229b2f08ab7944728ccdc6743083693750e" "a4d11382b57e6c08c26db2793670642b1fbb828e642cf41ae58685b4e37aeca9" "f8cf128fa0ef7e61b5546d12bb8ea1584c80ac313db38867b6e774d1d38c73db" "8e0c6a96a17a5b45979c31265821053aff9beea9fb5ac5e41130e0c27a89214e" default)))
 '(evil-want-C-u-scroll t)
 '(fci-rule-color "#343d46")
 '(org-agenda-files nil)
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file "todo.org")
      "* TODO ")
     ("c" "Clock in something" entry
      (file "clock.org")
      "* clock-entry: " :clock-in t)
     ("n" "Add Note about something" entry
      (file "notes.org")
      "")
     ("j" "Journal Entry" entry
      (file+olp+datetree "journal.org.gpg")
      "* %?" :empty-lines 1))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-eww org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
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
 '(vc-annotate-very-old-color nil))
;;;; Faces

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 110 :foundry "SRC" :family "Hack")))))


;;; Media stuff

;;;; PDF-Tools
(if (not (eq system-type 'windows-nt))
    (use-package pdf-tools
      :straight t
      :config
      (pdf-tools-install)
      :bind
      (:map pdf-view-mode-map
	    ("k" . pdf-view-previous-line-or-previous-page)
	    ("j" . pdf-view-next-line-or-next-page))))


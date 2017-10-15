;; .emacs -- Emacs config
;;; Commentary:
;;; My Emacs config
;;; Code:


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'load-path "~/.emacs.d/elisp")
;; (add-to-list 'load-path "~/build/org-mode/lisp")

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
(setq evil-move-cursor-back nil)
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


(use-package auto-complete
  :init
  (require 'auto-complete)
  (require 'auto-complete-config)
  (ac-config-default)
  (add-hook 'prog-mode-hook 'auto-complete-mode))

(use-package ac-c-headers
  :init
  (require 'ac-c-headers)
  (let ((acc-hook (lambda () (add-to-list 'ac-sources 'ac-source-c-headers))))
    (add-hook 'c++-mode-hook acc-hook)
    (add-hook 'c-mode-hook acc-hook)))

(defun ac-hook:semantic-mode ()
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'ac-hook:semantic-mode)

(add-hook 'c-mode-common-hook 'semantic-mode)


(use-package recentf
  :init (recentf-mode 1))

(use-package helm
  :init (helm-mode t)
  :config
  (progn
    (bind-keys
     :map helm-map
     ("M-h". helm-find-files-up-one-level)
     ("M-j". helm-next-line)
     ("M-k". helm-previous-line)
     ("M-l". helm-execute-persistent-action)
     :map helm-find-files-map
     ("M-l". helm-execute-persistent-action)
     )))

(use-package helm-projectile)
(use-package helm-company)
(use-package helm-swoop)
(use-package helm-ag)

;; (setq ido-enable-flex-matching t)
;; (define-key ido-completion-map (kbd "<SPC>") 'self-insert-command)

(use-package ido-vertical-mode
  :init (ido-vertical-mode t))

(use-package projectile
  :init (projectile-global-mode t))
(use-package magit)
(use-package magit-gitflow
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(use-package flycheck)
(use-package cmake-font-lock)
(use-package cmake-mode)
(use-package cmake-ide)
(use-package web-mode
  :mode ("\\.html\\'" "\\.php\\'"))
(use-package scss-mode)

(use-package cider)
(use-package clj-refactor)
(use-package parinfer
  :config
  (setq parinfer-extensions '(defaults pretty-parens evil smart-tabs)))

(defun my-clojure-mode-hook ()
  (clj-refactor-mode t)
  (yas-minor-mode t)
  (cljr-add-keybindings-with-prefix "C-c G"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; (use-package tango-plus-theme)
;; (use-package spacegray-theme)
;; (use-package foggy-night-theme)

(use-package org)
(use-package org-bullets)
(use-package org-journal)
(use-package org-doing
  :config
  (setq org-doing-file "~/org/doing.org"))

;; misc stuff
(use-package haste)
(use-package mingus)
(use-package highlight-parentheses)
(use-package xkcd)

(use-package nov
  :config
  (push '("\\.epub\\'" . nov-mode) auto-mode-alist))


(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
	aw-scope 'frame)
  :bind ("C-x C-o" . ace-window))

(use-package znc)

(use-package hledger-mode
  :config
  (setq hledger-jfile (expand-file-name "~/org/main.hledger"))
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :bind
  (("C-c l" . hledger-run-command)
   ("C-c k" . hledger-jentry)))

(use-package eshell
  :config
  (setq eshell-aliases-file (expand-file-name "~/dotfiles/.eshell.alias")))

(require 's)

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
		(:propertize
		 (:eval (when (magit-get-current-branch)
			  (concat " [" (magit-get-current-branch) "]")))
		 face font-lock-string-face)
		" :: "
		(:propertize "%m"
			     face font-lock-constant-face)
		" %e "
		(:eval (format-time-string "%H:%M" (current-time)))
		" %-"))


(defun set-alpha-hook (frame)
  (set-frame-parameter frame 'alpha '(100 85)))

(when (file-readable-p "/usr/share/clang/clang-format.el")
  (load "/usr/share/clang/clang-format.el")
  (add-hook 'c++-mode-hook
	    (lambda () "Add clang-format keybinds"
	      (define-key c++-mode-map (kbd "C-c f") 'clang-format-buffer))))

;; (add-hook 'after-make-frame-functions 'set-alpha-hook)

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
(defun get-string-from-file (filePath) "Return FILEPATH's file content."
       (with-temp-buffer
	 (insert-file-contents filePath)
	 (buffer-string)))
(defun open-ical-calendar () "Open calendar stored in .calendar file."
       (interactive)
       (cfw:open-ical-calendar (get-string-from-file "~/.calendar")))
(defun revbuf () "Revert buffer without asking."
       (interactive)
       (revert-buffer t t))

(defun cpp-insert-header-guard () "Insert a header guard in a cpp header file"
       (interactive)
       (let ((guardname (upcase (s-replace "." "_" (buffer-name)))))
	 (save-excursion (progn
			   (beginning-of-buffer)
			   (insert (format "#ifndef %s\n#define %s\n" guardname guardname))
			   (end-of-buffer)
			   (insert "\n#endif\n")))))


(defun eshell-evil-hat ()
  "Replace hat command in evil in eshell to jump to start of command instead of line."
  (interactive)
  (when (derived-mode-p 'eshell-mode)
    (evil-first-non-blank)
    (evil-find-char 1 ?$)
    (forward-char 2)))

(evil-define-key 'normal eshell-mode-map "^" 'eshell-evil-hat)
(evil-define-key 'normal dired-mode-map "gr" 'revbuf)

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
		   (concat
		    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
		    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(bind-keys
 ("M-z" . universal-argument)
 ("C-c e" . eshell)
 ("C-c E" . ansi-term)
 ("C-c a" . org-agenda)

 ("M-h" . evil-window-left)
 ("M-j" . evil-window-down)
 ("M-k" . evil-window-up)
 ("M-l" . evil-window-right)

 ;; ("C-c h" . helm-mini)
 ("C-c x" . helm-M-x)

 ("C-c g" . magit-status)
 ("C-c j" . org-journal-new-entry)
 ("C-c i" . znc-all)

 ("C-c s" . helm-swoop)
 ("C-c S" . helm-ag)

 ("C-c c" . org-capture)
 ("C-c d" . mingus)
 ("C-c b" . ibuffer)
 ("C-c w" . helm-systemd)
 ("C-c o d" . org-doing)
 
 ("C-<tab>" . hippie-expand)

 ("C-c r f"   . rtags-find-symbol-at-point)
 ("C-c r r"   . rtags-rename-symbol)

 ("C-x C-b" . lastbuf)
 ;; ("C-x C-o" . ace-window)
 ("C-x C-f" . helm-find-files)
 ("C-x C-r" . helm-recentf)
 ("<f5>"    . projectile-compile-project)
 ("<f6>"    . projectile-run-project)
 )

(bind-keys
 :map evil-normal-state-map
 (" " . helm-mini)
 ("C-x C-b" . lastbuf))

(define-key evil-normal-state-map " " 'helm-mini)

(defun startup-script () "Things to do and open at startup."
       (dired "~/projects")
       (find-file "~/.emacs")
       (find-file "~/org/journal.org")
       (find-file "~/org/welcome.org"))

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
			 :trash "[Gmail]/Bin"
			 :match-func (lambda (msg)
				       (when msg
					 (mu4e-message-contact-field-matches msg :to "northcode.no@gmail.com"))))
		  (:name "northcode"
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
		  )
		 ))

(setq podcaster-feeds-urls
      '("http://www.hellointernet.fm/podcast?format=rss"))


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

(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-save-before-save-hook ()
  (unless (file-writable-p (buffer-file-name))
    (when (y-or-n-p "No write access, try with sudo?")
      (sudo-save))))


(add-hook 'before-save-hook 'sudo-save-before-save-hook)


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
				 ))
    )
  )

(defun load-mu4e-conf ()
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (require 'mu4e)
  (require 'smtpmail)
  (require 'evil-mu4e)
  (require 'mu4e-maildirs-extension)

  (use-package evil-mu4e)
  (use-package mu4e-maildirs-extension)
  (use-package mu4e-alert)
  (use-package helm-mu)

  (bind-keys
   ("C-c m" . mu4e))

  (load-mailconf user-mailconf)

  (mu4e-maildirs-extension)
  (mu4e-alert-set-default-style 'libnotify)

  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display)
  )

(if (file-exists-p "/usr/share/emacs/site-lisp/mu4e")
    (progn

      (message "loading mu4e config")
      (load-mu4e-conf)))

(put 'narrow-to-region 'disabled nil)

(evil-set-initial-state 'mingus-help-mode 'emacs)
(evil-set-initial-state 'mingus-browse-mode 'emacs)
(evil-set-initial-state 'mingus-playlist-mode 'emacs)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     ("Dvipdfmx" "dvipdfmx %d" TeX-run-dvipdfmx nil t :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("LaTeXnonint" "%`%l -interaction=nonstopmode %(mode)%' %t" TeX-run-command nil t))))
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "firefox-nightly")
 '(chronos-shell-notify-program "notify-send")
 '(compilation-message-face (quote default))
 '(compilation-read-command nil)
 '(custom-enabled-themes (quote (spacegray)))
 '(custom-safe-themes
   (quote
    ("37bdda288337f95acabe21ae0e60ac5b6850dd2d57bc9ecf4c11d6c83999b4fa" "a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "3b31ebd74082c6a3043dfd8112069163978330e21cfc9e6ff2c9798dfd6d6505" "430f010f861b17aa2d5f5c74a6a7ed43f250ee9b405bf98e49cbebbbb20a9dd7" "ad30746a316d5fb6fedfb585c0aff4f795730d874e3cff87fe33120377a7b8de" "a9a3997a39f1a0771b99329bb628a49d3584c7d270506d9d005becc4f5349b84" "6cca9f90d6078c1f4e2359746fd73adad025375b0c87bc949ceb4ad479a72ef6" "ecb9fe1d5b165a35499191a909b2b5710a52935614058b327a39bfbbb07c7dc8" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" "fe1682ca8f7a255cf295e76b0361438a21bb657d8846a05d9904872aa2fb86f2" "80ceeb45ccb797fe510980900eda334c777f05ee3181cb7e19cd6bb6fc7fda7c" "d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "78559045fb299f3542c232166ad635c59cf0c6578d80a58b885deafe98a36c66" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "38e66a2a20fa9a27af5ffc4f4dd54f69e3fef6b51be7b351e137b24958bfebd7" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" default)))
 '(default-frame-alist (quote ((vertical-scroll-bars))))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring scrolltobottom stamp track)))
 '(erc-nick "northcode")
 '(erc-notify-list (quote ("okknor")))
 '(erc-notify-mode t)
 '(erc-port 8657)
 '(erc-prompt ">")
 '(erc-prompt-for-password nil)
 '(erc-server "northcode.no")
 '(erc-user-full-name "Andreas Larsen")
 '(eshell-cp-interactive-query t)
 '(eshell-default-target-is-dot t)
 '(evil-commentary-mode t)
 '(evil-emacs-state-modes
   (quote
    (archive-mode bbdb-mode biblio-selection-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode mh-folder-mode monky-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode pdf-outline-buffer-mode pdf-view-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode mingus-playlist-mode mingus-browse-mode cider-stacktrace-mode cider-docview-mode xkcd-mode)))
 '(evil-mode t)
 '(evil-want-C-u-scroll t)
 '(flycheck-clang-language-standard "c++11")
 '(forecast-api-key "5a07cfbffbf1ec37e3ac39a305903edc")
 '(forecast-city "Bodø")
 '(forecast-country "Norway")
 '(forecast-latitude 67.285573)
 '(forecast-longitude 14.561691)
 '(global-company-mode t)
 '(helm-mode t)
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-tail-colors
   (quote
    (("#424748" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#424748" . 100))))
 '(hledger-currency-string "kr")
 '(hledger-reporting-day 30)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/org/welcome.org")
 '(magit-commit-arguments (quote ("--gpg-sign=D97194E55873280A")))
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(mingus-mode-always-modeline t)
 '(mingus-use-mouse-p nil)
 '(mu4e-bookmarks
   (quote
    (("flag:unread AND NOT flag:trashed AND NOT maildir:\"/northcode/Junk\"" "Unread messages" 117)
     ("date:today..now AND NOT maildir:\"/northcode/Junk\"" "Today's messages" 116)
     ("date:7d..now AND NOT maildir:\"/northcode/Junk\"" "Last 7 days" 119)
     ("mime:image/*" "Messages with images" 112))))
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(org-agenda-files (quote ("~/org" "~/projects/school/")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (shell . t))))
 '(org-capture-templates
   (quote
    (("n" "Note item" entry
      (file "~/org/notes.org")
      "* ")
     ("a" "Todo item" entry
      (file "~/org/todo.org")
      "* TODO "))))
 '(org-journal-dir "~/Journal/")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-journal-time-format "<%Y-%m-%d %R>")
 '(org-journal-time-prefix "** Journal Entry ")
 '(org-time-stamp-custom-formats (quote ("<%M. %d %Y>" . "<%m/%d/%y %a %H:%M>")))
 '(package-selected-packages
   (quote
    (mic-paren parinfer system-packages org-doing dired-du hledger-mode markdown-mode+ nov tree-mode atom-one-dark-theme anti-zenburn-theme omnisharp chronos ac-c-headers flycheck-kotlin kotlin-mode outshine flycheck auto-complete-c-headers auto-complete elfeed-org elfeed-goodies elfeed podcaster eshell-did-you-mean eshell-up kaolin-theme flycheck-haskell inf-clojure lorem-ipsum paradox magit-gitflow writeroom-mode restclient ag dired+ auctex ace-window ix flycheck-irony irony rtags haskell-mode org-trello bookmark+ gradle-mode fireplace which-key melpa clj-refactor csharp-mode helm-systemd znc ibuffer-git ibuffer-projectile rainbow-mode hexrgb helm-ag yasnippet xkcd web-mode use-package tango-plus-theme spacegray-theme scss-mode powerline-evil org-journal org-bullets mu4e-maildirs-extension mu4e-alert mingus markdown-mode mark-multiple key-chord ido-vertical-mode howdoi highlight-parentheses helm-swoop helm-projectile helm-mu haste foggy-night-theme expand-region evil-surround evil-paredit evil-org evil-mu4e evil-magit evil-commentary darkokai-theme cmake-ide cmake-font-lock cider calfw-gcal calfw)))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(podcaster-mp3-player "/usr/sbin/mpv")
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(safe-local-variable-values
   (quote
    ((eval let
	   ((root
	     (projectile-project-root)))
	   (setq-local flycheck-clang-include-path
		       (list
			(file-truename
			 (concat root "src/alb"))
			(file-truename
			 (concat root "src/inc"))))))))
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
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
 '(which-key-mode t)
 '(znc-erc-connector (quote erc))
 '(znc-erc-ssl-connector (quote erc-tls-auth-source))
 '(znc-servers
   (quote
    (("northcode.no" 8667 t
      ((freenode "northcode/freenode" "")
       (snoonet "northcode/snoonet" "")))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#232830" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 108 :width normal :family "hack"))))
 '(Info-quoted ((t (:family "Liberation Mono"))))
 '(helm-selection ((t (:inherit highlight :background "#eeeeec" :foreground "black"))))
 '(mu4e-unread-face ((t (:inherit font-lock-keyword-face :foreground "#11aaff" :weight bold))))
 '(org-document-title ((t (:foreground "pale turquoise" :weight bold :height 3.0))))
 '(org-level-1 ((t (:weight bold :height 2.0))))
 '(org-level-2 ((t (:height 1.7))))
 '(org-level-3 ((t (:height 1.2))))
 '(widget-field ((t (:box (:line-width 1 :color "#666666"))))))


(startup-script)


(provide 'emacs)
;;; .emacs ends here

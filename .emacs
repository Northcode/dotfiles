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

(use-package company
  :init
  (add-hook 'prog-mode-hook 'company-mode))

(use-package company-c-headers)
(use-package company-jedi)
(use-package company-web)

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
    ))

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

(use-package flycheck)
(use-package cmake-font-lock)
(use-package cmake-mode)
(use-package cmake-ide)
(use-package web-mode
  :mode ("\\.html\\'" "\\.php\\'"))
(use-package scss-mode)

(use-package cider)
(use-package clj-refactor)

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

(use-package mu4e-maildirs-extension)
(use-package mu4e-alert)

(use-package ace-window
  :bind ("C-x C-o" . ace-window))

(use-package znc)

(message "welcome back, master!")

(require 'tls)
(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof
                                       -CAfile /etc/ca-certificates/ca.pem 
                                       -cert /home/andreas/user.pem"
		    "gnutls-cli --priority secure256 
                                 --x509cafile /home/ootput/.private/certs/CAs.pem 
                                 --x509certfile /home/ootput/.private/certs/nick.pem -p %p %h" 
		    "gnutls-cli --priority secure256 -p %p %h"))


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

(defun eshell-evil-hat ()
  "Replace hat command in evil in eshell to jump to start of command instead of line."
  (interactive)
  (when (derived-mode-p 'eshell-mode)
    (evil-first-non-blank)
    (evil-find-char 1 ?$)
    (forward-char 2)))

(evil-define-key 'normal eshell-mode-map "^" 'eshell-evil-hat)
(evil-define-key 'normal dired-mode-map "gr" 'revbuf)

(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(bind-keys
 ("C-c e" . eshell)
 ("C-c E" . ansi-term)
 ("C-c a" . org-agenda)
 ;; ("C-c h" . helm-mini)
 ("C-c x" . helm-M-x)

 ("C-c g" . magit-status)
 ("C-c j" . org-journal-new-entry)
 ("C-c i" . znc-all)

 ("C-c s" . helm-swoop)
 ("C-c S" . helm-ag)

 ("C-c c" . open-ical-calendar)
 ("C-c d" . mingus)
 ("C-c m" . mu4e)
 ("C-c b" . ibuffer)
 ("C-c w" . helm-systemd)

 ("C-<tab>" . hippie-expand)

 ("C-c r f"   . rtags-find-symbol-at-point)
 ("C-c r r"   . rtags-rename-symbol)

 ("C-x C-b" . lastbuf)
 ;; ("C-x C-o" . ace-window)
 ("C-x C-f" . helm-find-files)
 ("C-x C-r" . helm-recentf)
 ("<f5>"    . projectile-compile-project)
 ("<f6>"    . projectile-run-project)
 :map evil-normal-state-map
 (" " . helm-mini)
 )

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

(defun erc-tls-auth-source (a b c d e f g h)
  "Load erc-tls authentication info from .authinfo database."
  (let ((secret (nth 0 (auth-source-search :max 1
					   :host b
					   :port d))))
    (if secret
	(let ((pass (plist-get secret :secret)))
	  (let ((passwd (if (functionp pass)
			    (funcall pass)
			  pass)))
	    (message passwd)
	    (erc-tls a b c d e f g (concat f ":" passwd))))
      (erc-tls a b c d e f g h))))

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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(compilation-message-face (quote default))
 '(compilation-read-command nil)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("fe1682ca8f7a255cf295e76b0361438a21bb657d8846a05d9904872aa2fb86f2" "80ceeb45ccb797fe510980900eda334c777f05ee3181cb7e19cd6bb6fc7fda7c" "d606ac41cdd7054841941455c0151c54f8bff7e4e050255dbd4ae4d60ab640c1" "78559045fb299f3542c232166ad635c59cf0c6578d80a58b885deafe98a36c66" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "38e66a2a20fa9a27af5ffc4f4dd54f69e3fef6b51be7b351e137b24958bfebd7" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" default)))
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
 '(evil-commentary-mode t)
 '(evil-emacs-state-modes
   (quote
    (archive-mode bbdb-mode biblio-selection-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode mh-folder-mode monky-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode pdf-outline-buffer-mode pdf-view-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode mingus-playlist-mode mingus-browse-mode cider-stacktrace-mode cider-docview-mode xkcd-mode)))
 '(evil-mode t)
 '(evil-want-C-u-scroll t)
 '(fci-rule-color "#343d46")
 '(forecast-api-key "5a07cfbffbf1ec37e3ac39a305903edc")
 '(forecast-city "Bodø")
 '(forecast-country "Norway")
 '(forecast-latitude 67.285573)
 '(forecast-longitude 14.561691)
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
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/org/welcome.org")
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
 '(org-agenda-files (quote ("~/Journal")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (shell . t))))
 '(org-capture-templates
   (quote
    (("a" "Todo item" entry
      (file "~/Journal/todo.org")
      "* TODO "))))
 '(org-journal-dir "~/Journal/")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-journal-time-format "<%Y-%m-%d %R>")
 '(org-journal-time-prefix "** Journal Entry ")
 '(org-time-stamp-custom-formats (quote ("<%M. %d %Y>" . "<%m/%d/%y %a %H:%M>")))
 '(package-selected-packages
   (quote
    (writeroom-mode restclient ag dired+ auctex ace-window ix flycheck-irony irony rtags haskell-mode org-trello bookmark+ gradle-mode fireplace which-key melpa clj-refactor csharp-mode helm-systemd znc ibuffer-git ibuffer-projectile rainbow-mode hexrgb helm-ag yasnippet xkcd web-mode use-package tango-plus-theme spacegray-theme scss-mode powerline-evil org-journal org-bullets mu4e-maildirs-extension mu4e-alert mingus markdown-mode mark-multiple key-chord ido-vertical-mode howdoi highlight-parentheses helm-swoop helm-projectile helm-mu helm-company haste foggy-night-theme flycheck expand-region evil-surround evil-paredit evil-org evil-mu4e evil-magit evil-commentary darkokai-theme company-web company-jedi company-c-headers cmake-ide cmake-font-lock cider calfw-gcal calfw)))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
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
 '(znc-erc-connector (quote erc))
 '(znc-erc-ssl-connector (quote erc-tls-auth-source))
 '(znc-servers
   (quote
    (("northcode.no" 8667 t
      ((freenode "northcode" "")))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight light :height 100 :width normal :family "Inconsolata"))))
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

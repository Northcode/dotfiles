(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(package-initialize)

(if (not (package-installed-p 'paradox))
    (package-install 'paradox)
  )

(require 'paradox)

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
	ido-vertical-mode

	;; ace 
	ace-window
	ace-isearch

	;; project management
	projectile
	magit

	;; build tools
	flycheck
	cmake-font-lock
	cmake-mode
	cmake-ide
	
	;; theme 
	spacegray-theme
	powerline
	powerline-evil

	;; org-mode
	org
	org-bullets
	org-journal

	;; misc stuff
	haste
	evil-mu4e
	helm-mu
	))


(dolist (p my-packages)
  (paradox-require p)
  )
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
 '(global-flycheck-mode t)
 '(helm-mode t)
 '(ido-vertical-mode t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/")
 '(initial-scratch-message "")
 '(menu-bar-mode nil)
 '(paradox-github-token t)
 '(projectile-global-mode t)
 '(scroll-bar-mode nil)
 '(tab-always-indent t)
 '(tool-bar-mode nil)
 '(yas-global-mode t nil (yasnippet)))
(evil-magit-init)
(ido-vertical-mode t)
;; set themes
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mu4e-view-body-face ((t (:inherit default))))
 '(powerline-evil-emacs-face ((t (:inherit powerline-evil-base-face :background "purple3"))))
 '(powerline-evil-insert-face ((t (:inherit powerline-evil-base-face :background "RoyalBlue3"))))
 '(powerline-evil-motion-face ((t (:inherit powerline-evil-base-face :background "purple4"))))
 '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "PaleGreen3"))))
 '(powerline-evil-visual-face ((t (:inherit powerline-evil-base-face :background "burlywood3")))))

(powerline-evil-vim-color-theme)

;;;; MU4E config
(if (file-exists-p "/usr/share/emacs/site-lisp/mu4e")
    (progn
      (message "found mu4e!")
      (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
      (require 'mu4e)
      (require 'smtpmail)

      (setq mu4e-maildir "~/.mail")
      (setq mu4e-maildir-list '("~/.mail/gmail" "~/.mail/northcode"))

      (setq mu4e-contexts
	    `( ,(make-mu4e-context
		 :name "gmail"
		 :enter-func (lambda () (progn (mu4e-message "Switch gmail") (message "GMAIL")))
		 ;; leave-func not defined
		 :match-func (lambda (msg)
			       (when msg 
				 (mu4e-message-contact-field-matches msg :to "northcode.no@gmail.com")))
		 :vars '( 
			  ( mu4e-inbox-folder . "/gmail/INBOX")
			  ( mu4e-sent-folder . "/gmail/[Gmail].Sent Mail")
			  ( mu4e-refile-folder . "/gmail/[Gmail].All Mail")
			  ( mu4e-trash-folder . "/gmail/[Gmail].Bin")
			  ( mu4e-drafts-folder  . "/gmail/[Gmail].Drafts")
			  ( mu4e-maildir-shortcuts . (("/gmail/INBOX" . ?i)
						      ("/gmail/[Gmail].All Mail" . ?a)
						      ))
			  ( user-mail-address	     . "northcode.no@gmail.com"  )
			  ( user-full-name	    . "Andreas Larsen" )
			  ( mu4e-compose-signature . (concat
						    "Andreas Larsen ---\n"
						    "northcode.no - Norway"))

			  ( smtpmail-smtp-server . "smtp.gmail.com" )
			  ( smtpmail-smtp-service . 587 )
			  ( starttls-use-gnutls . t )
			  ( smtpmail-starttls-credentials . '(("smtp.gmail.com" 587 nil nil)))
			  ( smtpmail-auth-credentials . '(("smtp.gmail.com" 587 "northcode.no@gmail.com" nil)))
			  ))
	       ,(make-mu4e-context
		 :name "northcode"
		 :enter-func (lambda () (progn (mu4e-message "Switch to northcode") (message "NORTHCODE")))
		 ;; leave-fun not defined
		 :match-func (lambda (msg)
			       (when msg 
				 (mu4e-message-contact-field-matches msg :to "andreas@northcode.no")))
		 :vars '( 
			  (mu4e-inbox-folder . "/northcode/INBOX")
			  ( mu4e-sent-folder . "/northcode/Sent")
			  ( mu4e-drafts-folder  . "/northcode/Drafts")
			  ( mu4e-trash-folder . "/northcode/Trash")
			  ( mu4e-maildir-shortcuts . (("/northcode/INBOX" . ?i)))
			  ( user-mail-address	     . "andreas@northcode.no" )
			  ( user-full-name	    . "Andreas Larsen" )
			  ( mu4e-compose-signature . (concat
						    "Andreas Larsen ---\n"
						    "northcode.no - Norway"))

			  ( smtpmail-smtp-server . "northcode.no" )
			  ( smtpmail-smtp-service . 587 )
			  ( starttls-use-gnutls . t )
			  ( smtpmail-starttls-credentials . '(("northcode.no" 587 nil nil)))
			  ( smtpmail-auth-credentials . '(("northcode.no" 587 "andreas@northcode.no" nil)))
			  ))))

      
      (setq mu4e-context-policy 'pick-first)
      
      ;; (setq mu4e-drafts-folder "/gmail/[Gmail].Drafts")
      ;; (setq mu4e-sent-folder   "/gmail/[Gmail].Sent Mail")
      ;; (setq mu4e-trash-folder  "/gmail/[Gmail].Trash")

      (setq mu4e-sent-messages-behavior 'delete)

      ;; (setq mu4e-maildir-shortcuts
      ;; 	    '( ("/gmail/INBOX"               . ?i)
      ;; 	       ("/gmail/[Gmail].Sent Mail"   . ?s)
      ;; 	       ("/gmail/[Gmail].Trash"       . ?t)
      ;; 	       ("/gmail/[Gmail].All Mail"    . ?a)))

      (setq mu4e-get-mail-command "offlineimap")

      (setq
       user-mail-address "northcode.no@gmail.com"
       user-full-name  "Andreas Larsen"
       mu4e-compose-signature
       (concat 
	"Andreas Larsen\n"
	"http://www.northcode.no\n"))

      (setq message-send-mail-function 'smtpmail-send-it
	    starttls-use-gnutls t
	    smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	    smtpmail-auth-credentials
	    '(("smtp.gmail.com" 587 "northcode.no@gmail.com" nil))
	    smtpmail-default-smtp-server "smtp.gmail.com"
	    smtpmail-smtp-server "smtp.gmail.com"
	    smtpmail-smtp-service 587
	    )

      (global-set-key (kbd "C-c m") 'mu4e)
      (global-set-key (kbd "C-c M") 'mu4e-compose-new)
      (define-key mu4e-headers-mode-map (kbd "C-s") 'helm-mu)
      (define-key mu4e-headers-mode-map (kbd "M") 'mu4e-headers-mark-for-something)
      
      (setq message-kill-buffer-on-exit t)
      (setq mu4e-use-fancy-chars t)
      (setq mu4e-view-show-images t)

      (setq mu4e-html2text-command 'mu4e-shr2text)
      )
  (message "mu4e not found")
  )

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
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq flycheck-clang-language-standard "c++11")
  )
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

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
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t\n]+class[ \t\n]+"))))
      ;; (looking-back "enum[ \t\n]+class[ \t\n]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

;; set html files to load web-mode
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; enable isearch fuzzy searching
(setq search-whitespace-regexp ".*?")

;; set keybinds
(global-unset-key (kbd "M-2"))
(global-unset-key (kbd "M-3"))
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(bind-keys
 ("M-2" . er/expand-region)
 ("M-3" . mark-next-like-this)

 ("C-c e" . eshell)
 ("C-c h" . helm-mini)
 ("C-c x" . helm-M-x)
 ("C-c g" . magit-status)
 ("C-c j" . org-journal-new-entry)
 ("C-c s" . helm-swoop)
 ("C-c c" . cmake-ide-run-cmake)

 ("C-x C-b" . ibuffer)
 ("C-x C-f" . helm-find-files)
 ("C-x o" . ace-window)
 )

(put 'narrow-to-region 'disabled nil)

(provide 'emacs)

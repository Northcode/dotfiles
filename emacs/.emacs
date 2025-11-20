;; -*- eval: (outline-minor-mode) -*-
;; .emacs -- Emacs config with straight.el

;;; Bootstrapping

;;;; Get the actual home directory on windows
(if (eq system-type 'windows-nt)
    (setq win-home (file-truename "~/../../")))

(add-to-list 'load-path "~/.emacs.d/custom-lisp/")

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

;; defaults

(defalias 'yes-or-no-p 'y-or-n-p)

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
      shell-file-name "/bin/sh"
      warning-suppress-types '((comp))
      )

(put 'narrow-to-region 'disabled nil)

(defun save-all () "Save all files."
       (interactive)
       (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

;; Theme

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(use-package northcode-theme
  :straight t
  :config (load-theme 'northcode t))

(set-frame-parameter nil 'alpha-background 80) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth


;; Key bindings
(use-package general :straight t)

(use-package back-button :straight t
  :init (back-button-mode t)
  :general
  ("C--" 'back-button-global)
  ("C-_" 'back-button-global-forward))

(use-package evil :straight t
  :init
  (setq evil-want-keybinding nil
	evil-move-cursor-back nil
	evil-want-minibuffer t
	evil-undo-system 'undo-tree)
  (evil-mode t))

(use-package evil-collection :straight t
  :init
  (evil-collection-init))

(use-package evil-commentary :straight t
  :config
  (evil-commentary-mode t))

(use-package evil-surround :straight t
  :config (global-evil-surround-mode t))

(use-package relative-line-numbers :straight t
  :config (add-hook 'prog-mode-hook 'relative-line-numbers-mode))


(use-package ace-window :straight t
  :general
  ("C-x o" 'ace-window))

(use-package undo-tree 
  :straight (:type git :host nil :repo "http://www.dr-qubit.org/git/undo-tree.git")
  :config (global-undo-tree-mode t))

(use-package back-button :straight t
  :config (back-button-mode t)
  :general
  ("C--" 'back-button-global))

(defun lastbuf () "Switch to last buffer instantly."
       (interactive)
       (switch-to-buffer (other-buffer (current-buffer))))

(general-def "C-x C-b" 'lastbuf)

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

(general-def "C-s" 'custom-isearch)

(defun revbuf () "Revert buffer without asking."
       (interactive)
       (revert-buffer t t))

(general-def "M-z" 'universal-argument
  "C-x C-o" 'other-window)

(use-package expand-region
    :straight t
    :bind (("M-2" . er/expand-region)))

(use-package mark-multiple
    :straight t
    :bind (("M-3" . mark-next-like-this)
	   ("M-#" . mark-previous-like-this)))

(general-def
  "C-c e" 'eshell
  "C-c E" 'ansi-term)

;; Completion

(use-package selectrum-prescient :straight t
  :init
  (selectrum-mode t)
  (selectrum-prescient-mode t)
  :general
  ("C-c x" #'execute-extended-command)
  (selectrum-minibuffer-map
   "M-j" 'next-line
   "M-k" 'previous-line
   "M-h" 'selectrum-backward-kill-sexp
   "M-l" 'selectrum-insert-current-candidate))

(use-package embark :straight t
  :general
  ("M-e" 'embark-act))

(use-package marginalia :straight t
  :config (marginalia-mode t))

;; (use-package company-prescient :straight t
;;   :init
;;   (company-mode t)
;;   (company-prescient-mode t))

(use-package corfu :straight t
  :custom
  (corfu-auto t)
  (corfu-separator ?\s)
  :init
  (setq corfu-auto t
	corfu-quit-no-match 'separator)
  (add-hook 'eshell-mode-hook (lambda ()
				(setq-local corfu-auto nil)
				(corfu-mode)))
  (global-corfu-mode t)
  :general
  (corfu-map
   "C-SPC" 'corfu-insert-separator))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles . (partial-completion))))))

;; Org
(use-package org :straight t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
			       '(
				 (shell . t)
				 )
			       )
  )

(use-package org-present :straight t
  :config
  (add-hook 'org-present-mode-hook
	    (lambda ()
	      (org-present-big)))
  :general
  (:keymaps 'org-mode-map "C-c p" 'org-present))

;; Projects
(use-package projectile :straight t)

(use-package magit :straight t
  :general
  ("C-c g" 'magit-status))

;; Utils

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

(general-def :states 'visual "gdc" 'calc-eval-region)

;; Languages

(use-package flycheck :straight t) 

(use-package lsp-mode :straight t
  :config
  (setq lsp-keymap-prefix "C-c s"
	gc-cons-threshold 100000000
	read-process-output-max (* 1024 1024) ;; 1mb
	)
  :general
  ("C-c c" 'lsp-execute-code-action))

(use-package lsp-ui :straight t
  :config
  (general-def :states 'normal "g]" 'lsp-ui-peek-find-implementation)
  (general-def :states 'normal "g[" 'lsp-ui-peek-find-references)
  (general-define-key :keymaps 'lsp-ui-peek-mode-map "M-j" 'lsp-ui-peek--select-next)
  (general-define-key :keymaps 'lsp-ui-peek-mode-map "M-k" 'lsp-ui-peek--select-prev)
  )

(require 'dap-running-session-mode)

(use-package dap-mode :straight t
  :general
  (:keymaps 'dap-running-session-mode-map "M-n" 'dap-next "C-c q" 'dap-disconnect)
  :config
  (add-hook 'dap-session-created-hook 'dap-running-session-mode)
  (add-hook 'dap-stopped-hook 'dap-running-session-mode)
  (add-hook 'dap-stack-frame-changed-hook (lambda (session)
					    (when (dap--session-running session)
					      (dap-running-session-mode 1))))
  )

(defun java-get-project-name (relpath)
  (let* ((pos (string-match "[^/]*/src" relpath))
	 (posend (string-match "/src" relpath))
	 (name (substring relpath pos posend)))
    name))


(use-package rustic :straight t)

(use-package treemacs :straight t
  :general
  ("C-c t" 'treemacs))


(defun java-set-indent ()
  (setq c-basic-offset 2
	indent-tabs-mode nil)
  )

(use-package lsp-java :straight t
  :init
  (add-hook 'java-mode-hook #'lsp)
  (add-hook 'java-mode-hook #'java-set-indent))

  ;; :config
  ;; (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/1.12.0/jdt-language-server-1.12.0-202206011637.tar.gz"))

  ;; :config
  ;; (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/1.14.0/jdt-language-server-1.14.0-202207211651.tar.gz"))


(use-package yaml-mode :straight t)


(use-package dockerfile-mode :straight t)


;; Mail

(if (file-exists-p "~/.emacs.d/mail.el")
    (load-file "~/.emacs.d/mail.el"))

;; IRC

(use-package rirc
  :general
  ("C-c i" 'irc))

(use-package mu4e 
  :config
  (setq
   mu4e-maildir "~/.mail"
   mu4e-get-mail-command "mbsync -a"
   mu4e-context-policy 'pick-first
   mu4e-sent-messages-behavior 'delete
   message-send-mail-function 'smtpmail-send-it
   mu4e-change-filenames-when-moving t

   mu4e-contexts
	`(
	  ,(make-mu4e-context
	     :name "Northcode"
	     :enter-func (lambda () (mu4e-message "On Northcode"))
             :leave-func (lambda () (mu4e-message "Off Northcode"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
								 :to "andreas@northcode.no")))
             :vars '( ( user-mail-address	    . "andreas@northcode.no"  )
                      ( user-full-name	    . "Andreas Larsen" )
                      ( mu4e-compose-signature . "Andreas Larsen\n")
		      ( mu4e-refile-folder . "/northcode/Archive" )
		      ( mu4e-junk-folder . "/northcode/Junk" )
		      ( mu4e-sent-folder . "/northcode/Sent" )
		      ( mu4e-trash-folder . "/northcode/Trash" )
		      ( mu4e-drafts-folder . "/northcode/Drafts" )
		      ( mu4e-maildir-shortcuts . (( "/northcode/Inbox" . ?i )))
		      ))
	  ,(make-mu4e-context
	     :name "Gmail"
	     :enter-func (lambda () (mu4e-message "On Gmail"))
             :leave-func (lambda () (mu4e-message "Off Gmail"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
								 :to "northcode.no@gmail.com")))
             :vars '( ( user-mail-address	    . "northcode.no@gmail.com"  )
                      ( user-full-name	    . "Andreas Larsen" )
                      ( mu4e-compose-signature . "Andreas Larsen\n")
		      ( mu4e-refile-folder . "/gmail/[Gmail]/All Mail" )
		      ( mu4e-junk-folder . "/gmail/[Gmail]/Spam" )
		      ( mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail" )
		      ( mu4e-trash-folder . "/gmail/[Gmail]/Bin" )
		      ( mu4e-drafts-folder . "/gmail/[Gmail]/Drafts" )
		      ( mu4e-maildir-shortcuts . (( "/gmail/Inbox" . ?i )))
		      ))
	  ))
  :general
  ("C-c m" 'mu4e))

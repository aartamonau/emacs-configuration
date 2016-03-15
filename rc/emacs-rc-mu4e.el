(require 'mu4e)

(setq mu4e-hide-index-messages t)
(setq mu4e-headers-include-related nil)
(setq mu4e-headers-results-limit 250)
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-use-fancy-chars nil)
(setq mu4e-change-filenames-when-moving t)

(setq mu4e-maildir "~/mail")
(setq mu4e-drafts-folder "/aliaksiej.artamonau@gmail.com/[Gmail]/.All Mail")
(setq mu4e-drafts-folder "/aliaksiej.artamonau@gmail.com/[Gmail]/.Drafts")
(setq mu4e-sent-folder   "/aliaksiej.artamonau@gmail.com/[Gmail]/.Sent Mail")
(setq mu4e-trash-folder  "/aliaksiej.artamonau@gmail.com/[Gmail]/.Trash")
(setq mu4e-headers-time-format "%T")
(setq mu4e-headers-fields
      '((:human-date . 12)
        (:flags . 6)
        (:from . 22)
        (:subject)))
(setq mu4e-view-show-images t)
(setq mu4e-confirm-quit nil)

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(add-hook 'mu4e-compose-mode-hook
          (defun cpb-compose-setup ()
            "Outgoing mails get format=flowed."
            (use-hard-newlines t 'guess)))

(defconst inbox-dir "/aliaksiej.artamonau@gmail.com/inbox")
(defconst jira-dir "/aliaksiej.artamonau@gmail.com/Jira")
(defconst code-review-dir "/aliaksiej.artamonau@gmail.com/Code Review")

(setq mu4e-maildir-shortcuts
      `((,inbox-dir         . ?i)
        (,jira-dir          . ?j)
        (,code-review-dir   . ?c)
        (,mu4e-sent-folder  . ?s)
        (,mu4e-trash-folder . ?t)))

(defconst all-unread-query "flag:unread AND NOT flag:trashed")
(defconst inbox-unread-query
  (format "flag:unread AND maildir:\"%s\"" inbox-dir))
(defconst work-unread-query
  (format "flag:unread AND (maildir:\"%s\" OR maildir:\"%s\" OR maildir:\"%s\")"
          inbox-dir jira-dir code-review-dir))
(defconst other-unread-query
  (format "flag:unread AND NOT (maildir:\"%s\" OR maildir:\"%s\" OR maildir:\"%s\")"
          inbox-dir jira-dir code-review-dir))

(setq mu4e-bookmarks
      `((,all-unread-query "All unread messages" ?u)
        (,inbox-unread-query "Inbox unread messages" ?i)
        (,work-unread-query "Work-related unread messages" ?w)
        (,other-unread-query "Other unread messages" ?o)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "mbsync -aVq")

(add-to-list 'mu4e-view-actions
  '("View in browser" . mu4e-action-view-in-browser) t)

(require 'smtpmail)
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

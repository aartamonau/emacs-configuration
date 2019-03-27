(require 'mu4e)
(require 'mail-parse)
(require 'cl)

(setq mu4e-hide-index-messages t)
(setq mu4e-headers-include-related nil)
(setq mu4e-headers-results-limit 250)
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-headers-leave-behavior 'apply)
(setq mu4e-use-fancy-chars nil)
(setq mu4e-change-filenames-when-moving t)

(setq mu4e-maildir "~/mail")
(setq mu4e-drafts-folder "/aliaksiej.artamonau@gmail.com/[Gmail]/Drafts")
(setq mu4e-sent-folder   "/aliaksiej.artamonau@gmail.com/[Gmail]/Sent Mail")
(setq mu4e-trash-folder  "/aliaksiej.artamonau@gmail.com/[Gmail]/Trash")
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

(defconst inbox-dir "/aliaksiej.artamonau@gmail.com/inbox")
(defconst jira-dir "/aliaksiej.artamonau@gmail.com/Jira")
(defconst code-review-dir "/aliaksiej.artamonau@gmail.com/Code Review")

(setq mu4e-maildir-shortcuts
      `((,inbox-dir          . ?i)
        (,jira-dir           . ?j)
        (,code-review-dir    . ?c)
        (,mu4e-sent-folder   . ?s)
        (,mu4e-drafts-folder . ?d)
        (,mu4e-trash-folder  . ?t)))

(defconst all-unread-query "flag:unread AND NOT flag:trashed")
(defconst inbox-unread-query
  (format "flag:unread AND maildir:\"%s\"" inbox-dir))
(defconst work-unread-query
  (format "flag:unread AND (maildir:\"%s\" OR maildir:\"%s\" OR maildir:\"%s\")"
          inbox-dir jira-dir code-review-dir))
(defconst other-unread-query
  (format "flag:unread AND NOT (maildir:\"%s\" OR maildir:\"%s\" OR maildir:\"%s\")"
          inbox-dir jira-dir code-review-dir))
(defconst flagged-query "flag:flagged")

(setq mu4e-bookmarks
      `((,all-unread-query "All unread messages" ?u)
        (,inbox-unread-query "Inbox unread messages" ?i)
        (,work-unread-query "Work-related unread messages" ?w)
        (,other-unread-query "Other unread messages" ?o)
        (,flagged-query "Flagged messages" ?f)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "mbsync -aVq")

(add-to-list 'mu4e-view-actions
  '("View in browser" . mu4e-action-view-in-browser) t)

(require 'smtpmail)
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(defun aa/mu4e-open-url ()
  (interactive)
  (or (mu4e~view-browse-url-from-binding)
      (shr-browse-url)))

(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 50)

(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)
            (local-set-key (kbd "C-c C-o") 'aa/mu4e-open-url)))

(require 'org-mu4e)
(setq org-mu4e-convert-to-html t)

(defun aa/mu4e-org-compose ()
  "Switch to/from mu4e-compose-mode and org-mode"
  (interactive)
  (let ((p (point)))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (not (search-forward "#+OPTIONS: tex:imagemagick" nil t))
        (goto-char (point-max))
        (insert "
#+OPTIONS: tex:imagemagick
#+OPTIONS: toc:0
#+OPTIONS: num:nil
")))
    (goto-char p))
  (if (eq 'mu4e-compose-mode (buffer-local-value 'major-mode (current-buffer)))
      (org~mu4e-mime-switch-headers-or-body)
    (mu4e-compose-mode)))

(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-o") 'aa/mu4e-org-compose)))

(global-set-key (kbd "C-x m") 'mu4e)
(global-set-key (kbd "C-x M") 'mu4e-compose-new)

(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (auto-fill-mode -1)
            (setq-local whitespace-style (remq 'lines-tail whitespace-style))))

(defvar my/alternate-email-accounts
  '(((:name "Aliaksey Artamonau")
     (:email "aliaksey.artamonau@couchbase.com")
     (:pred my/use-couchbase-account-p))))

(defun my/use-couchbase-account-p (recipient)
  (let ((email (car recipient)))
    (string-suffix-p "@couchbase.com" email)))

(setq mu4e-user-mail-address-list
      (cons user-mail-address
            (mapcar (lambda (acc) (cadr (assq :email acc)))
                    my/alternate-email-accounts)))

(defun my/replace-existing-field (field value)
  (save-excursion
    (when (message-position-on-field field)
      (message-beginning-of-header t)
      (delete-region (point) (line-end-position))
      (insert value))))

(defun my/get-recipients (field)
  (let ((value (message-field-value field)))
    (when value
      (mail-header-parse-addresses value))))

(defun my/get-all-recipients ()
  (let ((fields '("to" "cc" "bcc")))
    (mapcan #'my/get-recipients fields)))

(defun my/get-account-pred (acc)
  (cadr (assq :pred acc)))

(defun my/choose-account-by-recipients (recipients)
  (let ((accs my/alternate-email-accounts)
        (found-acc nil)
        (acc nil)
        (pred nil))
    (while (not (or found-acc
                    (null accs)))
      (setq acc (car accs))
      (setq accs (cdr accs))
      (setq pred (my/get-account-pred acc))

      (when (some pred recipients)
        (setq found-acc acc)))
    found-acc))

(defun my/apply-account (acc)
  (let* ((name (cadr (assq :name acc)))
         (email (cadr (assq :email acc)))
         (from (format "%s <%s>" name email))
         (current-from (message-field-value "from")))
    (unless (equal from current-from)
      (my/replace-existing-field "from" from)
      (message "Updated email account to \"%s\"" from)
      t)))

(defun my/maybe-update-email-account ()
  (interactive)
  (let* ((recipients (my/get-all-recipients))
         (acc (my/choose-account-by-recipients recipients)))
    (when acc
      (my/apply-account acc))))

(defun my/maybe-update-email-account-pre-send ()
  (when (my/maybe-update-email-account)
    (unless (y-or-n-p "From field was updated. Continue?")
      (keyboard-quit))))

(add-hook 'mu4e-compose-mode-hook 'my/maybe-update-email-account)
(add-hook 'message-send-hook 'my/maybe-update-email-account-pre-send)

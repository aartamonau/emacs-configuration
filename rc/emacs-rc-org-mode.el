(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun org ()
  (interactive)
  (find-file-existing "~/dev/mine/org/tasks.org"))

(setq org-log-done t)
(setq org-agenda-files
      (list "~/dev/mine/org/tasks.org"
	    "~/dev/mine/org/ideas.org"))
(setq org-time-stamp-custom-formats
      (cons "<%d/%m/%Y>" "<%d/%m/%Y %a %H:%M>"))
(setq org-display-custom-times t)

(setq org-directory "~/dev/mine/org/")
(setq org-default-notes-file
      (concat org-directory "/notes.org"))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)

(setq org-remember-templates
    '(("Idea" ?i "* %^{Brief Description} %^g\n%?\nAdded: %U" "ideas.org" "Ideas")
     )
   )

(setq org-clock-persist t)
(org-clock-persistence-insinuate)

(add-hook 'org-mode-hook 'turn-on-font-lock)

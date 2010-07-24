;; this org mode config is mostly adapted from
;; http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner.html

(defconst todo-template
  (concat ))

(defun org ()
  (interactive)
  (find-file-existing "~/org/todo.org"))

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)
(define-key global-map "\C-cr" 'remember)

(eval-after-load "org"
  '(progn
     (add-hook 'org-mode-hook 'auto-fill-mode)))

(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "\C-n" 'next-line)
     (define-key org-agenda-keymap   "\C-n" 'next-line)
     (define-key org-agenda-mode-map "\C-p" 'previous-line)
     (define-key org-agenda-keymap   "\C-p" 'previous-line)))


(custom-set-variables
 '(org-todo-keywords '((sequence "TODO(t)"
                                 "DEFERRED(D@)"
                                 "STARTED(s!)"
                                 "WAITING(w@)"
                                 "ASSIGN(a!)"

                                 "|"

                                 "DONE(d@)"
                                 "CANCELLED(c@)"
                                 "DELEGATED(l@)")))
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-default-notes-file "~/org/notes.org")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("s" todo "ASSIGN" nil)
           ("d" todo "DELEGATED" nil)
           ("c" todo "DONE|DEFERRED|CANCELLED" nil)
           ("w" todo "WAITING" nil)
           ("W" agenda "" ((org-agenda-ndays 21)))
           ("A" agenda ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
             (org-agenda-ndays 1)
             (org-agenda-overriding-header "Today's Priority #A tasks: ")))
           ("u" alltodo ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                          (quote regexp) "<[^>\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   '(("todo" ?t
      "* ASSIGN %^{Description}\n  SCHEDULED: %t\n  %?"
      "~/org/todo.org" "Unsorted Tasks")

     ("note" ?n
      "* ASSIGN\n  SCHEDULED: %t\n %u %?"
      "~/org/todo.org" "Unsorted Notes")))

 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))

 '(org-time-stamp-custom-formats
   (cons "<%d/%m/%Y>" "<%d/%m/%Y %a %H:%M>"))
 '(org-display-custom-times t)
 '(org-clock-persist t)

 ;; ask for a note for every state change
 '(org-log-into-drawer "LOGBOOK")

 ;; log time for done state
 '(org-log-done 'time))

(org-clock-persistence-insinuate)

(eval-after-load "remember"
  '(progn
     (add-hook 'remember-mode-hook 'org-remember-apply-template)))

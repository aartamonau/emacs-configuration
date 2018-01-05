;; this org mode config is mostly adapted from
;; http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner.html

(require 'org-protocol)
(require 'org-checklist)
(require 'org-drill)
(require 'cl)

(defun org ()
  (interactive)
  (find-file-existing "~/org/todo.org"))

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cr" 'org-capture)

(eval-after-load "org"
  '(progn
     (add-hook 'org-mode-hook 'my/auto-fill-mode-comments-only-disable)

     (define-key org-mode-map "\M-n"    'org-metadown)
     (define-key org-mode-map "\M-p"    'org-metaup)
     (define-key org-mode-map "\M-\C-f" 'org-metaright)
     (define-key org-mode-map "\M-\C-b" 'org-metaleft)))

(defun* my/org-files (dir &key (except nil))
  "Returns a list of org files in a `dir' directory not including
  those that are enumerated in `except'"
  (remove-if (lambda (path) (member (file-name-nondirectory path)
                                    except))
             (file-expand-wildcards (concat dir "/*.org"))))

(custom-set-variables
 '(org-extend-today-until 3)
 '(org-todo-keywords '((sequence "TODO(t)"
                                 "STARTED(s!)"
                                 "PAUSED(p@)"
                                 "WAITING(w@)"
                                 "ASSIGN(a!)"

                                 "|"

                                 "DONE(d@)"
                                 "CANCELLED(c@)")
                       (sequence "TODO(t)"
                                 "ONLINE(n!)"
                                 "OFFLINE(f!)"
                                 "PAUSED(p@)"

                                 "|"

                                 "DONE(d@)")))
 `(org-agenda-files (quote
                     ,(my/org-files "~/org")))
 '(org-default-notes-file "~/org/notes.org")
 '(org-agenda-ndays 1)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("A" agenda "Today"
            ((org-agenda-tag-filter-preset '("-COUCHBASE" "-drill"))
             (org-agenda-skip-function
              '(org-agenda-skip-entry-if 'todo '("ASSIGN" "WAITING")))))
           ("T" agenda "Tomorrow"
            ((org-agenda-tag-filter-preset '("-COUCHBASE" "-drill"))
             (org-agenda-start-day "+1")
             (org-agenda-ndays 1)
             (org-agenda-skip-function
              '(org-agenda-skip-entry-if 'todo '("ASSIGN" "WAITING")))))
           ("W" agenda "Week"
            ((org-agenda-ndays 7)
             (org-agenda-tag-filter-preset '("-COUCHBASE" "-drill"))
             (org-agenda-skip-function
              '(org-agenda-skip-entry-if 'todo '("ASSIGN" "WAITING")))))

           ("C" . "Couchbase agendas")
           ("CA" agenda "Today"
            ((org-agenda-tag-filter-preset '("+COUCHBASE"))
             (org-agenda-skip-function
              '(org-agenda-skip-entry-if 'todo '("ASSIGN" "WAITING")))))
           ("CT" agenda "Tomorrow"
            ((org-agenda-tag-filter-preset '("+COUCHBASE"))
             (org-agenda-start-day "+1")
             (org-agenda-ndays 1)
             (org-agenda-skip-function
              '(org-agenda-skip-entry-if 'todo '("ASSIGN" "WAITING")))))
           ("CW" agenda "Week"
            ((org-agenda-tag-filter-preset '("+COUCHBASE"))
             (org-agenda-ndays 7)
             (org-agenda-skip-function
              '(org-agenda-skip-entry-if 'todo '("ASSIGN" "WAITING")))))

           ("Q" . "Queries")
           ("QN" "Next" tags "NEXT")
           ("QW" "Waiting" tags-todo "TODO=\"WAITING\"|WAITING" nil)
           ("QA" "Unassigned" todo "ASSIGN" nil)
           ("QD" "Completed" todo "DONE|CANCELLED" nil)
           ("QU" "Unscheduled" alltodo "TODO"
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                          (quote regexp) "<[^>\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-remember-store-without-prompt t)
 '(org-capture-templates
   '(("t" "todo" entry
      (file+headline "~/org/todo.org" "Unsorted Tasks")
      "* TODO %^{Title}\n  SCHEDULED: %t\n  %?")
     ("p" "protocol-capture" entry
      (file+headline "~/org/todo.org" "Unsorted Tasks")
      "* TODO %c\n  SCHEDULED: %t\n\n  %i" :immediate-finish t)
     ("n" "note" entry
      (file+headline "" "Misc notes")
      "* %^{Title}\n  %u\n  %?")
     ("w" "word" entry
      (file+headline "~/org/english.org" "Pending words")
      "* %^{Word}\n  %u\n  %?")))

 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))

 '(org-time-stamp-custom-formats
   (cons "<%d/%m/%Y>" "<%d/%m/%Y %a %H:%M>"))
 '(org-display-custom-times t)
 '(org-clock-persist t)

 ;; ask for a note for every state change
 '(org-log-into-drawer "LOGBOOK")

 ;; log time for done state
 '(org-log-done 'time)

 '(org-habit-show-habits-only-for-today nil)
 '(org-habit-graph-column 60)

 '(org-agenda-time-grid
   '((daily weekly)
     ""
     (600 800 1000 1200 1400 1600 1800 2000 2200 2359)))
 '(org-agenda-sorting-strategy
   '((agenda time-up habit-down priority-down category-keep)
     (todo   priority-down category-keep)
     (tags   priority-down category-keep)
     (search category-keep)))

 '(org-columns-default-format
   "%75ITEM %SCHEDULED %TAGS %PRIORITY %8Effort(ESTIMATE){:} %8CLOCKSUM(CLOCK)")

 '(org-clock-in-switch-to-state "STARTED")
 '(org-clock-out-switch-to-state "PAUSED")

 '(org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 3))

 '(org-refile-use-outline-path 'file)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

 '(org-mobile-directory "~/Dropbox/MobileOrg")
 '(org-mobile-files-exclude-regexp "^\\(english\\|from-mobile\\)\\.org$")
 '(org-mobile-agendas '("A"))

 '(org-drill-maximum-duration 15)
 '(org-tags-column 80))

(custom-set-faces
 '(org-level-1 ((t (:height 1.0))))
 '(org-level-2 ((t (:height 1.0))))
 '(org-level-3 ((t (:height 1.0))))
 '(org-level-4 ((t (:height 1.0))))
 '(org-level-5 ((t (:height 1.0))))
 '(org-level-6 ((t (:height 1.0))))
 '(org-level-7 ((t (:height 1.0))))
 '(org-level-8 ((t (:height 1.0)))))

(add-to-list 'org-modules 'org-habit)

(org-clock-persistence-insinuate)

;; automatically resume clocks when starting daemon
(when (daemonp)
  (setq org-clock-persist-query-resume nil))

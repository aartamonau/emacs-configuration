;; this org mode config is mostly adapted from
;; http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner.html

(require 'org-checklist)

(global-set-key "\C-ca" 'org-agenda)

(eval-after-load "org"
  '(progn
     (add-hook 'org-mode-hook
               (lambda ()
                 ;; auto-fill everything, not just comments in org-mode
                 (set-variable 'comment-auto-fill-only-comments nil t)))

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
                                 "PAUSED(p!)"
                                 "WAITING(w@)"
                                 "ASSIGN(a!)"

                                 "|"

                                 "DONE(d@)"
                                 "CANCELLED(c@)")))
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
            ((org-agenda-skip-function
              '(org-agenda-skip-entry-if 'todo '("ASSIGN" "WAITING")))))
           ("T" agenda "Tomorrow"
            ((org-agenda-start-day "+1")
             (org-agenda-ndays 1)
             (org-agenda-skip-function
              '(org-agenda-skip-entry-if 'todo '("ASSIGN" "WAITING")))))
           ("W" agenda "Week"
            ((org-agenda-ndays 7)
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
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))

 '(org-time-stamp-custom-formats
   (cons "<%B %d, %Y>" "<%B %d, %Y %H:%M>"))
 '(org-display-custom-times t)
 '(org-clock-persist t)

 ;; ask for a note for every state change
 '(org-log-into-drawer "LOGBOOK")

 ;; log time for done state
 '(org-log-done 'time)

 '(org-agenda-time-grid
   '((daily today require-timed)
     (600 800 1000 1200 1400 1600 1800 2000 2200 2359)
     "......" "----------------"))

 '(org-agenda-sorting-strategy
   '((agenda time-up priority-down category-keep)
     (todo   priority-down category-keep)
     (tags   priority-down category-keep)
     (search category-keep)))

 '(org-columns-default-format
   "%TODO %75ITEM %SCHEDULED %TAGS %PRIORITY %8Effort(ESTIMATE){:} %8CLOCKSUM(CLOCK)")

 '(org-clock-in-switch-to-state "STARTED")
 '(org-clock-out-switch-to-state "PAUSED")

 '(org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 3))

 '(org-refile-use-outline-path 'file)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

 '(org-tags-column 80)
 '(org-insert-heading-respect-content t)
 '(org-goto-interface 'outline-path-completion)
 '(org-enforce-todo-dependencies t)

 '(org-clock-report-include-clocking-task t)

 '(org-export-date-timestamp-format "%B %d, %Y")
 '(org-odt-use-date-fields t)

 '(org-cycle-emulate-tab 'white))

(org-clock-persistence-insinuate)

(global-set-key (kbd "C-c x J") 'my/org-goto)
(global-set-key (kbd "C-c x j") 'my/org-open-dwim)

(defun my/org-goto-buffer ()
  (find-file-existing "~/org/todo.org")
  (goto-char 0))

(defun my/org-open-dwim (&optional select)
  (interactive "@P")
  (condition-case err (org-clock-goto select)
    (error (message "Error in org-clock-goto: %s" (cdr err))
           (my/org-goto-buffer))))

(defun my/org-goto (&optional alternative-interface)
  (interactive "P")

  (unless (eq major-mode 'org-mode)
      ;; switch to org buffer only if we're not already there
    (my/org-goto-buffer))
  (org-goto alternative-interface))

(setq org-use-speed-commands t)

(defadvice org-mark-element (after org-mark-element-activate last activate)
  (activate-mark))

(defadvice org-mark-subtree (after org-mark-subtree-activate last activate)
  (activate-mark))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comment-auto-fill-only-comments t)
 '(custom-safe-themes
   (quote
    ("866be962f0a48c2fe648ea23a3f3c0148e5747d05626d75b6eaa9cd55a44c592" default)))
 '(desktop-restore-eager 10)
 '(dired-recursive-deletes (quote always))
 '(diredp-hide-details-initially-flag nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 78)
 '(haskell-doc-show-prelude nil)
 '(haskell-doc-show-reserved nil)
 '(ibuffer-expert t)
 '(ibuffer-movement-cycle nil)
 '(ibuffer-saved-filter-groups
   (quote
    (("default"
      ("Org"
       (mode . org-mode))
      ("Haskell"
       (mode . haskell-mode))
      ("C/C++"
       (or
        (mode . c-mode)
        (mode . c++-mode)))
      ("Python"
       (mode . python-mode))
      ("Emacs"
       (mode . emacs-lisp-mode))
      ("Grep"
       (mode . grep-mode))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-sorting-mode (quote alphabetic) t)
 '(ido-enable-flex-matching t)
 '(ido-show-dot-for-dired t)
 '(ispell-dictionary "american")
 '(ispell-extra-args (quote ("-i" "utf-8")))
 '(ispell-local-dictionary-alist
   (quote
    (("american" "[A-Za-z]" "[^A-Za-z]" "[']" t
      ("-d" "en_US")
      nil utf-8)
     ("russian" "[А-Яа-я]" "[^А-Яа-я]" "[']" t
      ("-d" "ru_RU")
      nil utf-8))))
 '(ispell-program-name "hunspell")
 '(ispell-silently-savep t)
 '(js2-basic-offset 4)
 '(js2-mirror-mode nil)
 '(ledger-reports
   (quote
    (("balance" "ledger -f %(ledger-file) -C balance '^(assets|liabilities):'")
     ("transactions" "ledger -f %(ledger-file) -C register ^%(account)")
     ("payee" "ledger -f %(ledger-file) -C register -- %(payee)")
     ("account" "ledger -f %(ledger-file) -C register %(account)"))))
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3)))
 '(org-agenda-custom-commands
   (quote
    (("A" agenda "Today"
      ((org-agenda-tag-filter-preset
        (quote
         ("-COUCHBASE" "-drill")))
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote todo)
          (quote
           ("ASSIGN" "WAITING")))))))
     ("T" agenda "Tomorrow"
      ((org-agenda-tag-filter-preset
        (quote
         ("-COUCHBASE" "-drill")))
       (org-agenda-start-day "+1")
       (org-agenda-ndays 1)
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote todo)
          (quote
           ("ASSIGN" "WAITING")))))))
     ("W" agenda "Week"
      ((org-agenda-ndays 7)
       (org-agenda-tag-filter-preset
        (quote
         ("-COUCHBASE" "-drill")))
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote todo)
          (quote
           ("ASSIGN" "WAITING")))))))
     ("C" . "Couchbase agendas")
     ("CA" agenda "Today"
      ((org-agenda-tag-filter-preset
        (quote
         ("+COUCHBASE")))
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote todo)
          (quote
           ("ASSIGN" "WAITING")))))))
     ("CT" agenda "Tomorrow"
      ((org-agenda-tag-filter-preset
        (quote
         ("+COUCHBASE")))
       (org-agenda-start-day "+1")
       (org-agenda-ndays 1)
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote todo)
          (quote
           ("ASSIGN" "WAITING")))))))
     ("CW" agenda "Week"
      ((org-agenda-tag-filter-preset
        (quote
         ("+COUCHBASE")))
       (org-agenda-ndays 7)
       (org-agenda-skip-function
        (quote
         (org-agenda-skip-entry-if
          (quote todo)
          (quote
           ("ASSIGN" "WAITING")))))))
     ("Q" . "Queries")
     ("QN" "Next" tags "NEXT")
     ("QW" "Waiting" tags-todo "TODO=\"WAITING\"|WAITING" nil)
     ("QA" "Unassigned" todo "ASSIGN" nil)
     ("QD" "Completed" todo "DONE|CANCELLED" nil)
     ("QU" "Unscheduled" alltodo "TODO"
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote scheduled)
           (quote deadline)
           (quote regexp)
           "<[^>
]+>")))
       (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-agenda-ndays 1)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda time-up habit-down priority-down category-keep)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-time-grid
   (quote
    ((daily weekly)
     ""
     (600 800 1000 1200 1400 1600 1800 2000 2200 2359))))
 '(org-capture-templates
   (quote
    (("t" "todo" entry
      (file+headline "~/org/todo.org" "Unsorted Tasks")
      "* TODO %^{Title}
  SCHEDULED: %t
  %?")
     ("p" "protocol-capture" entry
      (file+headline "~/org/todo.org" "Unsorted Tasks")
      "* TODO %c
  SCHEDULED: %t

  %i" :immediate-finish t)
     ("n" "note" entry
      (file+headline "" "Misc notes")
      "* %^{Title}
  %u
  %?")
     ("w" "word" entry
      (file+headline "~/org/english.org" "Pending words")
      "* %^{Word}
  %u
  %?"))))
 '(org-clock-in-switch-to-state "STARTED")
 '(org-clock-out-switch-to-state "PAUSED")
 '(org-clock-persist t)
 '(org-columns-default-format
   "%30ITEM %SCHEDULED %TAGS %PRIORITY %8Effort(ESTIMATE){:} %8CLOCKSUM(CLOCK)")
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/org/notes.org")
 '(org-display-custom-times t)
 '(org-drill-maximum-duration 15)
 '(org-extend-today-until 3)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-habit-graph-column 60)
 '(org-habit-show-habits-only-for-today nil)
 '(org-log-done (quote time))
 '(org-log-into-drawer "LOGBOOK")
 '(org-mobile-agendas (quote ("A")))
 '(org-mobile-directory "~/Dropbox/MobileOrg")
 '(org-mobile-files-exclude-regexp "^\\(english\\|from-mobile\\)\\.org$")
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))
 '(org-refile-use-outline-path (quote file))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote
    (("todo" 116 "* ASSIGN %^{Description}
  %u
  %?" "~/org/todo.org" "Unsorted Tasks")
     ("note" 110 "* %^{Title} :NOTE:
  %u
  %?" "~/org/todo.org" "Unsorted Notes"))))
 '(org-reverse-note-order t)
 '(org-time-stamp-custom-formats (cons "<%d/%m/%Y>" "<%d/%m/%Y %a %H:%M>"))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "STARTED(s!)" "PAUSED(p@)" "WAITING(w@)" "ASSIGN(a!)" "|" "DONE(d@)" "CANCELLED(c@)")
     (sequence "TODO(t)" "ONLINE(n!)" "OFFLINE(f!)" "PAUSED(p@)" "|" "DONE(d@)"))))
 '(quack-pretty-lambda-p nil)
 '(quack-programs
   (quote
    ("mred -l trace" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mred -zl" "mzscheme" "mzscheme -M errortrace" "mzscheme -l" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(reb-re-syntax (quote string))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace
               (point-min)
               (point-max))
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face trailing lines-tail)
     (require-final-newline . t))))
 '(split-height-threshold 10000)
 '(undo-tree-mode-lighter " ut")
 '(undo-tree-visualizer-timestamps t)
 '(wg-morph-on nil)
 '(wg-prefix-key (kbd "C-c w"))
 '(whitespace-global-modes (quote (not org-mode)))
 '(whitespace-style (quote (face tabs trailing lines-tail empty tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-field-face ((t (:foreground "#ad7fa8"))))
 '(agda2-highlight-function-face ((t (:inherit font-lock-function-name-face))))
 '(agda2-highlight-inductive-constructor-face ((t (:foreground "#8ae234"))))
 '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(agda2-highlight-module-face ((t (:inherit font-lock-builtin-face))))
 '(agda2-highlight-number-face ((t (:inherit font-lock-constant-face))))
 '(agda2-highlight-postulate-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-primitive-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-primitive-type-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-record-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-string-face ((t (:inherit font-lock-string-face))))
 '(diff-added ((t (:foreground "green3" :weight bold))))
 '(diff-removed ((t (:foreground "red3" :weight bold))))
 '(magit-item-highlight ((t (:background "#505050" :inherit nil)))))

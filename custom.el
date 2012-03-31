(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comment-auto-fill-only-comments t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 78)
 '(ibuffer-expert t)
 '(ibuffer-movement-cycle nil)
 '(ibuffer-saved-filter-groups (quote (("default" ("Org" (mode . org-mode)) ("Haskell" (mode . haskell-mode)) ("C/C++" (or (mode . c-mode) (mode . c++-mode))) ("Python" (mode . python-mode)) ("Emacs" (mode . emacs-lisp-mode)) ("Grep" (mode . grep-mode))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-sorting-mode (quote alphabetic))
 '(ido-enable-flex-matching t)
 '(ido-show-dot-for-dired t)
 '(ispell-dictionary "american")
 '(ispell-extra-args (quote ("-i" "utf-8")))
 '(ispell-local-dictionary-alist (quote (("american" "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_US") nil utf-8) ("russian" "[А-Яа-я]" "[^А-Яа-я]" "[']" t ("-d" "ru_RU") nil utf-8))))
 '(ispell-program-name "hunspell")
 '(ispell-silently-savep t)
 '(js2-basic-offset 4)
 '(js2-mirror-mode nil)
 '(org-agenda-custom-commands (quote (("s" todo "ASSIGN" nil) ("d" todo "DEFERRED" nil) ("c" todo "DONE|DEFERRED|CANCELLED" nil) ("w" todo "WAITING" nil) ("A" agenda "" ((org-agenda-skip-function (quote my/agenda-filter)) (org-agenda-overriding-header "Agenda (no ASSIGN|WATING)"))) ("W" agenda "" ((org-agenda-skip-function (quote my/agenda-filter)) (org-agenda-overriding-header "Agenda (no ASSIGN|WATING)") (org-agenda-ndays 7))) ("T" agenda "" ((org-agenda-skip-function (quote my/agenda-filter)) (org-agenda-overriding-header "Agenda (no ASSIGN|WATING)") (org-agenda-start-day "+1") (org-agenda-ndays 1))) ("C" agenda "Couchbase" ((org-agenda-skip-function (quote my/couchbase-filter)))) ("u" alltodo "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline) (quote regexp) "<[^>
]+>"))) (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (quote ("~/org/couchbase.org" "~/org/health.org" "~/org/lifelong.org" "~/org/money.org" "~/org/todo.org")))
 '(org-agenda-ndays 1)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy (quote ((agenda time-up habit-down priority-down category-keep) (todo priority-down category-keep) (tags priority-down category-keep) (search category-keep))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-time-grid (quote ((daily weekly) "" (600 800 1000 1200 1400 1600 1800 2000 2200 2359))))
 '(org-clock-in-switch-to-state "STARTED")
 '(org-clock-out-switch-to-state "PAUSED")
 '(org-clock-persist t)
 '(org-columns-default-format "%30ITEM %SCHEDULED %TAGS %PRIORITY %8Effort(ESTIMATE){:} %8CLOCKSUM(CLOCK)")
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/org/notes.org")
 '(org-display-custom-times t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-habit-graph-column 60)
 '(org-habit-show-habits-only-for-today nil)
 '(org-log-done (quote time))
 '(org-log-into-drawer "LOGBOOK")
 '(org-mobile-directory "~/Dropbox/MobileOrg")
 '(org-mobile-inbox-for-pull "~/org/flagged.org")
 '(org-remember-store-without-prompt t)
 '(org-remember-templates (quote (("todo" 116 "* ASSIGN %^{Description}
  %u
  %?" "~/org/todo.org" "Unsorted Tasks") ("note" 110 "* %^{Title} :NOTE:
  %u
  %?" "~/org/todo.org" "Unsorted Notes"))))
 '(org-reverse-note-order t)
 '(org-time-stamp-custom-formats (cons "<%d/%m/%Y>" "<%d/%m/%Y %a %H:%M>"))
 '(org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s!)" "PAUSED(p@)" "WAITING(w@)" "ASSIGN(a!)" "|" "DONE(d@)" "DEFERRED(D@)" "CANCELLED(c@)") (sequence "TODO(t)" "ONLINE(n!)" "OFFLINE(f!)" "PAUSED(p@)" "|" "DONE(d@)"))))
 '(quack-pretty-lambda-p nil)
 '(quack-programs (quote ("mred -l trace" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mred -zl" "mzscheme" "mzscheme -M errortrace" "mzscheme -l" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(reb-re-syntax (quote string))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace (point-min) (point-max)) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t))))
 '(whitespace-global-modes (quote (not org-mode)))
 '(whitespace-style (quote (face tabs trailing lines-tail empty tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "DarkSlateGray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 103 :width normal :family "misc-fixed"))))
 '(cursor ((t (:background "DarkGray"))))
 '(flymake-errline ((((class color)) (:background "dark red"))))
 '(flymake-warnline ((((class color)) (:background "gray7"))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "DarkBlue"))))
 '(region ((((class color) (min-colors 88) (background dark)) (:background "DarkSeaGreen4")))))

;; Prevent the startup message
(setq inhibit-startup-message t)

;; No toolbar
(tool-bar-mode 0)

;; No menubar
(menu-bar-mode 0)

;; Display time in status bar
(display-time)

;; Display column number in status bar
(column-number-mode 't)

;; Vertical scrollbar to the right
;; (set-scroll-bar-mode 'right)

;; No scrollbar
(scroll-bar-mode -1)

;; Scrolling
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)
(setq scroll-margin 10)

;; type "y"/"n" instead of "yes"/"no"
(fset 'yes-or-no-p 'y-or-n-p)

;; normal pasting from X applications
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; all operations that copy some text to x selection will do so
;; for clipboard too
(setq x-select-enable-clipboard t)

;; highlight region between point and mark
(transient-mark-mode nil)

;; ;; highlight current line
;; (hl-line-mode 1)

;; replace tabs with spaces
(setq-default indent-tabs-mode nil)

;; hide cursor in non-selected windows
(set-default 'cursor-in-non-selected-windows nil)

;; default font
(set-default-font "DejaVu Sans Mono-9")

;; the first day of week is monday
(setq calendar-week-start-day 1)

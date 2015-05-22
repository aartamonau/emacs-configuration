;; (autoload 'eclipse-mode "/eclipse.el" "ECLiPSe editing mode" t)
;; (autoload 'eclipse-esp-mode "<PATH>/eclipse.el" "ECLiPSe-ESP editing mode" t)
(setq auto-mode-alist (cons '("\\.ecl" . eclipse-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.esp" . eclipse-esp-mode) auto-mode-alist))

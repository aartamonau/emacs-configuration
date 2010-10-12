(eval-after-load "ispell"
  '(progn
     (setq ispell-program-name "hunspell"
           ispell-dictionary "american"
           ispell-extra-args '("-a" "-i" "utf-8")
           ispell-silently-savep t
           ispell-local-dictionary-alist
           '((nil ; default
              "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_US" "-i" "utf-8") nil utf-8)
             ("american" ; US English
              "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_US" "-i" "utf-8") nil utf-8)
             ("russian" ; russian
              "[А-Яа-я]" "[^А-Яа-я]" "[']" t ("-d" "ru_RU" "-i" "utf-8") nil utf-8)
             )
           )))


(add-hook 'text-mode-hook 'turn-on-flyspell)
;; (add-hook 'flyspell-mode-hook
;;           (lambda ()
;;             (set-process-query-on-exit-flag
;;               (get-buffer-process (current-buffer)) nil)))

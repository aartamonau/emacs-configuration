(custom-set-variables
 '(ispell-program-name "hunspell")
 '(ispell-dictionary "american")
 '(ispell-extra-args '("-i" "utf-8"))
 '(ispell-silently-savep t)
 '(ispell-local-dictionary-alist
   '(("american"
      "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_US") nil utf-8)
     ("russian"
      "[А-Яа-я]" "[^А-Яа-я]" "[']" t ("-d" "ru_RU") nil utf-8))))

(add-hook 'text-mode-hook 'turn-on-flyspell)

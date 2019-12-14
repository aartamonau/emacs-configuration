;; For some reason erlang likes adding /lib/erlang to the root dir on its own.
(defconst erlang-root-dir "/usr")
(defconst erlang-complete-root-dir
  (concat (file-name-as-directory erlang-root-dir) "lib/erlang"))

(defconst erlang-lib-dir
  (concat (file-name-as-directory erlang-complete-root-dir) "lib"))
(defconst erlang-bin-dir
  (concat (file-name-as-directory erlang-complete-root-dir) "bin"))
(defconst erlang-tools-dir
  (and (file-accessible-directory-p erlang-lib-dir)
       (concat (file-name-as-directory erlang-lib-dir)
               (first (directory-files erlang-lib-dir nil "^tools-.*")))))
(defconst erlang-emacs-dir
  (concat (file-name-as-directory erlang-tools-dir) "emacs"))

(defun my/erlang-get-thing-at-point ()
  (interactive)
  (my/identifier-to-string (erlang-get-identifier-at-point)))

(defun my/identifier-to-string (identifier)
  (pcase identifier
    (`(qualified-function ,module ,name ,arity)
     (if module
         (format "%s:%s" module name)
       name))
    (`(record ,module ,name ,arity) (format "#%s" name))
    (`(module ,module ,name ,arity) (format "%s:" name))
    (`(macro ,module ,name ,arity) (format "?%s" name))
    (`(nil ,module ,name ,arity) name)))

(when (file-accessible-directory-p erlang-emacs-dir)
  (add-to-list 'load-path erlang-emacs-dir)

  (require 'erlang-start)

  (add-hook 'erlang-mode-hook 'global-hook-handler)
  (add-hook 'erlang-mode-hook
            (lambda ()
              (set (make-local-variable 'find-tag-default-function)
                   'my/erlang-get-thing-at-point))))

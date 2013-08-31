(defconst erlang-root-dir "/usr/lib/erlang")
(defconst erlang-lib-dir
  (concat (file-name-as-directory erlang-root-dir) "lib"))
(defconst erlang-bin-dir
  (concat (file-name-as-directory erlang-root-dir) "bin"))
(defconst erlang-tools-dir
  (and (file-accessible-directory-p erlang-lib-dir)
       (concat (file-name-as-directory erlang-lib-dir)
               (first (directory-files erlang-lib-dir nil "^tools-.*")))))
(defconst erlang-emacs-dir
  (concat (file-name-as-directory erlang-tools-dir) "emacs"))

(when (file-accessible-directory-p erlang-emacs-dir)
  (add-to-list 'load-path erlang-emacs-dir)

  (require 'erlang-start)
  (require 'erlang-flymake)
  (add-hook 'erlang-mode-hook 'global-hook-handler)
  (add-hook 'erlang-mode-hook 'gtags-mode))

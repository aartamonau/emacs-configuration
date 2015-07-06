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

(defun setup-edts ()
  (require 'edts-start)
  (require 'edts-project)

  (require 'eproject)
  (require 'f)

  (defconst couchbase-root (f-expand "~/dev/membase/"))

  (defun my/ns-server-project-selector (file-name)
    (let ((res (and (f-ancestor-of? couchbase-root file-name)
                    (f-traverse-upwards (lambda (path)
                                          (equal (f-base path) "ns_server"))
                                        file-name))))
      (when res
        ;; ugly but needed to let this project type overrule edts' and
        ;; generic-git for the same root
        (concat (f-expand res)
                "/../"
                (f-base res)))))

  (define-project-type ns-server (edts)
    (my/ns-server-project-selector file)
    :xref-error-whitelist ("Call to undefined function 'ale_logger"))

  (advice-add 'edts-code-eunit :around
              (lambda (fun args)
                "don't run unit tests in ns_server"
                (unless (eq (eproject-type) 'ns-server)
                  (apply fun args))))

  ;; override edts' hackish defadvice
  (defadvice eproject--all-types (around edts-eproject-types activate) ad-do-it))

(when (file-accessible-directory-p erlang-emacs-dir)
  (add-to-list 'load-path erlang-emacs-dir)

  (require 'erlang-start)

  (setup-edts)

  (add-hook 'erlang-mode-hook 'global-hook-handler)
  (add-hook 'erlang-mode-hook 'edts-mode)
  (add-hook 'edts-mode-hook
            (lambda ()
              (define-key edts-mode-map (kbd "M-*") 'edts-find-source-unwind)
              (define-key edts-mode-map (kbd "C-c n") 'edts-code-next-issue)
              (define-key edts-mode-map (kbd "C-c p") 'edts-code-previous-issue))))

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

(defun setup-edts ()
  (setq edts-plugin-disabled-plugins '("edts_debug" "edts_dialyzer"))

  (setq edts-log-level 'debug)
  (require 'edts-start)

  (edts-man-set-root erlang-complete-root-dir)

  ;; workaround a bug in xref plugin
  (add-to-list 'edts-project-valid-properties
               '(:xref-file-whitelist . edts-project--string-list?))
  (add-to-list 'edts-project-valid-properties
               '(:xref-error-whitelist . edts-project--string-list?))

  (advice-add 'edts-code-eunit :around
              (lambda (fun args)
                "don't run unit tests in ns_server"
                (unless (equal (edts-project-name) "ns_server")
                  (apply fun args))))

  (advice-add 'f-this-file :around
              (lambda (fun &optional args)
                "if we are loading emacs.desktop, pretend we don't;
otherwise edts fails start node on restart"
                (if (and load-in-progress
                         (equal (f-filename load-file-name) ".emacs.desktop"))
                    (let ((load-in-progress nil))
                      (apply fun args))
                  (apply fun args)))))

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

(defadvice edts-face-update-buffer-mode-line
    ;; Don't use edts way of indicating errors. It doesn't work well with
    ;; spaceline.
    (around edts-face-update-buffer-mode-line-disable last activate)
  (force-mode-line-update))

(when (file-accessible-directory-p erlang-emacs-dir)
  (add-to-list 'load-path erlang-emacs-dir)

  (require 'erlang-start)
  (setup-edts)

  (add-hook 'erlang-mode-hook 'global-hook-handler)
  (add-hook 'erlang-mode-hook
            (lambda ()
              (set (make-local-variable 'find-tag-default-function)
                   'my/erlang-get-thing-at-point)))
  (add-hook 'edts-mode-hook
            (lambda ()
              (define-key edts-mode-map (kbd "M-*") 'edts-find-source-unwind)
              (define-key edts-mode-map (kbd "C-c n") 'edts-code-next-issue)
              (define-key edts-mode-map (kbd "C-c p") 'edts-code-previous-issue))))

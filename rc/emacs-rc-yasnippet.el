(require 'yasnippet) ;; not yasnippet-bundle

(yas/initialize)

(setq yas/root-directory "~/emacs/snippets")
(setq yas/prompt-functions '(yas/ido-prompt))

(yas/load-directory yas/root-directory)

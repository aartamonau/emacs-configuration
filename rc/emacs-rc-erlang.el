(require 'erlang)

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

(add-hook 'erlang-mode-hook
          (lambda ()
            (set (make-local-variable 'find-tag-default-function)
                 'my/erlang-get-thing-at-point)))

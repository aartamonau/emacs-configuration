(add-hook 'shell-mode-hook
          (lambda ()
            ;; don't overwrite the history, bash already does everything
            ;; needed to preserve the history
            (setq comint-input-ring-file-name nil)))

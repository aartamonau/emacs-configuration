;; -*- lexical-binding: t; -*-
(require 'spaceline-config)
(require 'diminish)

(display-time-mode -1)

(setq spaceline-buffer-size-p nil)
(setq spaceline-minor-modes-p t)
(setq spaceline-hud-p nil)
(setq spaceline-buffer-encoding-abbrev-p nil)
(setq spaceline-selection-info-p nil)
(setq spaceline-line-column-p nil)
(spaceline-emacs-theme)

(defun my/do-diminish (mode to-what)
  (condition-case err
    (diminish mode to-what)

    (error (message "Diminish failed: %s" (cadr err)) t)))

(defun my/safe-diminish (mode &optional to-what)
  (let ((body (lambda ()
                (let ((sym (symbol-function mode)))
                  (cond ((autoloadp sym) (eval-after-load (nth 1 sym)
                                           '(my/do-diminish mode to-what)))
                        ((functionp sym) (my/do-diminish mode to-what))
                        (t (warn "Couldn't diminish %S to %S" mode to-what)))))))
    (if after-init-time
        (funcall body)
      (add-hook 'after-init-hook body))))

(my/safe-diminish 'global-whitespace-mode)
(my/safe-diminish 'eproject-mode)
(my/safe-diminish 'undo-tree-mode)
(my/safe-diminish 'guru-mode)
(my/safe-diminish 'subword-mode)
(my/safe-diminish 'ivy-mode)
(my/safe-diminish 'yas-minor-mode)
(my/safe-diminish 'auto-complete-mode)
(my/safe-diminish 'auto-highlight-symbol-mode)
(my/safe-diminish 'abbrev-mode)
(my/safe-diminish 'mml-mode)
(my/safe-diminish 'ggtags-mode)
(my/safe-diminish 'haskell-doc-mode)
(my/safe-diminish 'hi2-mode)
(my/safe-diminish 'hindent-mode)
(my/safe-diminish 'with-editor-mode)

(my/safe-diminish 'auto-revert-mode "AR")
(my/safe-diminish 'overwrite-mode "O")
(my/safe-diminish 'view-mode "V")

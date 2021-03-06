;; -*- lexical-binding: t; -*-
(require 'spaceline-config)
(require 'diminish)

(display-time-mode -1)

;; Segments to show edts' errors.
(defun my/edts-compile-issues-plist ()
  (let ((compile-issues (plist-get edts-code-buffer-issues 'edts-code-compile))
        (xref-issues (plist-get edts-code-buffer-issues 'edts-xref)))
    (list 'error (append (plist-get compile-issues 'error)
                         (plist-get xref-issues 'error))
          'warning (plist-get compile-issues 'warning))))

(defun my/spaceline-edts-lighter (type)
  (let* ((all-issues (my/edts-compile-issues-plist))
         (issues (plist-get all-issues type))
         (count (length issues))
         (errorp (> count 0)))
    (when errorp
      (format spaceline-flycheck-bullet count))))

(dolist (type '(error warning))
  (let ((segment-name (intern (format "edts-%S" type)))
        ;; just reuse spaceline's flycheck faces
        (face (intern (format "spaceline-flycheck-%S" type))))
    (eval
     `(spaceline-define-segment ,segment-name
        ,(format "Information about edts %Ss. Requires `edts-mode' to be enabled" type)
        (when (bound-and-true-p edts-mode)
          (let ((lighter (my/spaceline-edts-lighter ',type)))
            (when lighter
              (powerline-raw (s-trim lighter) ',face))))))))

(spaceline-define-segment major-mode-recursive
    "Show major mode bracketed if recursive edit is on."
    (format "%%[%s%%]" mode-name))

;; Just copied from spaceline-config.el with addition of edts-* segments and
;; higher priority for flycheck-*.
(setq my/spaceline-theme
      '(
        ;; left
        ((((((persp-name :fallback workspace-number)
             window-number) :separator "|")
           buffer-modified
           buffer-size)
          :face highlight-face
          :priority 0)
         auto-compile
         (org-pomodoro :when (and active
                                  (eq org-pomodoro-state :pomodoro))
                       :face org-pomodoro-segment-face)
         (org-pomodoro :when (and active
                                  (or (eq org-pomodoro-state :short-break)
                                      (eq org-pomodoro-state :long-break)))
                       :face org-pomodoro-break-segment-face)
         ((buffer-id remote-host)
          :priority 5)
         (which-function :face which-func-segment-face)
         ((buffer-encoding-abbrev
           point-position
           line-column)
          :separator " | "
          :priority 3)
         (buffer-position :pririty 0)
         major-mode-recursive
         (process :when active)
         ((flycheck-error flycheck-warning flycheck-info edts-error edts-warning)
          :when active)
         (minor-modes :when active)
         (mu4e-alert-segment :when active)
         (org-clock :when active))

        ;; right
        ((python-pyvenv :fallback python-pyenv)
         purpose
         input-method
         (global :when active))))

(setq spaceline-responsive nil)

(setq spaceline-buffer-size-p nil)
(setq spaceline-minor-modes-p t)
(setq spaceline-buffer-encoding-abbrev-p nil)

(defface org-pomodoro-segment-face
  '((t (:inverse-video t :inherit org-pomodoro-mode-line)))
  "Face used to display active pomodoro")

(defface org-pomodoro-break-segment-face
  '((t (:inverse-video t :inherit org-pomodoro-mode-line-break)))
  "Face used to display pomodoro during break")

(setq spaceline-org-clock-format-function
      (lambda ()
        (s-trim (org-clock-get-clock-string))))

(defface which-func-segment-face
  '((t (:inverse-video t :inherit which-func)))
  "Face for the which-func spaceline segment")

(set-face-attribute 'spaceline-highlight-face nil :inverse-video nil)
(setq powerline-default-separator 'box)

(apply 'spaceline-compile my/spaceline-theme)
(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))

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
(my/safe-diminish 'smerge-mode)
(my/safe-diminish 'hungry-delete-mode)

(my/safe-diminish 'auto-revert-mode "AR")
(my/safe-diminish 'overwrite-mode "O")
(my/safe-diminish 'view-mode "V")

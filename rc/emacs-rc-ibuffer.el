(global-set-key (kbd "C-x C-b") 'ibuffer)

(custom-set-variables
 '(ibuffer-saved-filter-groups
   (quote (("default"
            ("Org"     (mode . org-mode))
            ("Haskell" (mode . haskell-mode))
            ("C/C++"   (or (mode . c-mode)
                           (mode . c++-mode)))
            ("Python"  (mode . python-mode))
            ("Emacs"   (mode . emacs-lisp-mode))
            ("Grep"    (mode . grep-mode))))))
 '(ibuffer-sorting-mode (quote alphabetic))
 '(ibuffer-movement-cycle nil)
 '(ibuffer-expert t)
 '(ibuffer-show-empty-filter-groups nil))

(defun my/ibuffer-toggle-filter-group-by-name (name)
  "Toggle the display status of filter group by its name"
  (if (member name ibuffer-hidden-filter-groups)
        (setq ibuffer-hidden-filter-groups
              (delete name ibuffer-hidden-filter-groups))
      (push name ibuffer-hidden-filter-groups)))

(defun my/ibuffer-show-filter-group-by-name (name)
  "Show filter group by its name"
  (setq ibuffer-hidden-filter-groups
        (delete name ibuffer-hidden-filter-groups)))

(defun my/ibuffer-show-all-filter-groups ()
  "Show all filter groups"
  (interactive)
  (let ((groups (copy-list ibuffer-hidden-filter-groups)))
    (mapc 'my/ibuffer-show-filter-group-by-name groups)
    (ibuffer-update nil t)))

(defun my/ibuffer-hide-filter-group-by-name (name)
  "Hide filter group by its name"
  (unless (member name ibuffer-hidden-filter-groups)
    (push name ibuffer-hidden-filter-groups)))

(defun my/ibuffer-hide-all-filter-groups ()
  "Hide all filter groups"
  (interactive)
  (let* ((buffers (ibuffer-current-state-list))
         (noempty (not ibuffer-show-empty-filter-groups))
         (groups  (mapcar 'first
                         (ibuffer-generate-filter-groups buffers noempty))))
    (mapc 'my/ibuffer-hide-filter-group-by-name groups)
    (ibuffer-update nil t)))

(defun my/ibuffer-toggle-all-filter-groups ()
  "Toggle the display status of all filter groups"
  (interactive)
  (let* ((hidden  (length ibuffer-hidden-filter-groups))
         (noempty (not ibuffer-show-empty-filter-groups))
         (buffers (ibuffer-current-state-list))
         (visible (length (ibuffer-generate-filter-groups buffers noempty))))
    (if (> hidden visible)
        (my/ibuffer-show-all-filter-groups)
      (my/ibuffer-hide-all-filter-groups))))

(defun my/ibuffer-get-filter-group-name (&optional position)
  "Returns a name of filter group at point (either current point or supplied
via parameters)"
  (unless position
    (setq position (point)))
  (get-text-property position 'ibuffer-filter-group-name))

(defun my/ibuffer-previous-filter-group ()
  "Return a position of previous filter group or nil if it can't be found"
  (save-excursion
    (let ((current-position (point)))
      (if (my/ibuffer-get-filter-group-name current-position)
          (progn (ibuffer-backward-filter-group 0)
                 (point))
        (progn
          (ibuffer-backward-filter-group 1)
          (let ((group-position (point)))
            (when (and group-position
                       (<= group-position current-position))
              group-position)))))))

(defun my/ibuffer-toggle-filter-group-at-point ()
  "Toggles filter group visibility at point"
  (interactive)
  (let ((group-position (my/ibuffer-previous-filter-group)))
    (when group-position
      (ibuffer-toggle-filter-group-1 group-position)
      (goto-char group-position))))

(defun my/ibuffer-is-at-filter-group (&optional group-position)
  "Return `t' if position is at the line with filter group"
  (unless group-position
    (setq group-position (my/ibuffer-previous-filter-group)))
  (when group-position
    (= (line-beginning-position) group-position)))

(defun my/ibuffer-current-filter-group-name ()
  "Returns a name of filter group at point (even if point is at some buffer)"
  (let ((group-position (my/ibuffer-previous-filter-group)))
    (when (and group-position
               (or (ibuffer-current-buffer)
                   (= (line-beginning-position) group-position)))
      (my/ibuffer-get-filter-group-name group-position))))

(defun my/ibuffer-get-current-position-base ()
  "Returns a base element for current position."
  (cond ((my/ibuffer-is-at-filter-group)
         (list 'group
               (line-beginning-position)
               (my/ibuffer-current-filter-group-name)))
        ((ibuffer-current-buffer)
         (list 'buffer
               (line-beginning-position)
               (buffer-name (ibuffer-current-buffer))))
        (t (let ((current-line  (line-beginning-position))
                 (backward-line (save-excursion
                                  (ibuffer-backward-line)
                                  (point))))
             (if (>= backward-line current-line)
                 (list 'absolute 0 nil)
               (save-excursion
                 (end-of-buffer)
                 (list 'last (line-number-at-pos) nil)))))))

(defun my/ibuffer-get-current-position ()
  "Returns a context of current position"
  (let* ((base (my/ibuffer-get-current-position-base))
         (tag (car base))
         (base-position (cadr base))
         (datum (caddr base))
         (offset (if (eq tag 'last)
                     (- base-position (line-number-at-pos))
                   (- (point) base-position))))

    (list tag datum offset)))

(defun my/ibuffer-get-line-length ()
  "Returns a length of current line"
  (save-excursion
    (end-of-line)
    (current-column)))

(defun my/ibuffer-jump-to-position (position)
  "Jump to previously saved position"
  (flet ((jump-helper (offset)
                      (let* ((line-length (my/ibuffer-get-line-length))
                             (final-offset (if (> offset line-length)
                                               line-length
                                             offset)))
                        (forward-char offset))))

    (let ((tag    (car position))
          (datum  (cadr position))
          (offset (caddr position)))
      (cond ((eq tag 'absolute) (goto-char offset))
            ((eq tag 'last)
             (progn (goto-char (point-max))
                    (goto-line (- (line-number-at-pos)
                                  offset))))
            ((eq tag 'buffer)
             (progn (ibuffer-jump-to-buffer datum)
                    (jump-helper offset)))
            ((eq tag 'group)
             (progn (ibuffer-jump-to-filter-group datum)
                    (jump-helper offset)))))))

(defun my/ibuffer-setup-isearch-mode-hooks ()
  "Adds isearch hooks to show all filter groups when search is
  started and to restore the old state after search is
  done. Filter group at final point is kept visible in any case."
  (require 'cl)
  (lexical-let ((hidden nil))
    (add-hook 'isearch-mode-hook
              '(lambda ()
                 (let ((position (my/ibuffer-get-current-position)))
                   (setq hidden ibuffer-hidden-filter-groups)
                   (my/ibuffer-show-all-filter-groups)
                   (my/ibuffer-jump-to-position position)))
              nil t)

    (add-hook 'isearch-mode-end-hook
              '(lambda ()
                 (let* ((position (my/ibuffer-get-current-position))
                        (current-group (my/ibuffer-current-filter-group-name))
                        (new-hidden (if (my/ibuffer-is-at-filter-group)
                                        hidden
                                      (remove current-group hidden))))
                   (setq ibuffer-hidden-filter-groups new-hidden)
                   (ibuffer-update nil t)
                   (my/ibuffer-jump-to-position position)))
              nil t)))

(defun my/ibuffer-mode-hook ()
  (ibuffer-switch-to-saved-filter-groups "default")
  (my/ibuffer-hide-all-filter-groups)

  ;; key bindings
  (define-key
    ibuffer-mode-filter-group-map (kbd "<tab>")
    'ibuffer-toggle-filter-group)
  (define-key
    ibuffer-mode-filter-group-map (kbd "<backtab>")
    'my/ibuffer-toggle-all-filter-groups)
  (define-key
    ibuffer-mode-map (kbd "<tab>")
    'my/ibuffer-toggle-filter-group-at-point)
  (define-key
    ibuffer-mode-map (kbd "<backtab>")
    'my/ibuffer-toggle-all-filter-groups)

  ;; search hooks
  (my/ibuffer-setup-isearch-mode-hooks))

(defadvice ibuffer (around ibuffer-adjust-point activate)
  "If ibuffer is called for the first time then cursor is moved
  to the beginning of the buffer. Otherwise old position is restored."
  (let* ((ibuffer-buffer (get-buffer "*Ibuffer*"))
         (position (when ibuffer-buffer
                     (with-current-buffer ibuffer-buffer
                       (point)))))
    ad-do-it
    (if position
        (goto-char position)
      (goto-char (point-min)))))


(add-hook 'ibuffer-mode-hook 'my/ibuffer-mode-hook)

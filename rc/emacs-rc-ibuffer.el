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
    (message "hidden: %d, visible: %d" hidden visible)
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

(defun my/ibuffer-mode-hook ()
  (ibuffer-switch-to-saved-filter-groups "default")

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
    'my/ibuffer-toggle-all-filter-groups))

(add-hook 'ibuffer-mode-hook 'my/ibuffer-mode-hook)

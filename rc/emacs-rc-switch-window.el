(defconst switch-window-min-windows 4)

;; Custom version of switch window. The main thing that changed is the number
;; of windows required to enable visual switching. It was 3 and I made it
;; 4. Motivation is that for 3 windows I can reach any of them in no more than
;; one keystroke. And that's one less than the number of keystrokes required
;; by switch-window.
(defun aa/switch-window (count force-switch-mode)
  (if (and (not force-switch-mode)
           (< (length (window-list)) switch-window-min-windows))
      (other-window count)
    (progn
      (let ((index (prompt-for-selected-window "Move to window: ")))
        (apply-to-window-index 'select-window index "Moved to %S")))))

(global-set-key (kbd "C-x o")
                (lambda (force-switch-mode)
                  (interactive "P")
                  (aa/switch-window -1 force-switch-mode)))

(global-set-key (kbd "C-x O")
                (lambda (force-switch-mode)
                  (interactive "P")
                  (aa/switch-window 1 force-switch-mode)))

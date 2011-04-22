(defadvice toggle-read-only (after run-view-mode-on-read-only activate)
  "Activates view-mode in buffer if it is made read-only"
  (let ((toggle (if buffer-read-only 1 0)))
    (view-mode toggle)))

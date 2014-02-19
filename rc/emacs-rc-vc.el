(defadvice vc-ensure-vc-buffer (before vc-ensure-vc-buffer-indirect activate)
  (if (and (not buffer-file-name)
           (buffer-file-name (buffer-base-buffer)))
      (set-buffer (buffer-base-buffer))))

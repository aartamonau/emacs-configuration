(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-9"))

(load-theme 'zenburn)

(custom-set-faces
 '(diff-added ((t (:foreground "green3" :weight bold))))
 '(diff-removed ((t (:foreground "red3" :weight bold))))
 '(magit-item-highlight ((t (:background "#505050" :inherit nil)))))

(custom-set-variables
 ;; avoid splitting windows horizontally even on large screens
 '(split-height-threshold 10000))

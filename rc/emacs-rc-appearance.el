(when window-system 'x
      (set-face-attribute 'default nil :font "DejaVu Sans Mono-8"))

(load-theme 'zenburn)

(custom-set-faces
 '(diff-added ((t (:foreground "green3" :weight bold))))
 '(diff-removed ((t (:foreground "red3" :weight bold))))
 '(magit-item-highlight ((t (:background "#505050" :inherit nil)))))

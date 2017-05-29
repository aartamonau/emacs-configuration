(require 'pdf-tools)
(pdf-tools-install)

(setq pdf-view-display-size 1.0)
(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
(define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)

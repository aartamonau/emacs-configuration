(require 'narrow-indirect)

(define-key ctl-x-4-map "nd" 'ni-narrow-to-defun-indirect-other-window)
(define-key ctl-x-4-map "nn" 'ni-narrow-to-region-indirect-other-window)
(define-key ctl-x-4-map "np" 'ni-narrow-to-page-indirect-other-window)

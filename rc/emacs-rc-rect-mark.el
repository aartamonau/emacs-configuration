(load "rect-mark.el")

(define-key ctl-x-map (kbd "C-SPC") 'rm-set-mark)

(global-set-key (kbd "C-w")
  '(lambda(b e) (interactive "r")
     (if rm-mark-active
       (rm-kill-region b e) (kill-region b e))))

(global-set-key (kbd "M-w")
  '(lambda(b e) (interactive "r")
     (if rm-mark-active
       (rm-kill-ring-save b e) (kill-ring-save b e))))

(define-key ctl-x-map (kbd "C-x")
  '(lambda(&optional p) (interactive "p")
     (if rm-mark-active
       (rm-exchange-point-and-mark p) (exchange-point-and-mark p))))


;; (autoload 'rm-set-mark "rect-mark"
;;   "Set mark for rectangle." t)
;; (autoload 'rm-exchange-point-and-mark "rect-mark"
;;   "Exchange point and mark for rectangle." t)
;; (autoload 'rm-kill-region "rect-mark"
;;   "Kill a rectangular region and save it in the kill ring." t)
;; (autoload 'rm-kill-ring-save "rect-mark"
;;   "Copy a rectangular region to the kill ring." t)
;; (autoload 'rm-mouse-drag-region "rect-mark"
;;   "Drag out a rectangular region with the mouse." t)

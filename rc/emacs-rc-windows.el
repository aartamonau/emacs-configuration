(require 'winner)
(require 'shackle)

(setq winner-dont-bind-my-keys t)
(setq winner-ring-size 20)

(define-key winner-mode-map (kbd "C-c [") 'winner-undo)
(define-key winner-mode-map (kbd "C-c ]") 'winner-redo)

(winner-mode)

(setq shackle-rules
      '((compilation-mode :select nil :align below :size 0.3)
        ("\\*[Wo]*Man.*\\*" :regexp t :select t :other t)
        ("*undo-tree*" :regexp t :size 0.3 :align right)))

(shackle-mode)

;; Prefer splitting windows vertically.
(setq split-height-threshold nil)

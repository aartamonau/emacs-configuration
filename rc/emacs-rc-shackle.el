(require 'shackle)

(setq shackle-rules
      '((compilation-mode :select nil :align below :size 0.3)
        ("\\*[Wo]*Man.*\\*" :regexp t :select t :other t)
        ("*undo-tree*" :regexp t :size 0.3 :align right)))

(shackle-mode)

;; Prefer splitting windows vertically.
(setq split-height-threshold nil)

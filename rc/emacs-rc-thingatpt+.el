(eval-after-load "thingatpt"
  '(progn (require 'thingatpt+)
          (tap-redefine-std-fns)

          (setq tap-near-point-y-distance 0)
          (setq tap-near-point-x-distance 10)))

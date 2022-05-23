;; Save point in buffer
(save-place-mode 1)

(require 'persistent-scratch)
(setq persistent-scratch-autosave-interval 60)
(persistent-scratch-setup-default)

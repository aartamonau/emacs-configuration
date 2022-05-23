;; Save point in buffer
(save-place-mode 1)

(savehist-mode 1)
(setq savehist-additional-variables '(search-ring regexp-search-ring))

(require 'persistent-scratch)
(setq persistent-scratch-autosave-interval 60)
(persistent-scratch-setup-default)

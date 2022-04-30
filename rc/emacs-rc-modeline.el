(require 'doom-modeline)

(display-time-mode -1)

;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon nil)

;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
(setq doom-modeline-unicode-fallback nil)

;; Whether display the buffer name.
(setq doom-modeline-buffer-name t)

;; Whether display the minor modes in the mode-line.
(setq doom-modeline-minor-modes nil)

;; Whether display the buffer encoding.
(setq doom-modeline-buffer-encoding t)

;; If non nil the default perspective name is displayed in the mode-line.
(setq doom-modeline-display-default-persp-name nil)

;; If non nil the perspective name is displayed alongside a folder icon.
(setq doom-modeline-persp-icon nil)

;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(setq doom-modeline-lsp t)

;; Whether display the gnus notifications.
(setq doom-modeline-gnus nil)

;; Whether display the IRC notifications. It requires `circe' or `erc' package.
(setq doom-modeline-irc nil)

;; Whether display the environment version.
(setq doom-modeline-env-version nil)

(doom-modeline-mode 1)

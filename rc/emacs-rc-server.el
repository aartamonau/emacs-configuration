(defvar first-time t
  "Flag signifying this is the first time that .emacs has been evaled")

;; start emacs server on first run
(if first-time
    (server-start))

;; Indicate that this file has been read at least once
(setq first-time nil)
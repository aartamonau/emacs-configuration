(require 'popwin)

(popwin-mode 1)

(global-set-key (kbd "C-z") popwin:keymap)
(define-key Man-mode-map (kbd "o") 'ace-link-woman)

(setq popwin:popup-window-position 'bottom)
(setq popwin:popup-window-height 25)

;; https://github.com/m2ym/popwin-el/issues/131
(defadvice display-buffer (around display-buffer-prevent-popwin-split last activate)
  (let* ((buffer (ad-get-arg 0)))
    (if (and (string-equal (buffer-name buffer) "*Help*")
                (get-buffer-window buffer))
        (let ((display-buffer-alist nil))
          ad-do-it)
      ad-do-it)))

;; due to emacs not having a hook which is called on every mode startup
;; this must be added to all interested modes manually
(defun global-hook-handler ()
  ;; make it possible to hop into the center of studdlyCaps words
  (subword-mode 1))

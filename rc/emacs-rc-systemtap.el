(autoload 'systemtap-mode "systemtap-mode")
(add-to-list 'auto-mode-alist '("\\.stp\\'" . systemtap-mode))

(defadvice systemtap-mode (before systemtap-mode activate)
  (require 'cc-awk))

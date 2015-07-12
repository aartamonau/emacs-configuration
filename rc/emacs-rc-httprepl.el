(defun my/format-json-response (buffer)
  (shell-command-on-region
   (httprepl-find-headers-end (current-buffer))
   (point-max)
   "jq ."
   :replace t))

(custom-set-variables
 '(httprepl-content-type-alist
   '(("text/html"              . html)
     ("application/json"       . json)
     ("application/javascript" . js)
     ("text/xml"               . xml)
     ("text/plain"             . text)
     ("application/xml"        . xml)
     ("html"                   . html)
     ("json"                   . json)
     ("javascript"             . js)
     ("xml"                    . xml)
     ("text"                   . text)))

 '(httprepl-content-type-middleware-alist
   '((html . ((lambda (b) (html-mode) b)))
     (js   . ((lambda (b) (js-mode) b)))
     (xml  . ((lambda (b) (xml-mode) b)))
     (text . ((lambda (b) (text-mode) b)))
     (json . ((lambda (b)
                (js-mode)
                (my/format-json-response b)
                b))))))

(add-hook 'httprepl-mode-hook #'turn-on-comint-history)

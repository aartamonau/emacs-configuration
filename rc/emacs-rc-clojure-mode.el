(autoload 'clojure-mode "clojure-mode"
  "Clojure mode." t)

(add-to-list 'auto-mode-alist '("\\.clj" . clojure-mode))

(require 'swank-clojure)

(setq swank-clojure-classpath
      (append '("/usr/share/clojure/clojure.jar"
                "/usr/share/clojure/clojure-contrib.jar")
              (swank-clojure-default-classpath)))

(autoload 'swank-clojure-init "swank-clojure" "Initialize clojure for swank")
(autoload 'swank-clojure-cmd "swank-clojure" "Command to start clojure")

(add-to-list 'slime-lisp-implementations
             `(clojure ,(swank-clojure-cmd) :init swank-clojure-init) t)

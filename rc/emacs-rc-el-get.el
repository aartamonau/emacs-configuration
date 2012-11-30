(add-extension-load-path "el-get")

(require 'el-get)

(setq el-get-sources
      '((:name systemtap-mode
               :type http
               :url "http://coderepos.org/share/export/39195/lang/elisp/systemtap-mode/systemtap-mode.el"
               :compile "systemtap-mode.el")
        (:name el-get
               :type http
               :url "http://www.dr-qubit.org/undo-tree/undo-tree.el"
               :compile "undo-tree")))

(setq aa/packages
      '(package
        org-mode
        auctex
        ido-ubiquitous smex
        slime
        quack
        clojure-mode
        js2-mode
        ;; seems to be incompatible with emacs24
        ;; sml-mode
        haskell-mode haskell-mode-exts
        markdown-mode
        doxymacs
        muse
        yasnippet
        django-mode
        rect-mark
        cmake-mode
        pkgbuild-mode
        zenburn-theme
        switch-window
        ace-jump-mode
        workgroups
        magit
        magithub
        ledger-mode
        eproject
        dired+))

(setq aa/all-packages
      (append aa/packages
              (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync aa/all-packages)

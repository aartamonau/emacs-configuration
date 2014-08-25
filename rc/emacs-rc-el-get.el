(add-extension-load-path "el-get")

(require 'el-get)

(setq el-get-sources
      '((:name etags-select
               :type github
               :branch "master"
               :pkgname "aartamonau/etags-select"
               :compile "etags-select.el")
        (:name wide-n
               :type emacswiki)
        (:name keyfreq
               :description "Track Emacs commands frequency"
               :type github
               :branch "master"
               :pkgname "dacap/keyfreq")))

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
        haskell-mode haskell-mode-exts structured-haskell-mode
        markdown-mode
        doxymacs
        muse
        yasnippet
        django-mode
        rect-mark
        cmake-mode
        pkgbuild-mode
        color-theme-zenburn
        switch-window
        ace-jump-mode
        workgroups
        magit
        ledger-mode
        eproject
        dired+
        ghc-mod
        emacs-w3m
        evil-numbers
        go-mode
        rust-mode
        rusti
        guru-mode
        hi2
        magit-view-file
        vkill
        man-preview
        llvm-mode
        indirect-region
        diff-hl
        wtf))

(setq aa/all-packages
      (append aa/packages
              (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync aa/all-packages)

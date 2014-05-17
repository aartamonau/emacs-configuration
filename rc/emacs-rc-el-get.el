(add-extension-load-path "el-get")

(require 'el-get)

(setq el-get-sources
      '((:name systemtap-mode
               :type http
               :url "http://coderepos.org/share/export/39195/lang/elisp/systemtap-mode/systemtap-mode.el"
               :compile "systemtap-mode.el")
        (:name undo-tree
               :type http
               :url "http://www.dr-qubit.org/undo-tree/undo-tree.el"
               :compile "undo-tree")
        (:name tomatinho
               :description "Pomodoro technique timer with a lean and usable interface."
               :type github
               :branch "master"
               :pkgname "konr/tomatinho")
        (:name etags-select
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
        haskell-mode haskell-mode-exts
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
        vkill))

(setq aa/all-packages
      (append aa/packages
              (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync aa/all-packages)

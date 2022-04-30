(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(setq el-get-sources
      '((:name unbound
               :type emacswiki :features (unbound))

        (:name persistent-scratch
               :type elpa)

        (:name magit
               :type elpa)))

(setq aa/packages
      '(package
        org-mode
        auctex
        haskell-mode
        flycheck
        company
        lsp-mode
        lsp-haskell
        markdown-mode
        solarized-emacs
        color-theme-solarized
        magit
        browse-at-remote
        eproject
        dired+
        dired-hacks
        go-mode
        go-lint
        go-imports
        rust-mode
        rusti
        guru-mode
        hi2
        magit-view-file
        man-preview
        diff-hl
        wtf
        protobuf-mode
        ggtags
        yaml-mode
        expand-region
        yagist
        narrow-indirect
        grep-o-matic
        scratch
        swiper
        avy
        ace-window
        ace-link
        shackle
        zones
        spaceline
        diminish
        thingatpt+
        hungry-delete
        ivy-xref
        undo-tree))

(setq aa/all-packages
      (append aa/packages
              (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync aa/all-packages)
(package-initialize)

(custom-set-variables
 '(el-get-verbose t))

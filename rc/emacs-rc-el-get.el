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
        go-lint
        go-flymake
        go-oracle
        go-imports
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
        wtf
        protobuf-mode
        idris-mode))

(setq aa/all-packages
      (append aa/packages
              (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync aa/all-packages)

;; Work around epg issue with gnupg 2.1.0. I put it in here for greater chance
;; for me to remember to remove this when upstream fix arrives.
;; See https://bbs.archlinux.org/viewtopic.php?id=190497 for details.
(defun epg--list-keys-1 (context name mode)
  (let ((args (append (if (epg-context-home-directory context)
                          (list "--homedir"
                                (epg-context-home-directory context)))
                      '("--with-colons" "--no-greeting" "--batch"
                        "--with-fingerprint" "--with-fingerprint")
                      (unless (eq (epg-context-protocol context) 'CMS)
                        '("--fixed-list-mode"))))
        (list-keys-option (if (memq mode '(t secret))
                              "--list-secret-keys"
                            (if (memq mode '(nil public))
                                "--list-keys"
                              "--list-sigs")))
        (coding-system-for-read 'binary)
        keys string field index)
    (if name
        (progn
          (unless (listp name)
            (setq name (list name)))
          (while name
            (setq args (append args (list list-keys-option (car name)))
                  name (cdr name))))
      (setq args (append args (list list-keys-option))))
    (with-temp-buffer
      (apply #'call-process
             (epg-context-program context)
             nil (list t nil) nil args)
      (goto-char (point-min))
      (while (re-search-forward "^[a-z][a-z][a-z]:.*" nil t)
        (setq keys (cons (make-vector 15 nil) keys)
              string (match-string 0)
              index 0
              field 0)
        (while (and (< field (length (car keys)))
                    (eq index
                        (string-match "\\([^:]+\\)?:" string index)))
          (setq index (match-end 0))
          (aset (car keys) field (match-string 1 string))
          (setq field (1+ field))))
      (nreverse keys))))

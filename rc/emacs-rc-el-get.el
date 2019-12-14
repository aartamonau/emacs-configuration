(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(setq el-get-sources
      '((:name etags-select
               :type github
               :branch "master"
               :pkgname "aartamonau/etags-select"
               :compile "etags-select.el")
        (:name unbound
               :type emacswiki :features (unbound))
        (:name keyfreq
               :description "Track Emacs commands frequency"
               :type github
               :branch "master"
               :pkgname "dacap/keyfreq")
        (:name edts
               :type elpa)
        (:name eclipse-mode
               :type http-tar
               :options ("xzf")
               :url "http://eclipseclp.org/Distribution/pub/eclipse_emacs.tgz")
        (:name annotate
               :type github
               :branch "master"
               :pkgname "bastibe/annotate.el"
               :compile "annotate.el")

        ;; this requires hidnent to be installed and in the PATH
        (:name hindent
               :type github
               :branch "master"
               :pkgname "chrisdone/hindent"
               :load-path "elisp")

        (:name persistent-scratch
               :type elpa)

        (:name ormolu
               :type github
               :branch "master"
               :pkgname "vyorkin/ormolu.el"
               :compile "ormolu.el")))

(setq aa/packages
      '(package
        org-mode
        org-pomodoro
        auctex
        slime
        quack
        clojure-mode
        js2-mode
        haskell-mode
        markdown-mode
        muse
        yasnippet
        rect-mark
        cmake-mode
        pkgbuild-mode
        solarized-emacs
        color-theme-solarized
        magit
        ledger-mode
        eproject
        dired+
        dired-hacks
        emacs-w3m
        evil-numbers
        go-mode
        go-lint
        go-flymake
        go-imports
        rust-mode
        rusti
        guru-mode
        hi2
        magit-view-file
        vkill
        man-preview
        llvm-mode
        diff-hl
        wtf
        protobuf-mode
        idris-mode
        ggtags
        yaml-mode
        flymake
        expand-region
        change-inner
        edts
        eclipse-mode
        annotate
        yagist
        hindent
        ormolu
        narrow-indirect
        httprepl
        grep-o-matic
        scratch
        swiper
        avy
        ace-window
        ace-link
        shackle
        skewer-mode
        ein
        pdf-tools
        zones
        spaceline
        diminish
        thingatpt+
        hungry-delete))

(setq aa/all-packages
      (append aa/packages
              (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync aa/all-packages)
(package-initialize)

(custom-set-variables
 '(el-get-verbose t))

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

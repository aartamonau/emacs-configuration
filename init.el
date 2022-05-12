;; custom custom-file :)
(setq custom-file "~/emacs/custom.el")
(load custom-file 'noerror)

;; no disabled commands for novice (does not clobber .emacs file)
(setq disabled-command-function nil)

(setq load-path
      (append load-path '("~/emacs/rc")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; el-get ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
        lsp-mode
        lsp-haskell
        markdown-mode
        doom-themes
        doom-modeline
        magit
        browse-at-remote
        eproject
        dired+
        dired-hacks
        go-mode
        go-lint
        go-imports
        guru-mode
        magit-view-file
        man-preview
        diff-hl
        wtf
        ggtags
        yaml-mode
        expand-region
        yagist
        grep-o-matic
        scratch
        swiper
        avy
        ace-window
        ace-link
        shackle
        thingatpt+
        hungry-delete
        ivy-xref
        undo-tree
        erlang
        use-package
        line-comment-banner
        flyspell-correct-ivy
        ))

(setq aa/all-packages
      (append aa/packages
              (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync aa/all-packages)
(package-initialize)

(custom-set-variables
 '(el-get-verbose t))

;;;;;;;;;;;;;;;;;;;;;;; general configuration ;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :custom
  ;; Prefer splitting windows vertically.
  (split-height-threshold nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tramp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tramp
  :bind (("C-c o" . sudo-find-file)
         ("C-c s" . sudo-reopen-file))

  :init
  (defun sudo-file-name (filename)
    (concat "/sudo::" filename))

  (defun sudo-find-file (filename &optional wildcards)
    "Calls find-file with filename with sudo-tramp-prefix prepended"
    (interactive "fFind file with sudo ")
    (let ((sudo-name (sudo-file-name filename)))
      (apply 'find-file
             (cons sudo-name (if (boundp 'wildcards) '(wildcards))))))

  (defun sudo-reopen-file ()
    "Reopen file as root by prefixing its name with
sudo-tramp-prefix and by clearing buffer-read-only"
    (interactive)
    (let ((file-name (expand-file-name (or buffer-file-name
                                           list-buffers-directory)))
          (buffer (current-buffer)))
      (sudo-find-file file-name)
      (kill-buffer buffer))))

;;;;;;;;;;;;;;;;;;;;;;;; which-function-mode ;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-func
  :demand t
  :custom
  (which-func-modes '(emacs-lisp-mode
                      c-mode
                      c++-mode
                      python-mode
                      makefile-mode
                      sh-mode
                      diff-mode
                      erlang-mode
                      haskell-mode))
  :config
  (which-function-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;; hippie expand ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hippie-exp
  :bind ("M-/" . hippie-expand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dired
  :custom
  (dired-listing-switches "-alF")
  (dired-recursive-deletes 'always))

(use-package dired+
  :after dired
  :demand t
  :custom
  (diredp-hide-details-initially-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

;;;;;;;;;;;;;;;;;;;;;;;;; window management ;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package winner
  :demand t
  :custom
  (winner-dont-bind-my-keys t)
  (winner-ring-size 20)
  :bind (:map winner-mode-map
              ("C-c [" . winner-undo)
              ("C-c ]" . winner-redo))
  :config
  (winner-mode))

(use-package shackle
  :demand t
  :custom
  (shackle-rules '((compilation-mode :select nil :other t)
                   ("\\*[Wo]*Man.*\\*" :regexp t :select t :other t)
                   ("\\*godoc .*\\*" :regexp t :select t :other t)
                   ("*undo-tree*" :regexp t :size 0.3 :align right)))
  :config
  (shackle-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;; banner comments ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package line-comment-banner
  :bind (("C-;" . (lambda (&optional width)
                    (interactive "p")
                    (unless (> width 1)
                      ;; default to 70 column fill
                      (setf width 70))
                    (line-comment-banner width)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; expand-region ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :bind (("C-'" . er/expand-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; spell-checking ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (unbind-key "C-;" flyspell-mode-map)
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-M-i" flyspell-mode-map)
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :custom
  (flyspell-correct-interface #'flyspell-correct-ivy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; undo-tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :demand t
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; grep-o-matic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package grep-o-matic
  :demand t
  :config
  ;; not using :custom because it doesn't allow updating default values
  (setq grep-find-ignored-directories
        (cons ".eunit" grep-find-ignored-directories))
  (setq grep-o-matic-search-patterns
        (append '("*.[he]rl" "*.hs" "*.cmake" "CMakeLists" "*.bash" "*.rb")
                grep-o-matic-search-patterns))
  (setq grep-o-matic-ask-about-save nil)

  :bind (:map grep-o-matic-map
              ("M-j" . grep-o-matic-repository)
              ("M-k" . grep-o-matic-current-directory)
              ("M-l" . grep-o-matic-visited-files)

              ;; the following variations prompt to confirm the pattern
              ("M-J" . (lambda nil
                         (interactive)
                         (grep-o-matic-repository t)))
              ("M-K" . (lambda nil
                         (interactive)
                         (grep-o-matic-current-directory t)))
              ("M-L" . (lambda nil
                         (interactive)
                         (grep-o-matic-visited-files t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; compilation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package compile
  :after eproject
  :init
  (defun makefile-compile ()
    (let* ((root (eproject-root))
           (makefile (concat (file-name-as-directory root) "Makefile")))
      (when (file-readable-p makefile)
        (set (make-local-variable 'compile-command)
             (format "make -k -C '%s'" root)))))
  :bind ("C-c c" . compile)
  :hook (generic-git-project-file-visit . makefile-compile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; view mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package view
  :after emacs
  :demand t
  :config

  (defun my/maybe-view-mode (&rest args)
    (let ((major-mode-name (symbol-name major-mode)))
      ;; don't activate view-mode in magit buffers
      (unless (or (string-prefix-p "magit-" major-mode-name)
                  (memq major-mode
                        '(rebase-mode git-rebase-mode doc-view-mode pdf-view-mode)))
        (apply 'view-mode args))))

  (defadvice toggle-read-only (after run-view-mode-on-read-only activate)
    "Activates view-mode in buffer if it is made read-only"
    (let ((toggle (if buffer-read-only 1 0)))
      (my/maybe-view-mode toggle)))

  :hook (find-file . (lambda nil
                       "Activates view-mode if buffer is read-only"
                       (when buffer-read-only
                         (my/maybe-view-mode))))
  :bind (:map view-mode-map
              ("j" . next-line)
              ("k" . previous-line)
              ("h" . backward-char)
              ("l" . forward-char)

              ("G" . (lambda ()
                       (interactive)
                       (View-goto-percent 100)))

              ("q" . delete-window)
              ("Q" . kill-buffer-and-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; whitespace ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package whitespace
  :demand t
  :config
  (global-whitespace-mode 1)
  :custom
  (whitespace-global-modes '(not org-mode
                                 dired-mode
                                 magit-status-mode
                                 magit-diff-mode
                                 magit-revision-mode
                                 magit-stash-mode))
  (whitespace-style '(face tabs trailing lines-tail empty tab-mark))
  (whitespace-line-column 100))

;;;;;;;;;;;;;;;;;;;;;;;;;; show-paren-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package paren
  :demand t
  :config
  (show-paren-mode 1)
  :custom
  (show-paren-style 'mixed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; uniquify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package uniquify
  :demand t
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; linum ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package linum
  :demand t
  :config
  (global-linum-mode 1)

  (defconst linum-mode-excludes '(doc-view-mode org-mode pdf-view-mode Man-mode)
    "List of major modes preventing linum to be enabled in the buffer.")

  (defadvice linum-mode (around linum-mode-selective activate)
    "Avoids enabling of linum-mode in the buffer having major mode set to one
of listed in `linum-mode-excludes'."
    (unless (member major-mode linum-mode-excludes)
      ad-do-it)))

;; must be loaded after custom file
(load "~/emacs/rc.el")

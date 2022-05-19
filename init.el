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
  :demand t
  :custom
  ;; Prefer splitting windows vertically.
  (split-height-threshold nil)
  ;; Prevent the startup message
  (inhibit-startup-message t)
  ;; Disable tool-bar
  (tool-bar-mode nil)
  ;; Disable menu-bar (but not on darwin, where it exposes a bug)
  (menu-bar-mode (eq system-type 'darwin))
  ;; Display column number in status bar
  (column-number-mode t)
  ;; Disable the scroll bar
  (scroll-bar-mode nil)
  ;; Scrolling
  (scroll-conservatively 50)
  (scroll-preserve-screen-position t)
  (scroll-margin 10)
  ;; don't use GUI pop-ups
  (use-dialog-box nil)
  ;; don't highlight the region between the point and the mark by default
  (transient-mark-mode nil)
  ;; don't use tabs for indentation
  (indent-tabs-mode nil)
  ;; don't show a cursor in non-selected windows
  (cursor-in-non-selected-windows nil)
  ;; add a newline at the end of files
  (require-final-newline t)
  ;; don't auto-revert buffers
  (global-auto-revert-mode nil)
  ;; don't auto-indent on pressing enter
  (electric-indent-mode nil)

  ;; auto-insert matching parentheses, etc.
  (electric-pair-mode t)
  ;; don't insert a matching symbol next to words
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

  ;; don’t ask whether to rever TAGS file
  (tags-revert-without-query t)
  ;; don’t "ring the bell"
  (ring-bell-function 'ignore)

  ;; in programming modes, auto-fill only comments
  (comment-auto-fill-only-comments t)
  ;; default fill column
  (fill-column 78)

  :config
  ;; set default font
  (set-face-attribute 'default nil :font "Monaco-11")
  ;; type "y"/"n" instead of "yes"/"no"
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; unbind suspend-frame unless running in the terminal
  (unless (controlling-tty-p)
    (unbind-key "C-z" global-map)
    (unbind-key "C-x C-z" global-map))

  ;; allow using Option as Meta on macos
  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'meta))

  ;; disable expensive modes on large files
  (defvar my/expensive-modes-disabled nil)

  (defun disable-expensive-modes ()
    (interactive)
    (fundamental-mode)
    (font-lock-mode 0)
    (linum-mode 0)
    (flyspell-mode 0)
    (auto-fill-mode 0)
    (undo-tree-mode -1)
    (toggle-truncate-lines 1)
    (local-set-key (kbd "C-s") 'isearch-forward)
    (local-set-key (kbd "C-r") 'isearch-backward)
    (setq-local my/expensive-modes-disabled t))

  (add-hook 'find-file-hook
            (lambda ()
              (let ((file-name (buffer-file-name (current-buffer))))
                (when file-name
                  (let* ((attributes (file-attributes file-name))
                         (size (nth 7 attributes))
                         (do-disable (and size (> size 5000000))))
                    (when do-disable
                      (message "Disabling expensive modes for `%s'" file-name)
                      (disable-expensive-modes)))))))

  (defadvice revert-buffer (around revert-buffer-disable-expensive last activate)
    (let ((was-disabled my/expensive-modes-disabled))
      ad-do-it
      (when was-disabled
        (disable-expensive-modes))))

  :init
  (defun my/linum-reset ()
    (when linum-mode
      (linum-delete-overlays)))

  :hook (text-mode . auto-fill-mode)

  :bind (;; kill current buffer (without a prompt)
         ("C-x k" . (lambda ()
                      (interactive)
                      (kill-buffer (current-buffer))))

         ;; open a man page
         ("C-h M" . man)

         ;; zoom in
         ("C-+" . (lambda ()
                    (interactive)
                    (text-scale-increase 1)
                    (my/linum-reset)))
         ;; zoom out
         ("C--" . (lambda ()
                    (interactive)
                    (text-scale-decrease 1)
                    (my/linum-reset)))
         ;; reset zoom
         ("C-=" . (lambda ()
                    (interactive)
                    (text-scale-set 0)
                    (my/linum-reset)))))

(use-package calendar
  :custom
  (calendar-week-start-day 1)
  (calendar-today-visible-hook 'calendar-mark-today)
  (calendar-today-marker 'holiday))

(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-gruvbox))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mode-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-modeline
  :demand t
  :custom
  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (doom-modeline-icon nil)

  ;; If non nil the perspective name is displayed alongside a folder icon.
  (doom-modeline-persp-icon nil)

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (doom-modeline-irc nil)

  ;; Whether display the environment version.
  (doom-modeline-env-version nil)

  :config
  (doom-modeline-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;; hungry delete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hungry-delete
  :demand t
  :custom
  (hungry-delete-chars-to-skip " \t\f\v")
  :config
  (global-hungry-delete-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; guru-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package guru-mode
  :demand t
  :config
  (guru-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ediff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ediff
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :custom
  (flycheck-standard-error-navigation nil)
  :custom-face
  ;; make errors more visible
  (flycheck-error ((t (:background "#500000" :underline nil))))
  (flycheck-info ((t (:background "#005000" :underline nil))))
  (flycheck-warning ((t (:background "#d04000" :underline nil))))
  :bind (("C-c ?" . flycheck-list-errors)
         ("C-c n" . flycheck-next-error)
         ("C-c p" . flycheck-previous-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; avy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :custom
  (avy-style 'de-bruijn)
  (avy-background t)
  (avy-timeout-seconds 0.3)
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-subword-extra-word-chars nil)
  :bind (("C-c j" . avy-goto-subword-1)
         ("C-c l" . avy-goto-line)
         ("C-c C-j" . avy-goto-subword-1)
         ("C-c C-l" . avy-goto-line)
         ("C-c C-w" . avy-kill-region)
         ("C-c M-w" . avy-kill-ring-save-region)
         ("C-c m" . avy-move-region)
         ("C-c M" . avy-move-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ace-window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ace-window
  :custom
  (aw-dispatch-always nil)
  (aw-scope 'frame)
  :bind (("C-x o" . ace-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ace-link ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ace-link
  :demand t
  :bind (("M-o" . ace-link-addr)
         ;; open a link visible in other window
         ("C-x 4 o" . my/ace-link-other-window))
  :config
  (defun my/other-window ()
    (interactive)
    (let ((window (selected-window)))
      (if (fboundp 'ace-window)
          (ace-window 0)
        (other-window 0))
      (when (not (eq window (selected-window)))
        window)))

  (defun my/ace-link-other-window ()
    (interactive)
    (let ((window (my/other-window)))
      (when window
        (condition-case err
            (ace-link)
          (error
           (select-window window)
           (signal (car err) (cdr err)))))))

  (ace-link-setup-default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ggtags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ggtags
  :hook (c-mode-common . ggtags-mode)
  :bind (:map ggtags-navigation-map
              ("M->" . nil)
              ("M-<" . nil)
              ("C-M-s" . nil)
              ("C-c M-<" . fisrst-error)
              ("C-c M->" . ggtags-navigation-last-error)
         :map ggtags-mode-map
              ;; interferes with grep-o-matic
              ("M-]" . nil)))

;; must be loaded after custom file
(load "~/emacs/rc.el")

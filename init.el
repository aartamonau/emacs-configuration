;; don't write custom settings to init.el
(setq custom-file
      (concat (file-name-as-directory user-emacs-directory)
              "custom.el"))
(load custom-file 'noerror)

;; no disabled commands for novice (does not clobber .emacs file)
(setq disabled-command-function nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; straight ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq my/packages
      '(unbound
        org
        org-contrib
        haskell-mode
        flycheck
        lsp-mode
        lsp-haskell
        markdown-mode
        doom-themes
        doom-modeline
        magit
        git-link
        eproject
        dired+
        dired-hacks
        go-mode
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
        ivy
        counsel
        swiper
        avy
        ace-window
        ace-link
        shackle
        hungry-delete
        ivy-xref
        undo-tree
        erlang
        use-package
        line-comment-banner
        flyspell-correct-ivy
        company
        ibuffer-vc
        yasnippet
        git-commit
        persistent-scratch))

(dolist (package my/packages)
  (straight-use-package package))

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

  ;; don’t ask whether to rever TAGS file
  (tags-revert-without-query t)
  ;; don’t "ring the bell"
  (ring-bell-function 'ignore)

  ;; in programming modes, auto-fill only comments
  (comment-auto-fill-only-comments t)
  ;; default fill column
  (fill-column 78)

  ;; enable subword-mode globally
  (global-subword-mode t)

  ;; lsp recommends these
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024))

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
                    (my/linum-reset)))

         ;; create a scratch buffer
         ("C-c C-s" . scratch)

         :map indent-rigidly-map
         ("j" . indent-rigidly-left)
         ("k" . indent-rigidly-right)
         ("J" . indent-rigidly-left-to-tab-stop)
         ("K" . indent-rigidly-right-to-tab-stop)
         ("RET" . keyboard-quit)))

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
  ;; I prefer to get the pop-up by pressing "C-c ?"
  (flycheck-display-errors-delay 100000)
  :custom-face
  ;; make errors more visible
  (flycheck-error ((t (:background "#500000" :underline nil))))
  (flycheck-info ((t (:background "#005000" :underline nil))))
  (flycheck-warning ((t (:background "#d04000" :underline nil))))
  :bind (("C-c ?" . flycheck-display-error-at-point)
         ("C-c n" . flycheck-next-error)
         ("C-c p" . flycheck-previous-error)
         ("C-g"
          . (lambda ()
              (interactive)
              ;; hide flycheck error buffer on pressing "C-g"
              (let* ((buffer (flycheck-error-message-buffer))
                     (window (when buffer (get-buffer-window buffer))))
                (when window
                  (save-selected-window
                    (quit-window nil window)))
                (keyboard-quit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; avy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :custom
  (avy-style 'pre)
  (avy-background t)
  (avy-timeout-seconds 0.3)
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-subword-extra-word-chars nil)
  :bind (("C-c j" . avy-goto-subword-1)
         ("C-c l" . avy-goto-line)
         ("C-c k" . avy-goto-char)
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

;;;;;;;;;;;;;;;;;;;;;;;;; swiper/ivy/counsel ;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package isearch
  :demand t
  :bind (;; extra bindings when swiper is in use
         ("M-s s" . isearch-forward)
         ("M-s r" . isearch-backward)

         ;; make isearch a bit more like ivy/swiper
         :map isearch-mode-map
         ("C-n" . isearch-repeat-forward)
         ("C-p" . isearch-repeat-backward)
         ("M-j" . isearch-yank-word-or-char)))

(use-package ivy
  :demand t
  :custom
  (ivy-use-virtual-buffers nil)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist '((org-refile . "^")
                              (org-agenda-refile . "^")
                              (org-capture-refile . "^")
                              (counsel-M-x . "^")
                              (counsel-describe-function . "")
                              (counsel-describe-variable . "")
                              (man . "^")
                              (woman . "^")))
  :config
  (ivy-mode 1)
  :bind (("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("M-r" . ivy-restrict-to-matches)
         ("C-r" . ivy-previous-line)
         ("C-s" . ivy-next-line)))

(use-package ivy-xref
  :demand t
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h S" . counsel-info-lookup-symbol)
         ("M-y" . counsel-yank-pop)
         ("C-x j" . counsel-file-jump)
         ("C-x C-j" . counsel-dired-jump)
         ("C-x C-," . counsel-mark-ring)
         ("C-c i" . counsel-imenu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; diff-hl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diff-hl
  :demand t
  :bind (:map diff-hl-mode-map
              ("C-x v n" . diff-hl-next-hunk)
              ("C-x v p" . diff-hl-previous-hunk)
              ("C-x v k" . diff-hl-revert-hunk)
              ("C-x v RET" . diff-hl-diff-goto-hunk))
  :config
  (global-diff-hl-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yagist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yagist
  :custom
  (yagist-view-gist t)
  :commands (yagist-region
             yagist-region-private
             yagist-buffer
             yagist-buffer-private
             yagist-list))

;;;;;;;;;;;;;;;;;;;;;;;;;; browse-at-remote ;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-link
  :bind (("C-c g g" . git-link)
         ("C-c g c" . git-link-commit))
  :custom
  (git-link-open-in-browser t)
  (git-link-use-commit t)

  :init
  (defun crowdstrike-git-link (hostname dirname filename branch commit start end)
    (-let [(repo project . _) (reverse (s-split "/" dirname))]
      (format "https://%s/projects/%s/repos/%s/browse/%s%s?at=%s"
              hostname
              project
              repo
              filename
              (if start (if end (format "#%s-%s" start end) (format "#%s" start)) "")
              (or branch commit))))

  (defun crowdstrike-git-link-commit (hostname dirname commit)
    (-let [(repo project . _) (reverse (s-split "/" dirname))]
      (format "https://%s/projects/%s/repos/%s/commits/%s"
              hostname
              project
              repo
              commit)))

  (defun crowdstrike-git-link-homepage (hostname dirname)
    (-let [(repo project . _) (reverse (s-split "/" dirname))]
      (format "https://%s/projects/%s/repos/%s/browse"
              hostname
              project
              repo)))

  :config
  (push '("bitbucket.cicd.dc" crowdstrike-git-link) git-link-remote-alist)
  (push '("bitbucket.cicd.dc" crowdstrike-git-link-commit) git-link-commit-remote-alist)
  (push '("bitbucket.cicd.dc" crowdstrike-git-link-homepage) git-link-homepage-remote-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vc
  :demand t
  :custom
  ;; ignore whitespace in git blame
  (vc-git-annotate-switches '("-w")))

(use-package git-commit
  :demand t
  :custom
  (git-commit-summary-max-length 65)
  (git-commit-style-convention-checks '(non-empty-second-line
                                        overlong-summary-line))
  :hook (git-commit-setup . (lambda ()
                              (setq fill-column 70)
                              (setq-local whitespace-line-column 70))))

(use-package magit-view-file
  :bind (("C-x v H" . magit-view-file-history))
  :config
  ;; reuse buffers
  (defadvice magit-view-file-at-commit (around
                                        magit-view-file-at-commit-reuse-buffer
                                        activate)
    (flet ((generate-new-buffer (name)
                                (if (get-buffer name)
                                    (kill-buffer name))
                                (get-buffer-create name)))
      ad-do-it)))

(use-package magit
  :after ivy
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :bind (("C-x g" . magit-status)
         :map magit-log-mode-map
         ("j" . magit-section-forward)
         ("k" . magit-section-backward)
         :map magit-mode-map
         ("C-c C-c" . magit-commit)
         ("C-c C-a" . magit-commit-amend)
         ("R" . magit-rebase-interactive)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cc-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :hook (c-mode-common
         . (lambda ()
             (c-set-style "k&r")
             (setq c-basic-offset 4)
             (c-set-offset 'inextern-lang 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; erlang ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package erlang
  :config
  (defun my/erlang-get-thing-at-point ()
    (interactive)
    (my/identifier-to-string (erlang-get-identifier-at-point)))

  (defun my/identifier-to-string (identifier)
    (pcase identifier
      (`(qualified-function ,module ,name ,arity)
       (if module
           (format "%s:%s" module name)
         name))
      (`(record ,module ,name ,arity) (format "#%s" name))
      (`(module ,module ,name ,arity) (format "%s:" name))
      (`(macro ,module ,name ,arity) (format "?%s" name))
      (`(nil ,module ,name ,arity) name)))

  :hook (erlang-mode
         . (lambda ()
             ;; better default search item for grep-o-matic
             (set (make-local-variable 'find-tag-default-function)
                  'my/erlang-get-thing-at-point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; company ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :demand t
  :custom
  (company-idle-delay nil)
  (company-global-modes '(emacs-lisp-mode lisp-interaction-mode))
  :bind (:map company-mode-map
              ("M-TAB" . 'company-complete))
  :config
  (global-company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp
  :commands (lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-.")
  (lsp-restart 'ignore)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-code-actions-enable nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; desktop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package desktop
  :custom
  (desktop-save-mode t)
  (desktop-restore-eager 20)
  (desktop-auto-save-timeout 10)
  :config

  (defun desktop-force-read ()
    "Read locked desktop file"
    (interactive)
    (let ((desktop-load-locked-desktop t))
      (desktop-read))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; savehist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package savehist
  :custom
  (savehist-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; saveplace ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package saveplace
  :custom
  (save-place-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;; persistent-scratch ;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package persistent-scratch
  :demand t
  :custom
  (persistent-scratch-autosave-interval 60)
  (persistent-scratch-scratch-buffer-p-function
   (lambda ()
     (or (persistent-scratch-default-scratch-buffer-p)
         ;; buffers created by M-x scratch
         (and (boundp 'scratch-buffer)
              scratch-buffer))))
  :config
  (persistent-scratch-setup-default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ibuffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ibuffer
  :custom
  (ibuffer-sorting-mode 'alphabetic)
  (ibuffer-movement-cycle nil)
  (ibuffer-expert t)
  :bind
  (("C-x C-b" . ibuffer)
   :map ibuffer-mode-map
   ("TAB" . nil)))

(use-package ibuf-ext
  :after ibuffer
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filters
   '(("Org" (mode . org-mode))
     ("Haskell" (mode . haskell-mode))
     ("C/C++" (or (mode . c-mode)
                  (mode . c++-mode)))
     ("Python" (mode . python-mode))
     ("Erlang" (mode . erlang-mode))
     ("Emacs" (mode . emacs-lisp-mode))
     ("Grep" (mode . grep-mode))
     ("Text"
      (and (derived-mode . text-mode)
           (not (starred-name))))
     ("Programming"
      (or (derived-mode . prog-mode)
          (mode         . ess-mode)
          (mode         . compilation-mode)))))
  :bind (:map ibuffer-mode-filter-group-map
              ("TAB" . ibuffer-toggle-filter-group)))

(use-package ibuffer-vc
  :after (ibuffer ibuf-ext)
  :hook (ibuffer
         . (lambda ()
             (ibuffer-vc-set-filter-groups-by-vc-root)
             (unless (eq ibuffer-sorting-mode 'alphabetic)
               (ibuffer-do-sort-by-alphabetic)))))

;; haskell
(use-package haskell
  :mode (("\\.hsc\\'" . haskell-mode)
         ("\\.l[gh]s\\'" . haskell-literate-mode)
         ("\\.hsig\\'" . haskell-mode)
         ("\\.[gh]s\\'" . haskell-mode)
         ("\\.cabal\\'\\|/cabal\\.project\\|/\\.cabal/config\\'" . haskell-cabal-mode)
         ("\\.chs\\'" . haskell-c2hs-mode)
         ("\\.ghci\\'" . ghci-script-mode)
         ("\\.dump-simpl\\'" . ghc-core-mode)
         ("\\.hcr\\'" . ghc-core-mode))
  :interpreter (("runhaskell" . haskell-mode)
                ("runghc" . haskell-mode))

  :init
  (defvar my/haskell-indent-dont-cycle nil)

  (defun my/haskell-comment-dwim (arg)
    (interactive "*P")
    (let ((my/haskell-indent-dont-cycle t))
      (comment-dwim arg)))

  :hook (haskell-mode
         . (lambda ()
             (haskell-decl-scan-mode)
             (interactive-haskell-mode)

             ;; haskell-mode indentation cycles through multiple possible
             ;; indentation offsets and this throws comment-dwim off. It calls
             ;; (indent-according-to-mode) multiple times and so new comments
             ;; get indented somewhat randomly. This advice checks a
             ;; dynamically scoped `my/haskell-indent-dont-cycle` to prevent
             ;; the cycling behavior.
             (defadvice indent-according-to-mode (around maybe-dont-cycle activate)
               (if (and (eq major-mode 'haskell-mode)
                        my/haskell-indent-dont-cycle)
                   (let* ((indentations (haskell-indentation-find-indentations))
                          (do-indent (save-excursion
                                       (back-to-indentation)
                                       (not (member (current-column) indentations)))))
                     (when do-indent ad-do-it))
                 ad-do-it))))
  :custom
  (haskell-process-type 'auto)
  (haskell-compile-ignore-cabal t)
  (haskell-process-load-or-reload-prompt t)
  :bind (:map haskell-mode-map
              ("C-c C-l" . haskell-process-load-file)
              ("C-c C-z" . haskell-interactive-switch)
              ("C-c c" . haskell-compile)
              ("M-;" . my/haskell-comment-dwim)
         :map haskell-indentation-mode-map
              ;; don't auto-indent on RET
              ("RET" . nil)
         :map interactive-haskell-mode-map
              ;; don't interfere with ivy-resume
              ("C-c C-r" . nil)
              ;; let xref do its thing
              ("M-." . nil)))

(use-package lsp-haskell
  :hook (haskell-mode
         . (lambda ()
             (lsp-deferred)
             ;; lsp needs yasnippet
             (yas-minor-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package go-mode
  :custom
  ;; generating completion candidates is slow
  (godoc-use-completing-read nil)
  (godoc-command "go doc -all")
  :hook (go-mode
         ;; don't highlight tabs, since go really likes them
         . (lambda ()
             (setq-local whitespace-style
                         (delq 'tabs whitespace-style)))))

(use-package lsp-go
  :hook (go-mode . lsp-deferred))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c x J" . my/org-goto)
         ("C-c x j" . my/org-open-dwim)
         :map org-mode-map
         ("M-n" . org-metadown)
         ("M-p" . org-metaup)
         ("M-C-f" . org-metaright)
         ("M-C-b" . org-metaleft))

  :init

  (defconst my/org-files
    (file-expand-wildcards (concat "~/org" "/*.org")))

  (defun my/org-goto-buffer ()
    (find-file-existing "~/org/todo.org")
    (goto-char 0))

  (defun my/org-open-dwim (&optional select)
    (interactive "@P")
    (condition-case err (org-clock-goto select)
      (error (message "Error in org-clock-goto: %s" (cdr err))
             (my/org-goto-buffer))))

  (defun my/org-goto (&optional alternative-interface)
    (interactive "P")

    (unless (eq major-mode 'org-mode)
      ;; switch to org buffer only if we're not already there
      (my/org-goto-buffer))
    (org-goto alternative-interface))

  :custom
  (org-extend-today-until 3)
  (org-todo-keywords '((sequence "TODO(t)"
                                  "STARTED(s!)"
                                  "PAUSED(p!)"
                                  "WAITING(w@)"
                                  "ASSIGN(a!)"

                                  "|"

                                  "DONE(d@)"
                                  "CANCELLED(c@)")))
  (org-agenda-files my/org-files)
  (org-default-notes-file "~/org/notes.org")
  (org-agenda-ndays 1)
  (org-deadline-warning-days 14)
  (org-agenda-show-all-dates t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-start-on-weekday nil)
  (org-reverse-note-order t)
  (org-fast-tag-selection-single-key 'expert)
  (org-agenda-custom-commands
    '(("A" agenda "Today"
       ((org-agenda-skip-function
         '(org-agenda-skip-entry-if 'todo '("ASSIGN" "WAITING")))))
      ("T" agenda "Tomorrow"
       ((org-agenda-start-day "+1")
        (org-agenda-ndays 1)
        (org-agenda-skip-function
         '(org-agenda-skip-entry-if 'todo '("ASSIGN" "WAITING")))))
      ("W" agenda "Week"
       ((org-agenda-ndays 7)
        (org-agenda-skip-function
         '(org-agenda-skip-entry-if 'todo '("ASSIGN" "WAITING")))))
      ("Q" . "Queries")
      ("QN" "Next" tags "NEXT")
      ("QW" "Waiting" tags-todo "TODO=\"WAITING\"|WAITING" nil)
      ("QA" "Unassigned" todo "ASSIGN" nil)
      ("QD" "Completed" todo "DONE|CANCELLED" nil)
      ("QU" "Unscheduled" alltodo "TODO"
       ((org-agenda-skip-function
         (lambda nil
           (org-agenda-skip-entry-if 'scheduled 'deadline
                                     'regexp "<[^>\n]+>")))
        (org-agenda-overriding-header "Unscheduled TODO entries: ")))))
  (org-remember-store-without-prompt t)
  (remember-annotation-functions '(org-remember-annotation))
  (remember-handler-functions '(org-remember-handler))

  (org-time-stamp-custom-formats
   (cons "<%B %d, %Y>" "<%B %d, %Y %H:%M>"))
  (org-display-custom-times t)
  (org-clock-persist t)

  ;; ask for a note for every state change
  (org-log-into-drawer "LOGBOOK")

  ;; log time for done state
  (org-log-done 'time)

  (org-agenda-time-grid
   '((daily today require-timed)
     (600 800 1000 1200 1400 1600 1800 2000 2200 2359)
     "......" "----------------"))

  (org-agenda-sorting-strategy
   '((agenda time-up priority-down category-keep)
     (todo   priority-down category-keep)
     (tags   priority-down category-keep)
     (search category-keep)))

  (org-columns-default-format
   "%TODO %75ITEM %SCHEDULED %TAGS %PRIORITY %8Effort(ESTIMATE){:} %8CLOCKSUM(CLOCK)")

  (org-clock-in-switch-to-state "STARTED")
  (org-clock-out-switch-to-state "PAUSED")

  (org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 3))

  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

  (org-tags-column 80)
  (org-insert-heading-respect-content t)
  (org-goto-interface 'outline-path-completion)
  (org-enforce-todo-dependencies t)

  (org-clock-report-include-clocking-task t)

  (org-export-date-timestamp-format "%B %d, %Y")
  (org-odt-use-date-fields t)

  (org-cycle-emulate-tab 'white)
  (org-use-speed-commands t)

  :config
  (org-clock-persistence-insinuate)

  (defadvice org-mark-element (after org-mark-element-activate last activate)
    (activate-mark))

  (defadvice org-mark-subtree (after org-mark-subtree-activate last activate)
    (activate-mark))

  :hook (org-mode
         . (lambda ()
             ;; auto-fill everything, not just comments in org-mode
             (set-variable 'comment-auto-fill-only-comments nil t))))

(use-package org-contrib
  :hook (org-mode
         . (lambda ()
             (require 'org-checklist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; unbound ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ubound
  :commands (describe-unbound-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eshell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eshell
  :bind ("C-c e" . (lambda ()
                     (interactive)
                     (eshell t))))

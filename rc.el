;; global hook handler
(load "emacs-rc-global-hook.el")

;; Customizing common Emacs' appearance and behavior
(load "emacs-rc-common.el")

;; misc keybindings
(load "emacs-rc-misc-keybindings.el")

;; appearance
(load "emacs-rc-appearance.el")
(load "emacs-rc-modeline.el")

;; auto-fill-mode adjustments
(load "emacs-rc-auto-fill-mode.el")

(load "emacs-rc-flycheck.el")

;; user info
(load "emacs-rc-user.el")

(load "emacs-rc-lsp-mode.el")

;; c-mode
(load "emacs-rc-c-mode.el")

;; Numbering lines
(load "emacs-rc-linum.el")

;; Loading persistence features.
(load "emacs-rc-desktop.el")

(load "emacs-rc-comint.el")

;; Using Haskell goodies
(load "emacs-rc-haskell.el")

;; Some eye candy including beautiful buffer switching (instead of iswitch)
(load "emacs-rc-swiper.el")

;; Parantheses highlighting
(load "emacs-rc-show-paren.el")

;; Org mode
(load "emacs-rc-org-mode.el")

;; latex
(load "emacs-rc-latex.el")

;; make names of the buffer be unique
(load "emacs-rc-uniquify.el")

;; whitespace mode
(load "emacs-rc-whitespace.el")

(load "emacs-rc-gtags.el")

;; Erlang
(load "emacs-rc-erlang.el")

;; markdown mode
(load "emacs-rc-markdown.el")

;; ediff adjustments
(load "emacs-rc-ediff.el")

;; ibuffer
(load "emacs-rc-ibuffer.el")

;; view-mode
(load "emacs-rc-view-mode.el")

;; avy
(load "emacs-rc-avy.el")

;; git
(load "emacs-rc-git.el")

;; compilation setup (must be placed after eproject)
(load "emacs-rc-compile.el")

(load "emacs-rc-go-mode.el")

(load "emacs-rc-vc.el")

(load "emacs-rc-grep.el")

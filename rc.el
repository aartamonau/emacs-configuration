;; el-get
(load "emacs-rc-el-get.el")

;; global hook handler
(load "emacs-rc-global-hook.el")

;; Customizing common Emacs' appearance and behavior
(load "emacs-rc-common.el")

;; misc keybindings
(load "emacs-rc-misc-keybindings.el")

;; Flyspell
;;
;; There's some weird interaction between theme loading and ispell
;; initialization that I don't quite understand but that causes start-up to
;; fail in ispell when custom.el is present. Loading this before loading
;; emacs-rc-appearance.el helps for some reason.
(load "emacs-rc-flyspell.el")

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

;; hippie expand
(load "emacs-rc-hippie-expand.el")

;; git
(load "emacs-rc-git.el")

;; compilation setup (must be placed after eproject)
(load "emacs-rc-compile.el")

(load "emacs-rc-undo-tree.el")

(load "emacs-rc-dired.el")

(load "emacs-rc-go-mode.el")

(load "emacs-rc-vc.el")

(load "emacs-rc-which-func.el")

(load "emacs-rc-tramp.el")

(load "emacs-rc-expand-region.el")

(load "emacs-rc-grep.el")

(load "emacs-rc-windows.el")

(load "emacs-rc-shell.el")

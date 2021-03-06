;; emacs-rc-linum.el

(global-linum-mode 1)

(defconst linum-mode-excludes '(doc-view-mode org-mode pdf-view-mode)
  "List of major modes preventing linum to be enabled in the buffer.")

(defadvice linum-mode (around linum-mode-selective activate)
  "Avoids enabling of linum-mode in the buffer having major mode set to one
of listed in `linum-mode-excludes'."
  (unless (member major-mode linum-mode-excludes)
    ad-do-it))

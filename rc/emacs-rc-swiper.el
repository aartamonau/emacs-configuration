(require 'swiper)
(require 'counsel)

(ivy-mode 1)

(setq ivy-use-virtual-buffers nil)
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)
(global-set-key (kbd "M-s s") 'isearch-forward)
(global-set-key (kbd "M-s r") 'isearch-backward)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h S") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x j") 'counsel-file-jump)
(global-set-key (kbd "C-x C-j") 'counsel-dired-jump)
(global-set-key (kbd "C-x C-,") 'counsel-mark-ring)
(global-set-key (kbd "C-c i") 'counsel-imenu)

(define-key ivy-minibuffer-map (kbd "M-r") 'ivy-restrict-to-matches)
(define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-s") 'ivy-next-line)

(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "M-j") 'isearch-yank-word-or-char)

(setq ivy-initial-inputs-alist
      '((org-refile . "^")
        (org-agenda-refile . "^")
        (org-capture-refile . "^")
        (counsel-M-x . "^")
        (counsel-describe-function . "")
        (counsel-describe-variable . "")
        (man . "^")
        (woman . "^")))

(defun my/ivy-thing-nearest-point (orig-fun &rest args)
  (let ((r (apply orig-fun args)))
    (message "result %s" r)
    (if (and r (not (string= r "")))
        r
      (or (tap-thing-nearest-point 'symbol) ""))))

(advice-add 'ivy-thing-at-point :around #'my/ivy-thing-nearest-point)
(advice-remove 'ivy-thing-at-point #'my/ivy-thing-nearest-point)

(require 'ivy-xref)
;; xref initialization is different in Emacs 27 - there are two different
;; variables which can be set rather than just one
(when (>= emacs-major-version 27)
  (setq xref-show-definitions-function #'ivy-xref-show-defs))
;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
;; commands other than xref-find-definitions (e.g. project-find-regexp)
;; as well
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)

(require 'swiper)
(require 'counsel)

(ivy-mode 1)

(setq ivy-use-virtual-buffers t)
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

(define-key ivy-minibuffer-map (kbd "M-r") 'ivy-restrict-to-matches)

(setq ivy-initial-inputs-alist
      '((org-refile . "^")
        (org-agenda-refile . "^")
        (org-capture-refile . "^")
        (counsel-M-x . "")
        (counsel-describe-function . "")
        (counsel-describe-variable . "")
        (man . "^")
        (woman . "^")))

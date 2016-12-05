(require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

(define-key magit-log-mode-map (kbd "j") 'magit-goto-next-section)
(define-key magit-log-mode-map (kbd "k") 'magit-goto-previous-section)

;; ignore whitespace in git blame
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" "-w" rev)))

(define-key magit-mode-map (kbd "C-c C-c") 'magit-commit)
(define-key magit-mode-map (kbd "C-c C-a") 'magit-commit-amend)
(define-key magit-mode-map (kbd "R") 'magit-rebase-interactive)

;; magit-view-file-history needs this
(setq magit-sha1-abbrev-length (magit-abbrev-length))

(defadvice magit-view-file-at-commit (around
                                      magit-view-file-at-commit-reuse-buffer
                                      activate)
  (flet ((generate-new-buffer (name)
                              (if (get-buffer name)
                                  (kill-buffer name))
                              (get-buffer-create name)))
    ad-do-it))

(global-diff-hl-mode t)

(require 'grep-o-matic)

(defconst my/grep-extra-patterns
  '("*.[he]rl" "*.hs" "*.cmake" "CMakeLists" "*.bash" "*.rb"))

(custom-set-variables
 '(grep-o-matic-use-git-grep nil)
 '(grep-o-matic-search-patterns (append my/grep-extra-patterns
                                        grep-o-matic-search-patterns)))

(define-key 'grep-o-matic-map "\M-j" 'grep-o-matic-repository)
(define-key 'grep-o-matic-map "j" 'grep-o-matic-repository)
(define-key 'grep-o-matic-map "\M-k" 'grep-o-matic-current-directory)
(define-key 'grep-o-matic-map "k" 'grep-o-matic-current-directory)
(define-key 'grep-o-matic-map "\M-l" 'grep-o-matic-visited-files)
(define-key 'grep-o-matic-map "l" 'grep-o-matic-visited-files)

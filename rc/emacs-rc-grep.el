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

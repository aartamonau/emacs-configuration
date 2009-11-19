;; (load "gas-mode")
;; (require 'gas-mode)
;; (add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))
;; (add-to-list 'auto-mode-alist '("\\.s\\'" . gas-mode))

(eval-after-load "asm-mode"
  '(progn
     (setq asm-comment-char ?\#)
     (define-key asm-mode-map (kbd "RET") 'newline)))

;; hook seems not to be working
;; (add-hook 'asm-mode-hook 'my-asm-mode-hook)


(global-set-key (kbd "C-x C-b") 'ibuffer)

(custom-set-variables
 '(ibuffer-saved-filters
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
 '(ibuffer-sorting-mode 'alphabetic)
 '(ibuffer-movement-cycle nil)
 '(ibuffer-expert t)
 '(ibuffer-show-empty-filter-groups nil))

(defun my/ibuffer-mode-hook ()
  (define-key
    ibuffer-mode-filter-group-map (kbd "<tab>")
    'ibuffer-toggle-filter-group))

(add-hook 'ibuffer-mode-hook 'my/ibuffer-mode-hook)

(add-hook 'c-mode-common-hook 'ggtags-mode)

(eval-after-load "ggtags"
  '(progn (define-key ggtags-navigation-map "\M->" nil)
          (define-key ggtags-navigation-map "\M-<" nil)
          (define-key ggtags-navigation-map "\C-\M-s" nil)
          (define-key ggtags-navigation-map "\C-c\M-<" 'fisrst-error)
          (define-key ggtags-navigation-map "\C-c\M->" 'ggtags-navigation-last-error)))

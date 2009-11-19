(autoload 'muse-mode "muse-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.muse$" . muse-mode))

(eval-after-load "muse-mode"
  '(progn
     (require 'muse-colors)
     (require 'muse-html)
     (require 'muse-latex)
     (require 'muse-project)
     (muse-derive-style "custom-pdf" "pdf"
		      :header "~/emacs/muse/styles/pdf/header.tex"
		      :footer "~/emacs/muse/styles/pdf/footer.tex")))

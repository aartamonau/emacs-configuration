(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-output-view-style
      '(("^dvi$"
	 ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
	 "%(o?)dvips -t landscape %d -o && gv %f")

	("^dvi$"
	 "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
	
	("^dvi$"
	 ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$")
	 "%(o?)xdvi %dS -paper a4r -s 0 %d")

	("^dvi$"
	 "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$"
	 "%(o?)xdvi %dS -paper a4 %d")

	("^dvi$"
	 ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$")
	 "%(o?)xdvi %dS -paper a5r -s 0 %d")

	("^dvi$"
	 "^\\(?:a5\\(?:comb\\|paper\\)\\)$"
	 "%(o?)xdvi %dS -paper a5 %d")

	("^dvi$"
	 "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")

	("^dvi$"
	 "^letterpaper$" "%(o?)xdvi %dS -paper us %d")

	("^dvi$"
	 "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")

	("^dvi$"
	 "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")

	("^dvi$"
	 "." "%(o?)xdvi %dS %d")

	("^pdf$"
	 "." "evince %o %(outpage)")

	("^html?$" "." "firefox %o")))
 	
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'TeX-mode-hook '(lambda () (TeX-PDF-mode 1)))
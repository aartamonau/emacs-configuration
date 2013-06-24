(require 'ledger)

(custom-set-variables
 '(ledger-reports
   '(("balance" "ledger -f %(ledger-file) -C balance '^\(assets|liabilities\):'")
     ("transactions" "ledger -f %(ledger-file) -C register ^%(account)")
     ("payee" "ledger -f %(ledger-file) -C register -- %(payee)")
     ("account" "ledger -f %(ledger-file) -C register %(account)"))))

(defadvice ledger-report-cmd (around ledger-report-gpg)
  (let ((ledger-reports
         (if (string= (file-name-extension (or (buffer-file-name ledger-buf) "")) "gpg")
             (mapcar
              (lambda (report)
                (list (car report)
                      (concat
                       "gpg2 --no-tty --quiet -d %(ledger-file) | ledger -f - "
                       (mapconcat 'identity (cdddr (split-string (cadr report))) " "))))
              ledger-reports)
           ledger-reports)))
    ad-do-it))

(ad-activate 'ledger-report-cmd)

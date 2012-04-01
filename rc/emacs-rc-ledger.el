(require 'ledger)

(custom-set-variables
 '(ledger-reports
   '(("balance" "ledger -f %(ledger-file) balance ^assets:")
     ("transactions" "ledger -f %(ledger-file) register ^assets")
     ("payee" "ledger -f %(ledger-file) register -- %(payee)")
     ("account" "ledger -f %(ledger-file) register %(account)"))))

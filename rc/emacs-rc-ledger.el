(require 'ledger)

(custom-set-variables
 '(ledger-reports
   '(("balance" "ledger -C -f %(ledger-file) balance '^\(assets|liabilities\):'")
     ("transactions" "ledger -C -f %(ledger-file) register ^%(account)")
     ("payee" "ledger -C -f %(ledger-file) register -- %(payee)")
     ("account" "ledger -C -f %(ledger-file) register %(account)"))))

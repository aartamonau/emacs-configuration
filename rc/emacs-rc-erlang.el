(defconst erlang-root-dir "/usr/lib/erlang")
(defconst erlang-bin-dir
  (concat (file-name-as-directory erlang-root-dir) "bin"))

(require 'erlang-start)

(add-hook 'erlang-mode-hook 'global-hook-handler)

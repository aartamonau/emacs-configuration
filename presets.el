(require 'cl)

(defconst presets-directory "~/emacs/presets"
  "Directory containing presets.")

(defvar preset-chosen "default"
  "Preset that was chosen by user.")

(defun preset-to-paths (preset)
  "Returns a list of paths where preset can possibly be found."
  (list (concat presets-directory "/" preset)
        (concat presets-directory "/" preset ".el")))

(defun load-preset-helper (preset)
  "Loads preset by its name.
Returns 't if file has been loaded and nil otherwise."
  (let ((paths (remove-if-not (lambda (p)
                                (and (file-exists-p p)
                                     (not (file-accessible-directory-p p))))
                              (preset-to-paths preset))))
    (if paths
        (load-file (first paths)))))

(defun load-preset (preset)
  "Loads specified preset falling back to the default one if it's needed."
  (unless (load-preset-helper preset)
    (error "Unable to find '%s' preset." preset)))

(defun parse-command-line (opts)
  "Parses command line options and sets 'preset-chosen' variable."
  (when opts
    (if (and (equal (first opts) "--preset")
             (first (rest opts)))
        (progn (setq preset-chosen (first (rest opts)))
               (rest (rest opts)))
      (cons (first opts)
            (parse-command-line (rest opts))))))

(setq command-line-args (parse-command-line command-line-args))
(load-preset preset-chosen)

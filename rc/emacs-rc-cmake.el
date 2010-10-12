(autoload 'cmake-mode "cmake-mode"
  "Major mode for editing CMake listfiles.")

(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

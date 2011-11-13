;; Trick to get the filename of the installation directory
(defconst epy-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs-for-python"
)

(add-to-list 'load-path epy-install-dir)

(require 'epy-setup)
(require 'epy-ropemacs)
(require 'epy-menu)
(require 'epy-misc)

(provide 'epy)

;; Trick to get the filename of the installation directory
(defconst epy-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs-for-python"
)

(add-to-list 'load-path epy-install-dir)

;; Adding paths to the variable load-path
(dolist (relpath '(""
                   "elisp"
                   "extensions/"
                   "extensions/yasnippet"
                   "extensions/auto-complete"
                   )
                 )
  (add-to-list 'load-path (concat epy-install-dir relpath)))

;; add `epy/bin' to PATH, for epylint, pep8 etc
(let ( (bin-path (concat epy-install-dir "bin")) )
       (add-to-list 'exec-path bin-path)
       (setenv "PATH" (concat bin-path path-separator (getenv "PATH"))))

(defgroup epy nil
  "emacs-for-python package"
  :group  'python
  :prefix "epy-")

(require 'epy-completion)
(require 'epy-menu)

(provide 'epy)

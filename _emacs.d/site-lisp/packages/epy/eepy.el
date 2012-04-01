;; Trick to get the filename of the installation directory
(defconst eepy-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs-for-python"
)

(add-to-list 'load-path eepy-install-dir)

;; Adding paths to the variable load-path
(dolist (relpath '(""
                   "extensions/"
                   "extensions/yasnippet"
                   "extensions/auto-complete"
                   "elisp"
                   )
                 )
  (add-to-list 'load-path (concat eepy-install-dir relpath)))

;; add `eepy/bin' to PATH, for epylint, pep8 etc
(let ( (bin-path (concat eepy-install-dir "bin")) )
       (add-to-list 'exec-path bin-path)
       (setenv "PATH" (concat bin-path path-separator (getenv "PATH"))))

;; add `eepy/python-libs' to PYTHONPATH
(let ( (pylibs-path (concat eepy-install-dir "python-libs")) )
       (setenv "PYTHONPATH" (concat pylibs-path path-separator (getenv "PYTHONPATH"))))

(defgroup eepy nil
  "emacs-for-python package"
  :group  'python
  :prefix "eepy-")

(require 'pymacs (concat eepy-install-dir "extensions/pymacs.el"))

(provide 'eepy)

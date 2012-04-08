;;* static checker and flymake checker

;;** develock
;;(setq develock-auto-enable nil)

(autoload 'develock-mode  "develock"
  "additional font-lock keywords for the developers." t)

;;TODO: (defcustom eepy-enable-develock)

(eval-after-load "develock"
  `(progn
     (load-file (concat eepy-install-dir "elisp/develock-py.el"))
     ))


;;** static checker
(defun eepy-pylint ()
  (interactive)
  (let ( (compile-command (concat "epylint"   ;; "pylint -rn -f parseable "
                                  (shell-quote-argument (buffer-file-name)))) )
    (call-interactively 'compile)
  ))


(defun eepy-pep8 ()
  (interactive)
  (let ( (compile-command (concat "pep8 "
                                  (shell-quote-argument (buffer-file-name)))) )
    (call-interactively 'compile)  
  ))

;;TODO: pychecker
;;TODO: pyflakes

(defcustom eepy-static-checker "epylint"
  "Default static checker/lint program for python."
  :group 'eepy
  )

(defun eepy-set-checker (checker)
  (interactive
   (list (ido-completing-read "Checker: "
                              '("epylint" "pep8" "pyflakes" "pychecker")
                              nil
                              nil
                              nil
                              nil
                              eepy-static-checker)))
  (setq eepy-set-checker checker)
  (message "Default python checker set to %s." checker))



;;** flymake
(when (or (require 'flymake (concat eepy-install-dir "elisp/flymake") t)
          (require 'flymake))
  (if (boundp 'flymake-info-line-regex)
      (setq flymake-info-line-regex     ;;only available in `flymake-patch.el'
            (append flymake-info-line-regex '("unused$" "^redefinition" "used$"))))
  
  (load "flymake-cursor" nil t)
  (require 'rfringe nil t))


(defun flymake-create-copy-file ()
       "Create a copy local file"
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace)))
         (file-relative-name
          temp-file
          (file-name-directory buffer-file-name))))

(defcustom eepy-flymake-cmdline nil
  "The static checker used for flymake-mode in python.

The CMDLINE should be something like:

  flymaker %f

e.g.
  python -mpylint \"%f\"

%f will be substituted with a temporary copy of the file that is
 currently being checked.

Make sure a `%f' is included in the command line"
  :type '(choice (const :tag "Off" nil)
                 (const :tag "epylint"    "epylint \"%f\"")
                 (const :tag "pep8"       "pep8 \"%f\"")
                 (const :tag "pycheckers" "pycheckers \"%f\""  )
                 (const :tag "pyflakes"   "pyflakes \"%f\"")
                 (string :tag "custom..."))
  :group 'eepy)


(defun flymake-eepy-init ()
    (let ((cmdline-subst (replace-regexp-in-string "%f"
                                                   (flymake-create-copy-file)
                                                   eepy-flymake-cmdline)))
    (setq cmdline-subst (split-string-and-unquote cmdline-subst))
    (list (first cmdline-subst) (rest cmdline-subst))
    ))

(add-to-list 'flymake-allowed-file-name-masks '("\\.pyw?\\'" flymake-eepy-init))

(defun eepy-flymake-with (checker-cmdline)
  (interactive
      (list (ido-completing-read "Checker: "
                              '("epylint \"%f\""
                                "pep8 \"%f\""
                                "pyflakes \"%f\""
                                "pychecker \"%f\"")
                              nil
                              nil
                              nil
                              nil
                              eepy-flymake-cmdline)))
  (make-variable-buffer-local 'eepy-flymake-cmdline)
  (setq eepy-flymake-cmdline cmdline)
  (flymake-mode -1)
  (flymake-mode t))
  
(defun python-mode-hook-flymake ()
  "initialize flymake on open python files."
  (when eepy-flymake-cmdline
    (eepy-flymake-with eepy-flymake-cmdline)))

(add-hook 'python-mode-hook 'python-mode-hook-flymake)


(provide 'eepy-checker)

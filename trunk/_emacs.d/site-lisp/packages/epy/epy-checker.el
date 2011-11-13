;;* static checker and flymake checker

;;** checker
(defun epy-pylint ()
  (interactive)
  (let ( (compile-command (concat "epylint"   ;; "pylint -rn -f parseable "
                                  (shell-quote-argument (buffer-file-name)))) )
    (call-interactively 'compile)
  ))


(defun epy-pep8 ()
  (interactive)
  (let ( (compile-command (concat "pep8 "
                                  (shell-quote-argument (buffer-file-name)))) )
    (call-interactively 'compile)  
  ))

;;TODO: pychecker
;;TODO: pyflakes

(defcustom epy-static-checker "epylint"
  "Default static checker/lint program for python."
  )

(defun epy-set-checker (checker)
  (interactive
   (list (ido-completing-read "Checker: "
                              '("epylint" "pep8" "pyflakes" "pychecker")
                              nil
                              nil
                              epy-static-checker
                              nil
                              epy-static-checker)))
  (setq epy-set-checker checker)
  (message "Default python checker set to %s." checker)



;;** flymake
(defun flymake-create-copy-file ()
       "Create a copy local file"
       (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                          'flymake-create-temp-inplace)))
         (file-relative-name 
          temp-file 
          (file-name-directory buffer-file-name))))

(defun flymake-command-parse (cmdline)...

;;Usage: (epy-setup-checker "epylint %f")
(defun epy-setup-checker (cmdline)
  (let ( (old-cfg (assoc-string "\\.py\\'" flymake-allowed-file-name-masks))
         (new-cfg (list (apply-partially 'flymake-command-parse cmdline))) )
    (if old-cfg
        (setcdr old-cfg new-cfg)
      (add-to-list 'flymake-allowed-file-name-masks new-cfg))))


(defcustom epy-flymaker nil
  "Whether to enable flymake"
  :type '(choice (const :tag "Off" nil)
                 (const :tag "epylint")                                                                                                                           
                 (const :tag "pep8")
                 (const :tag "pycheckers")
                 (const :tag "pyflakes")
                 (string :tag "custom..."))
  :group 'epy
  :set (lambda (symbol value)
         (set-default symbol value)
         (epy-setup-checker (concat value " %f"))))

(defun epy-flymake-with (checker)
  (interactive
      (list (ido-completing-read "Checker: "
                              '("epylint" "pep8" "pyflakes" "pychecker")
                              nil
                              nil
                              epy-flymaker
                              nil
                              epy-flymaker)))
  (epy-setup-checker (concat checker " %f"))
  (flymake-mode t))
  
(defun python-mode-hook-flymake ()
  "initialize flymake on open python files."
  (when epy-flymaker
    (epy-flymake-with epy-flymaker)))

(add-hook 'python-mode-hook 'python-mode-hook-flymake)

(when (require 'flymake "flymake-patch" t)
  (setq flymake-info-line-regex
        (append flymake-info-line-regex '("unused$" "^redefinition" "used$")))
  
  (load "flymake-cursor" nil t)
  (require 'rfringe nil t))

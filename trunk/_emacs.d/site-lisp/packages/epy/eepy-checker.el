;;* static checker and flymake checker

;;** checker
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
                              eepy-static-checker
                              nil
                              eepy-static-checker)))
  (setq eepy-set-checker checker)
  (message "Default python checker set to %s." checker))



;;** flymake
(when (require 'flymake "flymake-patch" t)
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
  
(defun flymake-command-setup (command &optional options)
  "Setup the command to be used with flymake, the command
will be called in this way: COMMAND OPTIONS FILE The FILE varible
is passed after the options."
  ;; Make sure it's not a remote buffer or flymake would not work
  (when (not (current-file-remotep)) 
    (list command
          (append options (list (flymake-create-copy-file))))))

;;Usage: (eepy-setup-checker "epylint %f")
(defun eepy-setup-checker (cmdline)
  (let ( (old-cfg (assoc-string "\\.py\\'" flymake-allowed-file-name-masks))
         (new-cfg (list (apply-partially 'flymake-command-parse cmdline))) )
    (if old-cfg
        (setcdr old-cfg new-cfg)
      (add-to-list 'flymake-allowed-file-name-masks new-cfg))))


(defcustom eepy-flymaker nil
  "Whether to enable flymake"
  :type '(choice (const :tag "Off" nil)
                 (const :tag "epylint")                                                                                                                           
                 (const :tag "pep8")
                 (const :tag "pycheckers")
                 (const :tag "pyflakes")
                 (string :tag "custom..."))
  :group 'eepy
  :set (lambda (symbol value)
         (set-default symbol value)
         (eepy-setup-checker (concat value " %f"))))

(defun eepy-flymake-with (checker)
  (interactive
      (list (ido-completing-read "Checker: "
                              '("epylint" "pep8" "pyflakes" "pychecker")
                              nil
                              nil
                              eepy-flymaker
                              nil
                              eepy-flymaker)))
  (eepy-setup-checker (concat checker " %f"))
  (flymake-mode t))
  
(defun python-mode-hook-flymake ()
  "initialize flymake on open python files."
  (when eepy-flymaker
    (eepy-flymake-with eepy-flymaker)))

(add-hook 'python-mode-hook 'python-mode-hook-flymake)



(provide 'eepy-checker)

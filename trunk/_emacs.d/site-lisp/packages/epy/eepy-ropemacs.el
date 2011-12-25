;; ropemacs Integration with auto-completion
(eval-after-load "auto-complete"
  `(progn     
     (defun ac-ropemacs-candidates ()
       (mapcar (lambda (completion)
                 (concat ac-prefix completion))
               (rope-completions)))

     (ac-define-source nropemacs
       '((candidates . ac-ropemacs-candidates)
         (symbol     . "p")))

     (ac-define-source nropemacs-dot
       '((candidates . ac-ropemacs-candidates)
         (symbol     . "p")
         (prefix     . c-dot)
         (requires   . 0)))

     (defun ac-nropemacs-setup ()
       (setq ac-sources (append '(ac-source-nropemacs
                                  ac-source-nropemacs-dot) ac-sources)))

     (eval-after-load "ropemacs"
       `(add-hook 'rope-open-project-hook 'ac-nropemacs-setup)
       )
     ))

(defun detect-rope-project ()
  (cond ((file-exists-p ".ropeproject")
         (rope-open-project default-directory)
         (ropemacs-mode t))
        ((file-exists-p "../.ropeproject")
         (rope-open-project (concat default-directory ".."))
         (ropemacs-mode t))
        ))

(defvar ropemacs-auto-mode nil
  "Whether to auto-detect `.ropeproject' file and turn on ropemacs-mode.")

(defun toggle-ropemacs-auto-mode (&optional arg)
  "Toggle: auto-detect `.ropeproject' file and if found turn on ropemacs-mode.

When turned on, upon opening Python file, it would check whether there's
`.ropeproject' file existing in current dir or parent dir. If found, it would
load it with ropemacs."
  (interactive "P")
  (setq ropemacs-auto-mode (if arg arg
                             (not ropemacs-auto-mode)))
  (if ropemacs-auto-mode
      (progn ;;turned on
        (add-hook 'python-mode-hook 'detect-rope-project)
        (if (eq major-mode 'python-mode)
            (detect-rope-project)))
    (progn
      (remove-hook 'python-mode-hook 'detect-rope-project)))
  (message "`ropemacs-mode' now would%s be automatically turned on."
           (if ropemacs-auto-mode "" " NOT"))
  )


(defun setup-ropemacs ()
  (require 'pymacs (concat eepy-install-dir "extensions/pymacs.el"))
  
  "Setup the ropemacs harness"
  ;; (setenv "PYTHONPATH"
  ;;         (concat
  ;;          (getenv "PYTHONPATH") path-separator
  ;;          (concat eepy-install-dir "python-libs/")))
  (pymacs-load "ropemacs" "rope-")
  
  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)

  (setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"
                                      "django.*"))
  
  ;; Adding hook to automatically open a rope project if there is one
  ;; in the current or in the upper level directory
;  (add-hook 'python-mode-hook 'auto-open-rope-project)

  (remove-hook 'python-mode-hook 'ropemacs-mode)  ;;supress auto ropemacs-mode
  
  (if (fboundp 'ac-nropemacs-setup)
      (add-hook 'rope-open-project-hook 'ac-nropemacs-setup))

  (toggle-ropemacs-auto-mode t)
  )

;; Python or python mode?
(eval-after-load 'python
  '(progn
     (setup-ropemacs)))

(provide 'eepy-ropemacs)



(global-set-key (kbd "<f3> f") 'find-function-at-point)
(global-set-key (kbd "<f3> F") 'find-function)
(global-set-key (kbd "<f3> v") 'find-variable-at-point)
(global-set-key (kbd "<f3> V") 'find-variable)
(global-set-key (kbd "<f3> l") 'find-library)

(global-set-key (kbd "<f12> k")   'find-function-on-key)

(global-set-key (kbd "<f12> e b") 'eval-buffer)
(global-set-key (kbd "<f12> e r") 'eval-region)
(global-set-key (kbd "<f12> e f") 'eval-defun)
(global-set-key (kbd "<f12> e s") 'eval-sexp)


(defun bmz/check-parens ()
  (interactive)
  (check-parens)
  (message "%s: OK" (buffer-file-name)))

(define-key emacs-lisp-mode-map (kbd "<M-f9>") 'bmz/check-parens)

(defun bmz/byte-compile-file ()
   (interactive)
   (let ( (emacs-lisp-mode-hook '()) )
     (byte-compile-file (buffer-file-name))))

(define-key emacs-lisp-mode-map (kbd "<C-f9>") 'bmz/byte-compile-file)


(defun bmz/dired-do-byte-compile (&optional arg)
  "Byte compile marked (or next ARG) Emacs Lisp files."
  (interactive "P")
  (let ( (emacs-lisp-mode-hook '()) )
    (dired-map-over-marks-check (function dired-byte-compile) arg 'byte-compile t)))

(define-key dired-mode-map "B" 'bmz/dired-do-byte-compile)


;;;_. code folding
(defun emacs-lisp-mode-init-fold ()
  (if (fboundp 'hideshowvis-enable)
      (hideshowvis-enable)
    (hs-minor-mode t))
  
  (setq outline-regexp "[ \t]*(defun ")
  )

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-init-fold)


;;;_. code complete

(if (fboundp 'ac-emacs-lisp-mode-setup)
    (add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup))


;;;_. documentatin
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)


(defun anything-info-elisp ()
  (interactive)
  (anything
   :input (thing-at-point 'symbol)
   :prompt "Info about: "
   :candidate-number-limit 10
   :sources
   '( anything-c-source-info-elisp
      ;;         anything-c-source-info-elib
      anything-c-source-info-cl)))

(define-key elisp-mode-map        (kbd "M-s f1")  'anything-info-elisp)
(define-key lisp-interaction-mode (kbd "M-s f1")  'anything-info-elisp)

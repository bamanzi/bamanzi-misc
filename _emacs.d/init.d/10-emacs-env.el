
;;;_. options
;;(setq custom-unlispify-tag-names nil)
(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10> <f10>") 'menu-bar-open)

(global-set-key (kbd "<C-f10> g") 'customize-group)
(global-set-key (kbd "<C-f10> v") 'customize-variable)
(global-set-key (kbd "<C-f10> f") 'customize-face)
(global-set-key (kbd "<C-f10> t") 'customize-themes)

(global-set-key (kbd "<C-f10> F") 'menu-set-font)

(global-set-key (kbd "<mode-line> <C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<mode-line> <C-wheel-down>") 'text-scale-decrease)


;;;_. emacs-lisp
(global-set-key (kbd "<f12> l l") 'load-library)
(global-set-key (kbd "<f12> l t") 'load-theme)

(global-set-key (kbd "<C-f10> d") 'toggle-debug-on-error)

(defun load-and-execute (library)
  "load a library 'foobar' and execute the command with same name (`foobar' or `foobar-mode')"
  (interactive
   (list (completing-read "Load library: "
                          (apply-partially 'locate-file-completion-table
                                           load-path
                                           (get-load-suffixes)))))
  (when (load library)
    (let ( (command (if (fboundp (intern library))
                        (intern library)
                      (intern (concat library "-mode")))) )
      (message "try to execute `%s'" command)
      (call-interactively command))))


;;; message
  (defadvice message (before who-said-that activate)
    "Find out who said that thing. and say so."
    (let ((trace nil) (n 1) (frame nil))
      (while (setq frame (backtrace-frame n))
        (setq n     (1+ n) 
              trace (cons (cadr frame) trace)) )
      (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
      (ad-set-args 1 (cons trace (ad-get-args 1))) ))


(ad-disable-advice 'message 'before 'who-said-that)
(ad-update 'message)

  (defadvice error (before who-said-that activate)
    "Find out who said that thing. and say so."
    (let ((trace nil) (n 1) (frame nil))
      (while (setq frame (backtrace-frame n))
        (setq n     (1+ n) 
              trace (cons (cadr frame) trace)) )
      (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
      (ad-set-args 1 (cons trace (ad-get-args 1))) ))


(ad-disable-advice 'error 'before 'who-said-that)
(ad-update 'error)

(defun bmz/toggle-debug-on-error ()
  (interactive)
  (if debug-on-error
      (progn ;;turn if off
        (setq debug-on-error nil)
        (ad-deactivate 'error)
        (ad-deactivate 'message))
    (progn  ;;turn it on
      (setq debug-on-error t)
      (ad-activate 'error)
      (ad-activate 'message)))
  (message "Debug on Error %s globally" (if debug-on-error
                                            "enabled"
                                          "disabled")))

(global-set-key (kbd "<C-f10> d") 'bmz/toggle-debug-on-error)


;;** helps
(idle-require 'help-mode)  ;;on linux sometime C-h v/f would complain 'help-setup-xref is void'
(idle-require 'help-fns+) 
(define-key help-map "\C-h" nil) ;;force '<f1> C-h' to list keymap of `help-map'
(define-key help-map " "  #'(lambda ()
                              (interactive)
                              (describe-keymap help-map)))  ;;help-fns+ needed?

(add-hook 'help-mode-hook 'visual-line-mode)

(defun describe-major-mode ()
  (interactive)
  (let ( (mode major-mode) )
    (with-help-window
        (format "%s" mode)
      (insert-string (describe-function mode)))))

(defun describe-major-mode ()
  (interactive)
  (describe-function major-mode))

(define-key help-map "M"  'describe-major-mode)
(define-key help-map "F"  'describe-face)
(define-key help-map "K"  'describe-keymap)

(defun show-variable-value (var)
  (interactive
     (let ((symb                          (or (and (fboundp 'symbol-nearest-point)
                                                   (symbol-nearest-point))
                                              (and (symbolp (variable-at-point)) (variable-at-point))))
           (enable-recursive-minibuffers  t)
           val)
       (setq val  (completing-read "variable: " obarray
                                   (if current-prefix-arg
                                       (lambda (vv) (user-variable-p vv))
                                     (lambda (vv) (or (boundp vv) (get vv 'variable-documentation))))
                                   t
                                   (symbol-name symb)
                                   nil (and (symbolp symb) (symbol-name symb))))
       (list (if (equal val "") symb (intern val)))))
  (message "%s: %s" (symbol-name var) (symbol-value var)))

(define-key help-map "V" 'show-variable-value)


;;** info
(define-key help-map "i"  nil)
(define-key help-map "ii" 'info)
(define-key help-map "ia" 'info-apropos)
(define-key help-map "is" 'info-lookup-symbol)
(define-key help-map "im" 'info-emacs-manual)
(define-key help-map "ic" 'Info-goto-emacs-command-node)
(define-key help-map "ik" 'Info-goto-emacs-key-command-node)

(idle-require 'info+)

(add-hook 'Info-mode-hook #'(lambda ()
                              ;; cancel binding to `Info-history-forward' and `Info-history-back'
                              (define-key Info-mode-map (kbd "<mouse-4>") nil)
                              (define-key Info-mode-map (kbd "<mouse-5>") nil)
                              ))


(defun info-view-file (file-or-node &optional buffer)
  "Read specific info file."
  (interactive (list
                (read-file-name "Info file name: " nil nil t)
                (if (numberp current-prefix-arg)
                    (format "*info*<%s>" current-prefix-arg))))
  (info-setup file-or-node
	      (pop-to-buffer-same-window (or buffer "*info*"))))
(define-key help-map "i " 'info-view-file)

;;** apropos
(define-key help-map "a"  nil)
(define-key help-map "aa" 'apropos)
(define-key help-map "ac" 'apropos-command)
(define-key help-map "ad" 'apropos-documentation)
(define-key help-map "ai" 'info-apropos)
(define-key help-map "al" 'apropos-library)
(define-key help-map "ao" 'apropos-user-options)
(define-key help-map "ag" 'apropos-group)
(define-key help-map "av" 'apropos-variable)
(define-key help-map "aV" 'apropos-value)
(autoload 'sys-apropos "sys-apropos" nil t)
(define-key help-map "as" 'sys-apropos)


;;;_. "utils"

(global-set-key (kbd "<f12> a") 'apropos)  ;;sys-apropos ?
(global-set-key (kbd "<f12> c") 'quick-calc)
(global-set-key (kbd "<f12> C") 'calc-dispatch)
(global-set-key (kbd "<f12> d") 'ediff)
(global-set-key (kbd "<f12> g") 'grep)
(global-set-key (kbd "<f12> i") 'info-apropos)

(global-set-key (kbd "<f12> m") 'woman)
(global-set-key (kbd "<f12> r") 'regexp-builder)
(global-set-key (kbd "<f12> s") 'eshell)
(global-set-key (kbd "<f12> S") 'shell)



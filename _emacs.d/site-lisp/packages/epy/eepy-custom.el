
;;Major Modes

;;- https://launchpad.net/python-mode
;;(require 'python-mode)

;;- https://github.com/fgallina/python.el
;;(load-file (concat eepy-install-dir "python-modes/fgallina/python.el"))

;;- http://www.loveshack.ukfsn.org/emacs/NEWS.python
;;(load-file (concat eepy-install-dir "python-modes/loveshack/python.el"))


(require 'eepy-menu)

;; Static checker & flymake
(setq flymake-log-level 2) ;;for debugging
(require 'eepy-checker)

(setq ropemacs-global-prefix "C-c C-p")
(require 'eepy-ropemacs)

(require 'eepy-completion)


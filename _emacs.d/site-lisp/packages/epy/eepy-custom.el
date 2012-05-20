
;;* Major Modes

;;- https://launchpad.net/python-mode
;;(require 'python-mode)

;;- https://github.com/fgallina/python.el
;;(load-file (concat eepy-install-dir "python-modes/fgallina/python.el"))

;;- http://www.loveshack.ukfsn.org/emacs/NEWS.python
;;(load-file (concat eepy-install-dir "python-modes/loveshack/python.el"))


;;* Static checker & flymake
(setq flymake-log-level 2) ;;for debugging
(require 'eepy-checker)

;;this would turn off flymake by default
(setq-default eepy-flymake-cmdline nil)
;;this would turn on flymake when opening new buffer, and use `epylint' as checker
(setq-default eepy-flymake-cmdline "epylint \"%f\"")

;;* Code completion: auto-complete + (pycomplete,ropemacs,yasnippet)
(require 'eepy-completion)

(setq ropemacs-global-prefix "C-c C-p")
(require 'eepy-ropemacs)




;;* a menu for accessing all EEPY stuff
(require 'eepy-menu)



;;{{{ packages, load-path and related
(add-to-list 'load-path "~/.emacs.d/lisp/")
;;(add-to-list 'load-path "~/.emacs.d/mylisp/")

;; (loop for f in (directory-files "~/.emacs.d/init.d" nil ".*.el")
;;       by (message "loading %s" f))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el") t)
  (package-initialize))

(when (file-exists-p "~/.emacs.d/site-lisp/site-start.el")
  (load "~/.emacs.d/site-lisp/site-start.el"))


;; leave customization & os/distro/installation-specific settings to another file
;; (customizations, paths, theme)
(setq custom-file "~/.emacs.d/customize.el")
(if (file-exists-p custom-file)
    (load custom-file))

;;}}}





;;{{{ generate autoload and load them
(defun generate-dir-autoload-and-load (top-dir)
  (let ( (generated-autoload-file "~/.emacs.d/autoloads.el") )
    (mapc '(lambda (dir)
             (update-autoloads-from-directories dir))
          '("e:/emacs/site-lisp/misc_"
            "e:/emacs/site-lisp/filetype_"
            "e:/emacs/site-lisp/prog_"
            "e:/emacs/site-lisp/win32_"
            ))
    (load-file generated-autoload-file)))


;;;_ misc
(unless (load "idle-require" t)
  ;; fail-safe for `idle-quire'
  (defun idle-require (feature &optional file noerror)
    (require feature)))
;; (setq idle-require-symbols '(cedet nxml-mode)) ;; <- Specify packages here.
;;(idle-require 'foobar)

(mapc  '(lambda (file)
	  (ignore-errors
	    (load file)))
       (directory-files "~/.emacs.d/init.d/" t "[0-9[0-9].*\.el$"))

;; (load "~/.emacs.d/init.d/10-init.el")

;; (load "~/.emacs.d/init.d/20-symbol-fns" t)
;; (load "~/.emacs.d/init.d/20-win-fns" t)

;; (load "~/.emacs.d/init.d/50-misc" t)

;; (load "~/.emacs.d/init.d/60-cua-keys" t)
;; (load "~/.emacs.d/init.d/60-vi-keys" t)


;; (load "~/.emacs.d/init.d/80-mouse" t)

;; (load "~/.emacs.d/init.d/95-one-key" t)


(if (fboundp 'idle-require-mode)
  (idle-require-mode t))

;;restore stub
;(defun idle-require (feature)
;  (require feature nil t))


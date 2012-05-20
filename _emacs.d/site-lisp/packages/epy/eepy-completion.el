;;* code completion for eepy

(require 'eepy)


;;** auto-complete
(autoload 'auto-complete-mode  "auto-complete"
  "AutoComplete mode" t)
(autoload 'global-auto-complete-mode  "auto-complete"
  "Toggle Auto-Complete mode in every possible buffer." t)

(eval-after-load "auto-complete"
  `(progn
     (unless (fboundp 'ac-config-default)  ;;if user not loaded `auto-complete' before this
       (load "auto-complete-config")
       (setq ac-dwim t)
       (ac-config-default))

     (add-to-list 'ac-dictionary-directories
                  (concat eepy-install-dir "extensions/auto-complete/dict/"))
     ))

(require 'auto-complete)

;;a helper command
(defun ac-toggle-source (source &optional desire)
  "Add or remove a SOURCE in `ac-sources'.

If DESIRE given, this source would be absolutely added (if DESIRE > 0) or
remove (if DESIRE <= 0). If DESIRE not given, it would be toggled."
  (interactive
   (list (intern-soft (ido-completing-read "Source: "
										   (loop for x being the symbols
												 if (and (boundp x)
														 (string-match "^ac-source-" (symbol-name x)))
												 collect (symbol-name x))))))
  (when (and source (symbolp source))
	(if desire
		(if (> desire 0)
			(add-to-list 'ac-sources source)
		  (setq ac-sources (remq source ac-sources)))
	  (if (memq source ac-sources)
		  (setq ac-sources (remq source ac-sources))
		(add-to-list 'ac-sources source)))
	(message "Source `%s' %s." source (if (memq source ac-sources)
										  "enabled"
										"disabled"))))


(defcustom eepy-auto-complete-sources
  '(ac-source-python-builtin
    ;;ac-source-pycompletemine
    ;;ac-source-scite-api
    ;;ac-source-yasnippet
    )
  "Default additional auto-completion sources for python-mode
(besides `ac-sources' default value).

You don't need add `ac-source-nropemacs' into this, as it's due to add
by ropeproject hook."
  :group 'eepy)

(defun python-mode-init-ac-sources ()
  (mapc #'(lambda (source)
            (add-to-list 'ac-source source))
        eepy-auto-complete-sources))

(add-hook 'python-mode-hook 'python-mode-init-ac-sources)  
  
;;*** Emacs's built-in completion
;;advantages:
;;      + no other libraries needed
;;      + with 'send region' to inferior python process, you can get more completions
;;        e.g. if you send
;;               foo = re.compile("^(defcustom eepy-")
;;        to inferior python process, then you can get completions for 'foo'
;;disadvantages:
;;      - no doc info, nor function signature
;;      - in order to get completions, you need to send some python code to inferior python

(defun python-symbol-completions-maybe (prefix)
  (let ((python-el (symbol-file major-mode)))
    (if (string-match "lisp/progmodes/python.el" python-el) ;;Emacs builtin python.el
        (python-symbol-completions prefix)
      nil) ;;otherwise, return nil
    ))

(ac-define-source python-builtin
  '( (candidates . (python-symbol-completions-maybe ac-prefix))
     (symbol . "py")
     (prefix . "[ \t\n['\",()]\\([^\t\n['\",()]+\\)\\=") ))

(defun ac-enable-python-builtin-source (&optional on)
  (interactive)
  (let ((turn-on (or on
                     (not (memq ac-source-python-builtin ac-sources)))))
    (if turn-on
        (add-to-list 'ac-sources 'ac-source-python-builtin)
      (setq ac-sources (remq ac-source-python-builtin ac-sources)))))


;;*** pycompletemine from PDEE (https://github.com/pdee/pdee/ )
;; You need `pycompletemine.{el,py}' from PDEE and pymacs
;;advantages:
;;   + differ from `pycomplete', this one would work on both python-mode.el
;;     and GNU Emacs built-in python.el (improved by PDEE and EEPY)
;;   + doc info and signature for completions
;;disadvantages:
;;   - `pymacs' needed

(ac-define-source pycompletemine
  '((depends pycompletemine)  ;;FIXME: ok?
    (prefix .  "[ \t\n['\",()]\\([^\t\n['\",()]+\\)\\=")
    (candidates . (pycomplete-get-all-completions-for-ac ac-prefix))
    (symbol . "pyc")
    (document . py-complete-help)))

(defun ac-enable-pycompletemine-source (&optional on)
  (interactive)
  (let ((turn-on (or on
                     (not (memq ac-source-pycompletemine ac-sources)))))
    (if turn-on
        (add-to-list 'ac-sources 'ac-source-pycompletemine)
      (setq ac-sources (remq ac-source-pycompletemine ac-sources)))))

;;*** auto-complete-scite-api
;;TODO: ...
(if (require 'auto-complete-scite-api nil t)
    (add-to-list 'ac-scite-api-directories (concat eepy-install-dir "etc")))


;;** ropemacs: (code completion (and other features) for project
(require 'eepy-ropemacs)

;;** yasnippets

;; Disabling Yasnippet completion 
(defun epy-snips-from-table (table)
  (with-no-warnings
    (let ((hashtab (ac-yasnippet-table-hash table))
          (parent (ac-yasnippet-table-parent table))
          candidates)
      (maphash (lambda (key value)
                 (push key candidates))
               hashtab)
      (identity candidates)
      )))

(defun epy-get-all-snips ()
  ;;https://github.com/mlf176f2/yas-jit.el
  (require 'yas-jit nil t) ;; FIXME: find a way to conditionally load it
  (if (featurep 'yasnippet)
      (let (candidates)
        (maphash
         (lambda (kk vv) (push (epy-snips-from-table vv) candidates)) yas/tables)
        (apply 'append candidates))
    ))

(eval-after-load "auto-complete"
  `(setq ac-ignores (concatenate 'list ac-ignores (epy-get-all-snips)))
  )


(provide 'eepy-completion)

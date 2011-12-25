;;* code completion for eepy

(require 'eepy)

;;** autopair
;; Matching parentheses for all languages and so on
(require 'autopair)
;;(autopair-global-mode t)
(setq autopair-autowrap t)
;; Fix for triple quotes in python
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

;;** auto-complete
(require 'auto-complete)

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
     
     (add-to-list 'ac-dictionary-directories (concat eepy-install-dir "extensions/auto-complete/dict/"))
     ))

;;*** Emacs's built-in completion python.el
;;advantages:
;;      + no other libraries needed
;;      + with 'send region' to inferior python process, you can get more completions
;;        e.g. if you send
;;               foo = re.compile("^(defcustom eepy-")
;;        to inferior python process, then you can get completions for 'foo'
;;disadvantages:
;;      - no doc info, nor function signature
;;      - in order to get completions, you need to send some python code to inferior python

(ac-define-source python
  '( (candidates . (python-symbol-completions ac-prefix))
     (symbol . "py")
     (prefix . "[ \t\n['\",()]\\([^\t\n['\",()]+\\)\\=") ))

;;*** pycompletemine from PDEE (https://github.com/pdee/pdee/ )
;; You need `pycompletemine.{el,py}' from PDEE and pymacs
;;advantages:
;;   + differ from `pycomplete', this one would work on both python-mode.el
;;     and GNU Emacs built-in python.el (improved by PDEE and EEPY)
;;   + doc info and signature for completions
;;disadvantages:
;;   - `pymacs' needed

;;(ac-define-source pycomplete
(setq ac-source-pycompletemine
  '((depends pycompletemine)
    (prefix .  "[ \t\n['\",()]\\([^\t\n['\",()]+\\)\\=")
    (candidates . (pycomplete-get-all-completions ac-prefix))
    (symbol . "pyc")
    (document . py-complete-help)))

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
  (require 'yasnippet) ;; FIXME: find a way to conditionally load it
  (let (candidates)
    (maphash
     (lambda (kk vv) (push (epy-snips-from-table vv) candidates)) yas/tables)
    (apply 'append candidates))
  )

(eval-after-load "auto-complete"
  `(setq ac-ignores (concatenate 'list ac-ignores (epy-get-all-snips)))
  )

;;** other completion methods
;;TODO: implement this

(provide 'eepy-completion)

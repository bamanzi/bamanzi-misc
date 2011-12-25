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

     
;;** ropemacs
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

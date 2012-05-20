;; query python document

;; This file is part of EEPY (Enhanced Emacs for Python)
;; 

;;** `info-lookup-symbol'
;; With python 2.6's switching to sphinx as documentation tool, the texinfo
;; document no longer provided with python official releases.
;;
;; You can generate texinfo documents following these info:
;;  http://stackoverflow.com/questions/1054903/how-do-you-get-python-documentation-in-texinfo-info-format
;;  http://bitbucket.org/jonwaltman/sphinx-info
;;  http://bitbucket.org/jonwaltman/rst2texinfo/src
;;
;; But if you're lazy, you can force to use python-2.5's info file
;;   http://packages.debian.org/squeeze/python2.5-doc
(defvar eepy-python-info-force-version "2.5"
  "Force using python X.x's texinfo documents.)

;;stolen from Dave Love's python.el
(defun python-init-info-look ()
  "Set up info-look for Python.
Tries to take account of versioned Python Info files, e.g. Debian's
python2.5-ref.info.gz.
Used with `eval-after-load'."
  (let* ((py-version (let ((s
                            (shell-command-to-string (concat python-command " -V"))))
                       (string-match "^Python \\([0-9]+\\.[0-9]+\\>\\)" s)
                       (match-string 1 s)))
         (version (or eepy-python-info-foce-version py-version))
         ;; Whether info files have a Python version suffix, e.g. in Debian.
         (versioned
          (with-temp-buffer
            (Info-mode)
            ;; First look for Info files corresponding to the version
            ;; of the interpreter we're running.
            (condition-case ()
                ;; Don't use `info' because it would pop-up a *info* buffer.
                (progn
                  (Info-goto-node (format "(python%s-lib)Miscellaneous Index"
                                          version))
                  t)
              (error
               ;; Otherwise see if we actually have an un-versioned one.
               (condition-case ()
                   (progn
                     (Info-goto-node
                      (format "(python-lib)Miscellaneous Index" version))
                     nil)
                 (error
                  ;; Otherwise look for any versioned Info file.
                  (condition-case ()
                      (let (found)
                        (dolist (dir (or Info-directory-list
                                         Info-default-directory-list))
                          (unless found
                            (let ((file (car (file-expand-wildcards
                                              (expand-file-name "python*-lib*"
                                                                dir)))))
                              (if (and file
                                       (string-match
                                        "\\<python\\([0-9]+\\.[0-9]+\\>\\)-"
                                        file))
                                  (setq version (match-string 1 file)
                                        found t)))))
                        found)
                    (error)))))))))
    (info-lookup-maybe-add-help
     :mode 'python-mode
     :regexp "[[:alnum:]_]+"
     :doc-spec
     ;; Fixme: Can this reasonably be made specific to indices with
     ;; different rules?  Is the order of indices optimal?
     ;; (Miscellaneous in -ref first prefers lookup of keywords, for
     ;; instance.)
     (if versioned
	 ;; The empty prefix just gets us highlighted terms.
	 `((,(concat "(python" version "-ref)Miscellaneous Index"))
	   (,(concat "(python" version "-ref)Module Index"))
	   (,(concat "(python" version "-ref)Function-Method-Variable Index"))
	   (,(concat "(python" version "-ref)Class-Exception-Object Index"))
	   (,(concat "(python" version "-lib)Module Index"))
	   (,(concat "(python" version "-lib)Class-Exception-Object Index"))
	   (,(concat "(python" version "-lib)Function-Method-Variable Index"))
	   (,(concat "(python" version "-lib)Miscellaneous Index")))
       '(("(python-ref)Miscellaneous Index")
	 ("(python-ref)Module Index")
	 ("(python-ref)Function-Method-Variable Index")
	 ("(python-ref)Class-Exception-Object Index")
	 ("(python-lib)Module Index")
	 ("(python-lib)Class-Exception-Object Index")
	 ("(python-lib)Function-Method-Variable Index")
	 ("(python-lib)Miscellaneous Index"))))))

(eval-after-load "info-look" '(python-init-info-look))

;;** CHM (only available on Windows)
(defcustom eepy-chm-file-path "python.chm"
  :group 'eepy
  :type 'filename
  "Path to pythonXXx.chm")

;;You need to install `keyhh' utility
;; http://www.keyworks.net/keyhh.htm
;;KeyHH -MyHelp -#klink "ActiveX Control Wizard" htmlhelp.chm
(defun chm-keyword-lookup (typeid help-file symbol)
  "lookup a keyword in a CHM file and display it"
  (interactive)
  (start-process "CHM keyword lookup" nil
                 "keyhh.exe"
                 (concat "-" typeid) "-#klink"
                 (format "'%s'" symbol)
                 help-file ))

(defun eepy-chm-keyword-help (symbol)
  (interactive
   (list (read-string "Help on symbol(CHM): "
                      (or (thing-at-point 'symbol) ""))))
  (chm-keyword-lookup "EEPY" eepy-chm-file-path symbol))

;;** pylookup
;;TODO: http://taesoo.org/Opensource/Pylookup

;;** haddoc
;;haddoc: Browse HTML Python Documentation From Emacs
;;TODO: http://furius.ca/haddoc/

;;** pydoc
;;stolen from http://stackoverflow.com/questions/1054903/how-do-you-get-python-documentation-in-texinfo-info-format
(defun pydoc (&optional arg)
  (interactive)
  (when (not (stringp arg))
    (setq arg (thing-at-point 'word)))

  (setq cmd (concat "pydoc " arg))
  (ad-activate-regexp "auto-compile-yes-or-no-p-always-yes")
  (shell-command cmd)
  (setq pydoc-buf (get-buffer "*Shell Command Output*"))
  (switch-to-buffer-other-window pydoc-buf)
  (python-mode)
  (ad-deactivate-regexp "auto-compile-yes-or-no-p-always-yes")
)

(provide 'eepy-doc)

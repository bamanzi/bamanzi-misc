;; add this to your .emacs file:

(when (load "flymake" t)
  (let* ((cwd (if load-file-name
                  (file-name-directory load-file-name)
                default-directory))
         (script-file (concat cwd "pyflymake.py"))
         (target-file "~/.emacs.d/flymake/pyflymake.py"))
    (if (file-exists-p script-file)
        (progn
          ;; copy pyflymake.py to ~/.emacs.d/flymake
          (unless (file-directory-p (file-name-directory target-file))
            (make-directory (file-name-directory target-file) 'parents))
          (unless (file-exists-p target-file)
            (copy-file script-file target-file))
         
          (if (memq system-type '(ms-dos windows-nt))
              ;; copy pyflymake.bat
              (copy-file (concat (concat cwd "pyflymake.bat")
                                 (file-name-directory target-file)))
            ;; make sure pyflymake.py executable
            (unless (file-executable-p target-file)
              (chmod target-file 555))))
      (message "Error: pyflymake.py not found.")))
    
  (defun flymake-pylint-init (&optional trigger-type)
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name)))
	   (options (when trigger-type (list "--trigger-type" trigger-type))))
      (list "~/.emacs.d/flymake/pyflymake.py" (append options (list local-file)))))

  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pylint-init)))

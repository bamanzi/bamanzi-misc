(defun my-add-sub-dir-to-load-path (dir)
  (let ((default-directory
          (concat (file-name-directory (or load-file-name "./")) dir)))
    (if (file-exists-p default-directory)
	(progn
	  (add-to-list 'load-path default-directory)
	  (normal-top-level-add-subdirs-to-load-path)))))

(defun my-add-sub-dir-to-info-path (dir)
  (let ((topdir
          (concat (file-name-directory (or load-file-name "./")) dir)))
    (if (file-directory-p topdir)
        (mapc '(lambda (pkgdir)
                 (unless (file-exists-p (expand-file-name ".nosearch" pkgdir))
                   (let ((pkginfodir (expand-file-name "info" "packages/cedet")))
                     (when (file-directory-p pkginfodir)
                       (message "Adding %s to Info-default-directory-list. " pkginfodir)
                       (add-to-list 'Info-default-directory-list pkginfodir)))))
              (directory-files dir t "^[^.]")))))

;; common
(my-add-sub-dir-to-load-path "common")

;; package
(my-add-sub-dir-to-load-path "packages")

(my-add-sub-dir-to-load-path "proglangs")

;; emacs-xx
(my-add-sub-dir-to-load-path (concat "emacs-" (format "%d" emacs-major-version)))
;; emacs-xx.x
(my-add-sub-dir-to-load-path (concat "emacs-" emacs-version))


   
;; load each file in site-start.d     
;; (mapc (directory-files  
;;        (concat (file-name-directory (or load-file-name "./")) "site-start.d")            
;;        t "\\.el\\'"))      
;; (load file) 
;; )

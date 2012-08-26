(require 'projectile)
(require 'anything)

;; helper for anything
(defun anything-c-projectile-files ()
  "Generates a list of files in the current project"
  (projectile-get-project-files
   (projectile-get-project-root)))

(defvar anything-c-source-projectile-files
  `((name . "Projectile Files")
    (candidates . anything-c-projectile-files)
    (type . file)
    (match . anything-c-match-on-basename)
    (mode-line . "Projectile Files")
    )
  "Anything source definition")

(defun anything-with-projectile-files ()
  "Example function for calling anything with the projectile file source.

Use this function as example and create your own list of anything sources.
"
  (interactive)
  (anything :sources '(anything-c-source-projectile-files)))

(defvar anything-c-source-projectile-buffers
  `((name . "Projectile buffers")
    (candidates . projectile-get-project-buffer-names)
    (type . buffer)
    (mode-line . "Projectile Buffers")
    )
  "Anything source definition")

(defun anything-with-projectile-buffers ()
  "Call anything with the projectile buffer source."
  (interactive)
  (anything :sources '(anything-c-source-projectile-buffers)))

(provide 'projectile-anything)

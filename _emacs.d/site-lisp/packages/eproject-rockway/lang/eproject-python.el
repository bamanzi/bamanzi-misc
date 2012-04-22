;;; eproject-python.el --- python support for eproject

;; Copyright (C) 2011  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@googlemail.com>
;; Keywords: tools, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Python extensions for eproject.

;; The extension defines a new project types named "python".  This project type
;; looks for standard distutils/setuptools "setup.py" files.

;;; Rope support

;; If a file of a "python" project is visited the first time, the rope project
;; of the current project is opened if rope is available.  This makes rope
;; functionality like refactoring or completion available to the project's
;; files.

;;; Code:

(require 'eproject)

;; Python projects
(define-project-type python (generic)
  (look-for "setup.py")
  :relevant-files ("\\.py$" "\\.rst$")
  :irrelevant-files ("\\.py[co]$"))

;; remember the last automatically opened rope project, to avoid
;; re-opening it all the time
(defvar eproject-python--auto-rope-project nil)

(defun eproject-python--open-rope-project ()
  "Open the rope project corresponding to the project the current
buffer belongs to.

If this function is subsequently called for files of the same
project, the rope project is opened only once for all files."
  (when (fboundp 'rope-open-project)
    (let ((root (eproject-root)))
      (unless (equal eproject-python--auto-rope-project root)
        (rope-open-project root)
        (setq eproject-python--auto-rope-project root)))))

;; automatically open rope project when visiting a Python project file
(add-hook 'python-project-file-visit-hook 'eproject-python--open-rope-project)


(provide 'eproject-python)
;;; eproject-python.el ends here

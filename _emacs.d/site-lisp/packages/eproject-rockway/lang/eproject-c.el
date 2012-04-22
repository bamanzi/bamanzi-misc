;;; eproject-c.el --- C language tools for eproject

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

;; C language extensions for eproject.

;; This eproject extensions provides eproject support for c-mode languages.

;;; CMake detection

;; The extension detects CMake build systems by looking for a "CMakeLists.txt"
;; in the project root and sets the project attributes `:is-cmake' for such
;; projects.  You can use this attribute to set a specific `compile-command'
;; for CMake projects.

;;; Style support

;; You can enable per-project C styles by customizing `eproject-c-style-alist'.
;; It maps C style names to project names.  For projects contained in this
;; list, the corresponding C style is enabled for files in C modes.

;;; Code:

(require 'eproject)

;; special projects
;; identify CMakeLists projects
(defun eproject-c--cmake-project-p (root)
  "Return t if the project at ROOT directory is a CMake project.
Otherwise return nil."
  (file-exists-p (format "%s/CMakeLists.txt" root)))

;; find cmake projects
(define-project-attribute #'eproject-c--cmake-project-p '(:is-cmake t))

;; C projects
(defun eproject-c-set-c-style ()
  "Set the c-style for the current project.

Set project styles in `eproject-c-style-alist'."
  (interactive)
  (let ((style (assoc (eproject-name) eproject-c-style-alist)))
    (when style (c-set-style (cdr style)))))

(defcustom eproject-c-style-alist nil
    "Association list defining C styles for projects.

In an element (PROJECT . STYLE), PROJECT is the name of a project
as returned by `eproject-name', and STYLE is the name of a c
style.  You can use any style in `c-style-alist', including your
own styles."
    :type '(alist :key-type string :value-type string)
    :group 'c
    :group 'eproject
    :require 'eproject-c)

(defun eproject-c--c-mode-setup ()
  "Setup eproject in C modes."
  (ignore-errors
    (eproject-maybe-turn-on)
    (eproject-c-set-c-style)))
(add-hook 'c-mode-common-hook 'eproject-c--c-mode-setup)

(provide 'eproject-c)
;;; eproject-c.el ends here

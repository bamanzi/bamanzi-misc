;;; eproject-lunaryorn.el --- eproject extensions

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

;; Extensions for eproject (https://github.com/jrockway/eproject)

;;; Code:

(require 'compile)
(require 'full-ack)

;; mercurial project type
(define-project-type generic-mercurial (generic) (look-for ".hg")
  :irrelevant-files ("^[.]" "^[#]" ".hg/"))

;; generic utilities
(defun eproject-lunaryorn-run ()
  "Run the command in the :run-command eproject attribute in the
project directory.

Use this function and the project attribute :run-command to run
the main executable of your project."
  (interactive)
  (let ((run-command (eproject-attribute :run-command))
        (default-directory (eproject-root))
        (buffer-name (format "*%s run*" (eproject-name))))
    (when run-command
      (compilation-start run-command nil (lambda (mode-name) buffer-name))
    )))

(defun eproject-lunaryorn-ack (regexp)
  "Search all files in the current project for REGEXP, using Ack."
  (interactive "sRegexp ack: ")
  (let* ((root (eproject-root))
         (default-directory root)
         (files (eproject-list-project-files-relative root)))
    (ack regexp t default-directory)))

(defun eproject-lunaryorn-shell ()
  "Start a new shell in the directory of the current project."
  (interactive)
  (let ((buffer-name (format "*shell: %s" (eproject-name)))
        (default-directory (eproject-root)))
    (shell buffer-name))
  )

(provide 'eproject-lunaryorn)
;;; eproject-lunaryorn.el ends here

;;; wings-theme.el --- custom theme for faces

;; Copyright (c) 2011 Eugene Rwagasore <rwagasore@gmail.com>

;; wings-theme is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with wings-theme.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme wings
  "A high-contrast theme with a dark background.
   Basic, Font Lock, Isearch, Gnus, and Message faces are included.
   The default face foreground is wheat, with other faces in shades
   of green, brown, and blue.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'wings
   `(default ((,class (:foreground "#F8F8F8" :background "grey15"))))
   `(cursor ((,class (:foreground "black" :background "grey15"))))

   ;; Highlighting faces
   `(highlight ((,class (:foreground "white" :background "MediumPurple4"))))
   `(region ((,class (:foreground "white" :background "DarkGoldenrod4"))))
   `(secondary-selection ((,class (:background "dark slate gray"))))
   `(isearch ((,class (:foreground "white" :background "dark goldenrod"))))
   `(lazy-highlight ((,class (:background "grey25"))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "LightSteelBlue"))))
   `(font-lock-comment-face ((,class (:foreground "SpringGreen3"))))
   `(font-lock-constant-face ((,class (:foreground "turquoise"))))
   `(font-lock-function-name-face ((,class (:foreground "pale green"))))
   `(font-lock-keyword-face ((,class (:foreground "peru"))))
   `(font-lock-string-face ((,class (:foreground "dark khaki"))))
   `(font-lock-type-face ((,class (:foreground "aquamarine"))))
   `(font-lock-variable-name-face ((,class (:foreground "yellow green"))))
   `(font-lock-warning-face ((,class (:foreground "salmon1"))))

   ;; Button and link faces
   `(button ((,class (:underline t :foreground "cyan"))))
   `(link ((,class (:underline t :foreground "cyan"))))
   `(link-visited ((,class (:underline t :foreground "dark cyan"))))
   
   ;; Gnus faces
   `(gnus-header-content ((,class (:weight normal :foreground "yellow green"))))
   `(gnus-header-from ((,class (:foreground "pale green"))))
   `(gnus-header-subject ((,class (:foreground "pale turquoise"))))
   `(gnus-header-name ((,class (:foreground "dark sea green"))))
   `(gnus-header-newsgroups ((,class (:foreground "dark khaki"))))

   ;; Message faces
   `(message-header-name ((,class (:foreground "dark turquoise"))))
   `(message-header-cc ((,class (:foreground "yellow green"))))
   `(message-header-other ((,class (:foreground "dark khaki"))))
   `(message-header-subject ((,class (:foreground "pale turquoise"))))
   `(message-header-to ((,class (:foreground "pale green"))))
   `(message-cited-text ((,class (:foreground "SpringGreen3"))))
   `(message-separator ((,class (:foreground "deep sky blue"))))))

(provide-theme 'wings)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; wings-theme.el ends here


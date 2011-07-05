;;; color-theme-radiance.el --- Color theme for Emacs

;;; Copyright (C) 2011 by Miloš Hadžić

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.


;;; Code:

(deftheme radiance
  "A Theme that should look nice with the Radiance light theme in Ubuntu.")

(custom-theme-set-faces
 'radiance
 '(default ((t (:background "#ffffff" :foreground "#000000"))))
 '(cursor ((t (:background "#aea79f" :foreground "#aea79f"))))
 '(region ((t (:background "#d6d5cf" :foreground "#333333"))))
 '(mode-line ((t (:background "#cfcfcf" :foreground "#000000"))))
 '(mode-line-inactive ((t (:background "#b6b6b6" :foreground "#333333"))))
 '(fringe ((t (:background "#f6f5ef"))))
 '(minibuffer-prompt ((t (:foreground "#f47321" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#62124b"))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#6e675f"))))
 '(font-lock-constant-face ((t (:foreground "#f68d47"))))
 '(font-lock-function-name-face ((t (:foreground "#860e73" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#f47321" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#dd1144"))))
 '(font-lock-type-face ((t (:foreground "#83a525"))))
 '(font-lock-variable-name-face ((t (:foreground "#8b005a"))))
 '(font-lock-warning-face ((t (:foreground "#e80000" :weight bold))))
 '(isearch ((t (:background "#fedfc7" :foreground "#333333"))))
 '(lazy-highlight ((t (:background "#f6f5ef"))))
 '(link ((t (:foreground "#0086b3" :underline t))))
 '(link-visited ((t (:foreground "#800080" :underline t))))
 '(button ((t (:background "#f6f5ef" :weight bold))))
 '(header-line ((t (:background "#ffffff" :foreground "#62124b"))))
 '(hl-line ((t (:background "#f6f5ef")))))

(provide-theme 'radiance)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; radiance-theme.el  ends here

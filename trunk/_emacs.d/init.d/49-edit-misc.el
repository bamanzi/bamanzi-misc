
;;;_ S(@* "emacs options")

(defun bmz/toggle-show-paren-style ()
  (interactive)
  (if (eq show-paren-style 'parenthesis)
      (setq show-paren-style 'expression)
    (setq show-parent-style 'parenthesis))
  (message "show-paren-style switched to %s." show-paren-style))

(global-set-key (kbd "<C-f10> p") 'bmz/toggle-show-paren-style)
          

;;;_ S(@* "minor modes")

;; (global-set-key (kbd "<f10> c") 'highlight-changes-visible-mode)
;; (global-set-key (kbd "<f10> f") 'auto-fill-mode)
;; (global-set-key (kbd "<f10> p") 'show-paren-mode)
;; (global-set-key (kbd "<f10> w") 'whitespace-mode)
;; (global-set-key (kbd "<f10> h") 'hs-minor-mode)
;; (global-set-key (kbd "<f10> o") 'outline-minor-mode)
;; (global-set-key (kbd "<f10> v") 'toggle-viper-mode)
;; (global-set-key (kbd "<f10> C-w") 'visual-line-mode)
;; (global-set-key (kbd "<f10> t") 'toggle-truncate-lines)
;; (global-set-key (kbd "<f10> l") 'linum-mode)



;;;_. 3rd-party modules
(global-set-key (kbd "<f10> C") 'auto-complete-mode)

(autoload 'highlight-indentation "highlight-indentation" "Toggle highlight indentation." t)
(autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
(autoload 'idle-highlight "idle-highlight" nil t)
(autoload 'rainbow-delimiters "rainbow-delimiters" nil t)
(autoload 'visible-mark-mode "visible-mark" nil t)
(autoload 'drag-stuff-mode "drag-stuff" nil t)
(autoload 'setnu-mode "setnu" "vi-style line numbers" t)

(global-set-key (kbd "<f10> I") 'highlight-indentation)
(global-set-key (kbd "<f10> P") 'highlight-parentheses-mode)
(global-set-key (kbd "<f10> H") 'idle-highlight)
(global-set-key (kbd "<f10> R") 'rainbow-delimiters)
(global-set-key (kbd "<f10> M") 'visibile-mark-mode)
(global-set-key (kbd "<f10> D") 'drag-stuff-mode)
(global-set-key (kbd "<f10> N") 'setnu-mode)

     
;;;_ S(@* "keybindings")


;;;_. misc keys


;(global-set-key (kbd "<f3> C-f") 'ffap-other-window)


(global-set-key (kbd "C-c d") 'diff-buffer-with-file)




;;;_ S(@* "editing")
;; make M-z behave more as zap-up-to-char
(defun zap-up-to-char (arg char)
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
    (interactive "p\ncZap to char: ")
    (zap-to-char arg char)
    (insert char)
    (forward-char -1))

(global-set-key (kbd "M-z") 'zap-up-to-char)

(defun zap-back-to-char (arg char)
  (interactive "p\ncBack-zap to char: ")
  (zap-to-char (- arg) char))

(global-set-key (kbd "ESC M-z") 'zap-back-to-char)


;;;_ S(@* "search")
;;;_. hidesearch
(autoload 'hidesearch "hidesearch" "Incrementally show only lines in file based on what user types." t)
(autoload 'show-all-invisible "hide-lines" "Show all areas hidden by the filter-buffer command" t)
(autoload 'hide-non-matching-lines "hide-lines" "Hide lines that don't match the specified regexp." t)

(global-set-key (kbd "C-c C-s") 'hidesearch)
(global-set-key (kbd "C-c C-a") 'show-all-invisible)

;;FIXME: anything-occur is better?

;;;_ S(@* "secondary selection")
(autoload 'secondary-to-primary "second-sel" nil t)
(autoload 'primary-to-secondary "second-sel" nil t)
(autoload 'secondary-swap-region "second-sel" nil t)
(define-key search-map (kbd "2") 'primary-to-secondary)
(define-key search-map (kbd "1") 'secondary-to-primary)
(define-key search-map (kbd "`") 'secondary-swap-region)

;;(define-key search-map (kbd "~") 'transpose-selections)

;;;_ S(@* "misc")
;;;_. extend selection incrementally (ergoemacs-functions.el)
;; http://xahlee.org/emacs/syntax_tree_walk.html
(autoload 'extend-selection "ergoemacs-functions" nil t)
(global-set-key (kbd "C-.") 'extend-selection)
;; see also: mark-sexp (C-M-SPC), mark-word (M-@)



;;;_. quickly swap lines
;; Move line up/down. Stolen from org-mode's M-up/down
;; TODO: support region (move region line up/down)
;; see also:  (@file :file-name "drag-stuff.el" :to "define-minor-mode drag-stuff-mode")
(defun swap-line-up ()
  "Swap the current line with the line above."
  (interactive)
  (transpose-lines 1)
  (beginning-of-line -1))

(defun swap-line-down ()
  "Swap current line with the line below."
  (interactive)
  (beginning-of-line 2) (transpose-lines 1) (beginning-of-line 0))

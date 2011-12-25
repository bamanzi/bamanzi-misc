;;* some special highlights & overlay


;;** whitespaces
(global-set-key (kbd "<f10> ws") 'whitespace-mode)

;;*** develock: programmers whitespace-mode
(autoload 'develock-mode  "develock" "Toggle Develock mode." t)

;;** highlight mark
;;visible-mark-mode

;;** highlight current position
;;*** current line

(autoload 'highline-mode "highline"
  "minor mode to highlight current line in buffer" t)

(autoload 'hl-spotlight-mode "hl-spotlight" "spotlight current few lines." t)

;;*** current column
(autoload 'column-marker-1 "column-marker"
  "Highlight a column." t)

;;*** target column
(autoload 'highlight-beyond-fill-column "highlight-beyond-fill-column"
  "Setup this buffer to highlight beyond the `fill-column'." t)

(autoload 'fci-mode "fill-column-indicator" "Undocumented." t)

;;*** both
;; crosshair


;;** highlight matching paren/pair

(defun bmz/toggle-show-paren-style ()
  (interactive)
  (if (eq show-paren-style 'parenthesis)
      (setq show-paren-style 'expression)
    (setq show-parent-style 'parenthesis))
  (message "show-paren-style switched to %s." show-paren-style))

(global-set-key (kbd "<C-f10> p") 'bmz/toggle-show-paren-style)


(autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
(global-set-key (kbd "<f10> hp") 'highlight-parentheses-mode)

(autoload 'rainbow-delimiters "rainbow-delimiters" nil t)
(global-set-key (kbd "<f10> rd") 'rainbow-delimiters)

;;** highlight symbol
(autoload 'highlight-regexp  "hi-lock" "Set face of each match of REGEXP to FACE." t)
(autoload 'highlight-phrase  "hi-lock" "Set face of each match of phrase REGEXP to FACE." t)
(autoload 'highlight-lines-matching-regexp  "hi-lock" "Set face of all lines containing a match of REGEXP to FACE." t)
(autoload 'unhighlight-regexp  "hi-lock" "Remove highlighting of each match to REGEXP set by hi-lock." t)

;;*** manually
(autoload 'highlight-symbol-get-symbol "highlight-symbol" nil t)
(autoload 'highlight-symbol-next       "highlight-symbol" nil t)
(autoload 'highlight-symbol-prev       "highlight-symbol" nil t)
(autoload 'highlight-symbol-at-point   "highlight-symbol" "Toggle highlighting of the symbol at point." t)

(idle-require 'highlight-symbol)

(global-set-key (kbd "C-c j")          'highlight-symbol-at-point)
(define-key search-map (kbd "j")        'highlight-symbol-at-point)
(define-key search-map (kbd "<up>")   'highlight-symbol-prev)
(define-key search-map (kbd "<down>") 'highlight-symbol-next)

(global-set-key (kbd "<double-mouse-1>")  'highlight-symbol-at-point)
(global-set-key (kbd "<S-wheel-up>")      'highlight-symbol-prev)
(global-set-key (kbd "<S-wheel-down>")    'highlight-symbol-next)

;;*** automatically highlight symbol at point for a short time
(autoload 'idle-highlight "idle-highlight" nil t)
(global-set-key (kbd "<f10> ih") 'idle-highlight)

;;See also: light-symbol.el


;;** in-buffer bookmark with visual effects
(autoload 'bm-toggle "bm" "Toggle bookmark at point." t)

(idle-require 'bm)

(progn
  (global-set-key (kbd "<C-f2>") 'bm-toggle)
  (global-set-key (kbd "<M-f2>") 'bm-next)
  (global-set-key (kbd "<S-f2>") 'bm-previous)
  (global-set-key (kbd "<f2> r") 'bm-bookmark-regexp)
  (global-set-key (kbd "<f2> c") 'bm-remove-all-current-buffer)

  (global-set-key (kbd "<left-fringe> <S-mouse-1>")     'bm-toggle-mouse)
  (global-set-key (kbd "<left-fringe> <S-wheel-up>")    'bm-previous-mouse)
  (global-set-key (kbd "<left-fringe> <S-wheel-down>")  'bm-next-mouse)
  (global-set-key (kbd "<left-fringe> <S-mouse-2>")     'bm-show)

  (global-set-key (kbd "<f2> <f2>") 'bm-next)

  (global-set-key (kbd "<H-f2>") 'bm-show)  
  (global-set-key (kbd "<f2> l") 'bm-show)

  (if (fboundp 'anything-bm-list)
      (global-set-key (kbd "<f2> l") 'anything-bm-list))
  )

;;*** linemark.el from CEDET
;(idle-require 'linemark) 
(eval-after-load "linemark"
  `(progn        
     (define-key global-map (kbd "<f2> t") 'viss-bookmark-toggle)
     (define-key global-map (kbd "<f2> n") 'viss-bookmark-prev-buffer)
     (define-key global-map (kbd "<f2> p") 'viss-bookmark-next-buffer)
     (define-key global-map (kbd "<f2> c") 'viss-bookmark-clear-all-buffer)

     (define-key global-map (kbd "<f2> <f2>") 'viss-bookmark-next-buffer)
     ))

(autoload 'highlight-indentation "highlight-indentation" "Toggle highlight indentation." t)
(global-set-key (kbd "<f10> hi") 'highlight-indentation)


;;** linkd : links to file/man/info/url
;; e.g. (@url :file-name "http://www.emacswiki.org/emacs/LinkdMode" :display "LinkdMode - EmacsWiki")
;; (linkd-follow (@file :file-name "linkd.el" :to "linkd-mode"))
(autoload 'linkd-mode        "linkd" "Create or follow hypertext links." t)
(autoload 'linkd-insert-link "linkd" "Insert a link." t)
(autoload 'linkd-follow      "linkd" "Follow the link represented by SEXP." nil)

(eval-after-load "linkd"
  `(progn
     ;;_.. linkd icons path
     (let ( (dir (concat (file-name-directory (locate-library "linkd")) "icons")) )
        (when (file-exists-p dir)
          (setq linkd-icons-directory dir)
          (setq linkd-use-icons t)))

      ;;_.. restore [mouse-4] (for mwheel-scroll, linkd bind it to `linkd-back')
      (when (eq window-system 'x)
        (define-key linkd-map [mouse-4] nil)
        (define-key linkd-overlay-map [mouse-4] nil))
        
;;      (add-hook 'emacs-lisp-mode-hook 'linkd-enable)
;;      (add-hook 'python-mode-hook 'linkd-enable)
;;      (add-hook 'espresso-mode-hook 'linkd-enable)

      (progn  ;; change linkd faces according to current theme
        ;; (copy-face 'org-link 'linkd-generic-link )
        ;; (copy-face 'org-link 'linkd-generic-link-name)
        
        (set-face-foreground 'linkd-generic-link (face-foreground 'org-link))
        (set-face-underline  'linkd-generic-link t)
        
        (set-face-foreground 'linkd-generic-link-name (face-foreground 'org-link))
        (set-face-underline  'linkd-generic-link-name t)
        
        (set-face-foreground 'linkd-generic-link-name (face-foreground 'org-link))
        (set-face-underline  'linkd-generic-link-name t)
        
        (set-face-foreground 'linkd-tag-name (face-foreground 'org-done))
        )
      ))

;; turn
;;    http://www.emacswiki.org/emacs/LinkdMode
;; into:
;;    (linkd-follow (@url :file-name "http://www.emacswiki.org/emacs/LinkdMode"))
(defun linkd-wrap-url (begin end)
  (interactive "r")
  (goto-char end)
  (insert-string "\"))")
  (goto-char begin)
  (insert-string "(linkd-follow (@url :file-name \""))


;;** highlight FIXME/TODO etc in any file
;;FROM: http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(add-hook 'find-file-hook
          '(lambda ()
             (font-lock-add-keywords
              nil
              '(("\\<\\(FIXME\\|TODO\\|NOTE\\|BUG\\):" 1 font-lock-warning-face prepend)
                ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))))

;;*** fixme-mode 
(autoload 'fixme-mode "fixme-mode" "A minor mode for making FIXME and other warnings stand out" t)
(autoload 'fic-ext-mode "fic-ext-mode"  "minor mode for highlighting FIXME/TODO in comments" t)
  
;;** pulse: temperarily highlight a line/region, to draw user's attension
(idle-require 'pulse)
(unless (fboundp 'cedet-called-interactively-p)
  (defalias 'cedet-called-interactively-p 'called-interactively-p))
(setq pulse-command-advice-flag t)

(defadvice imenu (after pulse-advice activate)
  "Cause the line that is `imenu'd to pulse when the cursor gets there."
  (when (and pulse-command-advice-flag (cedet-called-interactively-p))
    (pulse-momentary-highlight-one-line (point))))


;;** misc
;;*** rainbow-mode: colorize strings like 'red', "#3303c4"
(autoload 'rainbow-mode "rainbow-mode" "Colorize strings that represent colors." t)
(global-set-key (kbd "<f10> rb") 'rainbow-mode)

;;*** markerpen / himark
;;TODO: foo

;;***

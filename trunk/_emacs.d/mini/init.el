(when nil
  (progn
    ;; highlight headers in this file
    (highlight-lines-matching-regexp "^;;\\* "    'org-level-1)
    (highlight-lines-matching-regexp "^;;\\*\\* " 'org-level-2)
    (highlight-lines-matching-regexp "^;;\\*\\*\\* " 'org-level-3)
    (setq outline-regexp "^;;[\*]+ ")
    )    ;;<- put cursor here, press C-x C-e
  )


(if (file-exists-p "~/.emacs.d/site-lisp/site-start.el")
    (load-file "~/.emacs.d/site-lisp/site-start.el"))

(let ((this_dir (if load-file-name
                    (file-name-directory load-file-name)
                  default-directory)))
  (add-to-list 'load-path (concat this_dir "lisp"))
  (add-to-list 'load-path (concat this_dir "lisp/testing")))


;;** key bindings

;;***  key modifiers and prefix keys

;;***  <f1> .. <f12> as prefix key
(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10>") 'menu-bar-open)

;;** emacs enviroment
(global-set-key (kbd "ESC ESC e r") 'eval-region)
(global-set-key (kbd "ESC ESC e b") 'eval-buffer)
(global-set-key (kbd "ESC ESC l l") 'load-library)
(global-set-key (kbd "ESC ESC f l") 'find-library)


;;** emacs environmen
(defun describe-keymap (keymap)
    (interactive
     (list (intern (completing-read "Keymap: " obarray
                                    (lambda (m) (and (boundp m) (keymapp (symbol-value m))))
                                    t nil 'variable-name-history))))
    (with-output-to-temp-buffer "*Help*"
      (princ (substitute-command-keys (concat "\\{" (symbol-name keymap) "}")))
      ))
(define-key help-map (kbd "M-k") 'describe-keymap)

(define-key global-map (kbd "<C-f10> g") 'customize-group)
(define-key global-map (kbd "<C-f10> v") 'customize-variable)
(define-key global-map (kbd "<C-f10> f") 'customize-face)

;;** gui options

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(setq frame-title-format '("%b%* (%m) - Emacs "
                           (:eval emacs-version)
                           (:eval (if buffer-file-name
                                      (format " - [%s]" buffer-file-name)
                                    ""))))

(global-set-key (kbd "<C-M-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-M-wheel-down>") 'text-scale-decrease)


;;** files & buffers
(global-set-key (kbd "C-c C-b") 'ibuffer)

(global-set-key (kbd "<C-tab>") 'previous-buffer)
(global-set-key (kbd "<C-S-tab>") 'next-buffer)
(global-set-key (kbd "<f12> <left>")    'previous-buffer)
(global-set-key (kbd "<f12> <right>")   'next-buffer)

;;***  recentf
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-menu-path '("File"))
(recentf-mode t)

;;*** vc
(unless (fboundp 'vc-svn-root)  ;;fix emacs-23's svn support
  (defun vc-svn-root (file)
    (vc-find-root file vc-svn-admin-directory)))

;;** windows
;;***  winner-mode
(setq winner-dont-bind-my-keys t)
(winner-mode t)
(global-set-key (kbd "<f11> C-z") 'winner-undo)
(global-set-key (kbd "<f11> C-y") 'winner-redo)

;;*** windmove
(require 'windmove)
(progn
  (global-set-key (kbd "<f11> <up>")    'windmove-up)
  (global-set-key (kbd "<f11> <down>")  'windmove-down)
  (global-set-key (kbd "<f11> <left>")  'windmove-left)
  (global-set-key (kbd "<f11> <right>") 'windmove-right)
  )

;;*** windresize
(autoload 'windresize "windresize" "Resize windows interactively." t)
(setq windresize-default-increment 4)
(global-set-key (kbd "<f11> RET") 'windresize)


;;***  tabbar
(eval-after-load "tabbar"
  `(progn
     (tabbar-mode t)
     (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward)
     (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-backward)
     (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-forward-group)
     (define-key tabbar-mode-map (kbd "<C-S-M-tab>") 'tabbar-backward-group)

     (define-key tabbar-mode-map (kbd "<f12> <up>")      'tabbar-press-home)
     (define-key tabbar-mode-map (kbd "<f12> <left>")    'tabbar-backward-tab)
     (define-key tabbar-mode-map (kbd "<f12> <right>")   'tabbar-forward-tab)
     (define-key tabbar-mode-map (kbd "<f12> <M-left>")  'tabbar-backward-group)
     (define-key tabbar-mode-map (kbd "<f12> <M-right>") 'tabbar-forward-group)
     ))

  (define-key tabbar-mode-map (kbd "<f12> <right>") 'tabbar-forward-tab)
  (define-key tabbar-mode-map (kbd "<f12> <left>")  'tabbar-backward-tab)  
  (define-key tabbar-mode-map (kbd "<f12> <up>")    'tabbar-backward-group)
  (define-key tabbar-mode-map (kbd "<f12> <down>")  'tabbar-forward-group)

  (if (display-graphic-p)
      (require 'tabbar-ruler nil t))  
  )



;;** editing

;;***  CUA

(transient-mark-mode t)
(setq shift-select-mode t)
(delete-selection-mode t)

(setq cua-enable-cua-keys nil)
;;(setq cua-rectangle-modifier-key 'hyper)  ;;leave C-RET
(cua-mode t)

(global-set-key (kbd "C-x r RET") 'cua-set-rectangle-mark)

(when (eq window-system 'x)
    (setq x-select-enable-clipboard t)
;;  (setq x-select-enable-primary t)
;;    (set-scroll-bar-mode 'right)
    )

(setq mouse-yank-at-point t) ;;rather than the click point

;;***  tab key & indent
(setq tab-always-indent t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;***  parens
(setq show-paren-style 'mixed)
(setq show-paren-mode t)
(show-paren-mode t)

;;***  newline & line-wrap
(setq require-final-newline 't)
(setq-default truncate-lines t)
(setq-default fill-column 100)
;;(auto-fill-mode t)

(global-set-key (kbd "C-c C-w") 'toggle-truncate-lines)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

;;***  changes
(require 'undo-tree nil 'noerror)
(eval-after-load "undo-tree"
  `(progn
      (global-undo-tree-mode t)
      (global-set-key (kbd "C-c C-z") 'undo-tree-undo)
      (global-set-key (kbd "C-c C-y") 'undo-tree-redo)
      ))

(setq highlight-changes-visibility-initial-state nil)
(global-highlight-changes-mode t)

(define-key global-map (kbd "<f10> h c") 'highlight-changes-visible-mode)

(setq diff-switches "-u")    ;;I prefer the unified format
(global-set-key (kbd "C-c d") 'diff-buffer-with-file)

;;***  quickly swap lines
(eval-after-load "drag-stuff"
  `(progn
     ;;    (setq drag-stuff-modifier 'hyper)
     (add-to-list 'drag-stuff-except-modes 'org-mode)
     (drag-stuff-global-mode t)))

(require 'drag-stuff nil t)


;;***  misc

(global-set-key (kbd "C-=") 'align-regexp)


;;** minibuffer

;;***  icomplete
(icomplete-mode t)  ;; completion for minibuffer
                                        ; commands (M-x)
                                        ; variables (C-h v, customize-variable)
                                        ; functions (C-h f)

;;***  ido
(require 'ido)

(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point 'guess)
(ido-mode 'buffers)

;;***  anything
(progn
  (global-set-key (kbd "<f5> r") 'anything-recentf)
  (global-set-key (kbd "<f5> b") 'anything-buffers+)
  (global-set-key (kbd "<f5> B") 'anything-bookmarks)
  (global-set-key (kbd "<f5> l") 'anything-locate)
  (global-set-key (kbd "<f5> c") 'anything-browse-code)
  (global-set-key (kbd "<f5> i") 'anything-imenu)
  (global-set-key (kbd "<f5> o") 'anything-occur)
  )

(eval-after-load "anything-config"    
    `(progn
       (global-set-key (kbd "<f5> a") anything-command-map)

       (define-key global-map (kbd "<f5> |") 'anything-toggle-resplit-window)
       
       ;;enable multiple keyword/regexp match
       ;;(load "anything-match-plugin" t) ;;FIXME: would cause crash?
       ;;(global-set-key (kbd "M-x") 'anything-M-x)

       (define-key minibuffer-local-map (kbd "<f5> <f5>") 'anything-minibuffer-history)
      ))

(unless (require 'anything-config nil t)
  (message "%s: failed to load `anything'." load-file-name))


;;** completion
;;*** emacs built-in
(if (string< "23.1.99" emacs-version) ;; emacs >= 23.2
   (setq tab-always-indent 'complete))

;; Emacs default:
;;   M-TAB - lisp-complete-symbol(<24)/completion-at-point(v24)
;;   M-/ - dabbrev-expand

(global-set-key (kbd "M-/") 'hippie-expand)

;;***  auto-compelte
(eval-after-load "auto-complete-config"    
  `(progn
      (ac-config-default)
      (define-key ac-completing-map (kbd "ESC ESC") 'ac-stop)
      
      ;;(add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup)

      (if (load "auto-complete-scite-api" t)
          (add-to-list 'ac-sources 'ac-source-scite-api)
        (message "%s: failed to load `auto-complete-scite-api'." load-file-name)))
  )

(define-key global-map (kbd "<f10> a c") 'auto-complete-mode)

(unless (and (load "auto-complete" t)
             (load "auto-complete-config" t))
  (message "%s: failed to load `auto-complete'." load-file-name))


;;** code folding

;;***  hideshow
(eval-after-load "hideshow"
  `(progn
     (define-key hs-minor-mode-map (kbd "M-+")  'hs-toggle-hiding)
     (define-key hs-minor-mode-map (kbd "<C-mouse-1>") 'hs-mouse-toggle-hiding)
     ))

(define-key global-map (kbd "<f10> h s") 'hs-minor-mode)

(defun bmz/turn-on-hideshow ()
  (interactive)
  (if (display-graphic-p)
      (if (and (require 'hideshowvis nil t)
              (require 'hideshow-fringe nil t))
          (progn
            (hs-minor-mode t)
            (hideshowvis-enable))
        (hs-minor-mode t)        
     )))

(eval-after-load "hideshowvis"
  ` (define-key global-map (kbd "<f10> h s") 'hideshowvis-minor-mode)
  )


;;***  outline
(require 'outline)
(eval-after-load "outline"
  `(progn
     (global-set-key (kbd "C-z")   outline-mode-prefix-map)
     
     (global-set-key (kbd "C-z <up>")     'outline-previous-visible-heading)
     (global-set-key (kbd "C-z <down>")   'outline-next-visible-heading)
     
     (define-key outline-mode-prefix-map (kbd "<left>")  'hide-subtree)
     (define-key outline-mode-prefix-map (kbd "<right>") 'show-subtree)

     (global-set-key (kbd "M-+")            'outline-toggle-children)
     (global-set-key (kbd "<C-wheel-up>")   'outline-previous-visible-heading)
     (global-set-key (kbd "<C-wheel-down>") 'outline-next-visible-heading)
     (global-set-key (kbd "<C-mouse-1>")    'outline-toggle-children)
     (global-set-key (kbd "<C-mouse-3>")    'show-subtree)
     (global-set-key (kbd "<C-mouse-2>")    'show-all)
  ))

(define-key global-map (kbd "<f10> o l") 'outline-minor-mode)

(autoload 'qtmstr-outline-mode "qtmstr-outline"
  "Add left-fringe +/- icons and line overlays for outline-sections." t)

(define-key global-map (kbd "<f10> q o") 'qtmstr-outline-mode)

;;** some visual effect

;;***  highlight-symbol
(require 'highlight-symbol nil t)

(define-key search-map (kbd "j")     'highlight-symbol-at-point)
(define-key search-map (kbd "#")     'highlight-symbol-prev)
(define-key search-map (kbd "*")     'highlight-symbol-next)

(global-set-key (kbd "<double-mouse-1>")  'highlight-symbol-at-point)
(global-set-key (kbd "<S-wheel-up>")      'highlight-symbol-prev)
(global-set-key (kbd "<S-wheel-down>")    'highlight-symbol-next)

(defun occur-current-symbol (arg)
  (interactive "P")
  (occur (thing-at-point 'symbol) arg))

(define-key search-map "O" 'occur-current-symbol)

;;***  bm
(require 'bm nil t)

(progn
  (global-set-key (kbd "<f2> <f2>") 'bm-toggle)
  (global-set-key (kbd "<f2> n")    'bm-next)
  (global-set-key (kbd "<f2> p")    'bm-previous)
  (global-set-key (kbd "<f2> l")    'bm-show)

(global-set-key (kbd "<left-fringe> <C-mouse-1>")     'bm-toggle-mouse)
(global-set-key (kbd "<left-fringe> <C-wheel-up>")    'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <C-wheel-down>")  'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <C-mouse-2>")     'bm-show)

;;*** idle-highlight
(autoload 'idle-highlight "idle-highlight"
  "highlight the word the point is on" t)

(eval-after-load "idle-highlight"
  `(progn
     (if (fboundp 'idle-highlight)
         (add-hook 'find-file-hook 'idle-highlight)
       (add-hook 'find-file-hook 'idle-highlight-mode))
     ))

(require 'idle-highlight nil t)

(define-key global-map (kbd "<f10> i h") 'idle-highlight)

;;** programming

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)


(define-key global-map (kbd "<f10> w f") 'which-func-mode)

(which-func-mode t)

;; move which-func indicator to the start of mode line
(setcar mode-line-format '(which-func-mode which-func-format))

(eval-after-load "which-func"
  `(progn
     (define-key which-func-keymap (kbd "<mode-line> <C-mouse-1>") 'imenu)
     ))

;;***  imenu
(define-key goto-map "i" 'imenu)

(defun anything-goto-symbol ()
  "Show anything list, using current symbol as input to narrow the choices."
  (interactive)
  (anything
   :prompt "Go to:"
   :candidate-number-limit 10
   :input (thing-at-point 'symbol)
   :sources
      '( anything-c-source-imenu
         anything-c-source-browse-code
         anything-c-source-semantic
;;         anything-c-source-etags-select
         )))

(define-key goto-map "s" 'anything-goto-symbol)

;;*** tags
(define-key goto-map "e" 'find-tag)

;;***  compilation
(setq compilation-error-regexp-alist '(gnu java))
(global-set-key (kbd "<C-f9>") 'compile)

;;*** flymake
(eval-after-load "flymake"
  '(require 'flymake-cursor nil t))

(define-key goto-map "`" 'flymake-goto-next-error)
(define-key goto-map "~" 'flymake-goto-prev-error)


;;** buffer navigations
;;*** mark
(global-set-key (kbd "C-`")   'set-mark)
;;(global-set-key (kbd "M-`") 'exchange-point-and-mark)
(global-set-key (kbd "M-`")   'pop-mark)

(global-set-key (kbd "C-`")   'set-mark-command)
(global-set-key (kbd "M-`")   'cua-exchange-point-and-mark)
(global-set-key (kbd "C-M-`") 'pop-to-mark-command)


;;***  recent-jump
(setq rj-column-threshold 100)
(if (load "recent-jump" t)
    (recent-jump-mode t)
  (message "Warning: failed to load `recent-jump' (%s)." load-file-name))

(global-set-key (kbd "C-c <") 'recent-jump-backward)
(global-set-key (kbd "C-c >") 'recent-jump-forward)


;;** major modes
;;*** emacs lisp mode
(eval-after-load "lisp-mode"
  `(progn
     (define-key goto-map (kbd "f") 'find-function-at-point)
     (define-key goto-map (kbd "F") 'find-function)
     (define-key goto-map (kbd "v") 'find-variable-at-point)
     (define-key goto-map (kbd "V") 'find-variable)
     ))

(require 'eldoc)
(require 'eldoc-extension nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;*** python mode

(eval-after-load "python"
  `(progn
     (remove-hook 'python-mode-hook 'wisent-python-default-setup)     
     ))
  
(defun python-reset-imenu ()
  (interactive)
  (if (fboundp 'setq-mode-local)
      (setq-mode-local python-mode
                       imenu-create-index-function 'python-imenu-create-index))
;;  (remove-hook 'python-mode-hook 'semantic-python-setup)
  (setq imenu-create-index-function 'python-imenu-create-index))

(autoload 'highlight-indentation-mode "highlight-indentation"
  "Highlight indentation minor mode, highlights indentation based" t)

(global-set-key (kbd "<f10> h i") 'highlight-indentation-mode)

;;***  org-mode

(setq org-CUA-compatible t)

(setq org-completion-use-ido t
      ;; org-hide-leading-stars t
      org-use-sub-superscripts nil ;;don't use `_' for subscript

      org-export-with-section-numbers nil ;; no numbers in export headings
      org-export-with-toc nil ;; no ToC in export
      org-export-with-author-info nil ;; no author info in export
      org-export-with-creator-info nil ;; no creator info
      org-export-htmlize-output-type 'css ;; separate css
      )

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o c") 'org-capture)



;;** utils

;;*** eshell
;;FIXME:

;;** misc
(column-number-mode t)

;;*** color-theme
;;FIXME:

;;*** iedit
(autoload 'iedit-mode "iedit"
  "Edit multiple regions in the same way simultaneously." t)

;;*** terminal
;;FIXME:

(autoload 'lacarte-execute-menu-command "lacarte"
  "Execute a menu-bar menu command in an alternative way." t)

(define-key global-map (kbd "ESC <f10>") 'lacarte-execute-menu-command)
	 
  

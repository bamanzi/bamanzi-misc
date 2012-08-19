(when nil
  (progn
    ;; highlight headers in this file
    (highlight-lines-matching-regexp "^;;\\* "    'org-level-1)
    (highlight-lines-matching-regexp "^;;\\*\\* " 'org-level-2)
    (highlight-lines-matching-regexp "^;;\\*\\*\\* " 'org-level-3)
    )    ;;<- put cursor here, press C-x C-e
  )


(if (file-exists-p "~/.emacs.d/site-lisp/site-start.el")
    (load-file "~/.emacs.d/site-lisp/site-start.el"))

(add-to-list 'load-path (concat (if load-file-name
                                    (file-name-directory load-file-name))
                                    default-directory) "lisp")

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



;;** files & buffers
(global-set-key (kbd "C-c C-b") 'ibuffer)
(global-set-key (kbd "<C-tab>") 'previous-buffer)
(global-set-key (kbd "<C-S-tab>") 'next-buffer)

;;***  recentf
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-menu-path '("File"))
(recentf-mode t)

;;*** vc
;;...

;;** windows
;;***  winner-mode
(setq winner-dont-bind-my-keys t)
(winner-mode t)
;;(global-set-key (kbd "<f11> C-z") 'winner-undo)
;;(global-set-key (kbd "<f11> C-y") 'winner-redo)

;;*** windresize
(autoload 'windresize "windresize" "Resize windows interactively." t)
(setq windresize-default-increment 4)
(global-set-key (kbd "<f11> RET") 'windresize)


;;***  tabbar
;; ide-skel would group buffers into two: editing buffer, emacs buffer
;;(if window-system
;;    (require 'ide-skel nil t))

;; if you use `ide-skel', don't directly load `tabbar' after `ide-ske'
;; as this would mess up the tab group definition of `ide-skel'
(when (or (featurep 'tabbar)
          (load "tabbar" t))
  (tabbar-mode t)
  (define-key tabbar-mode-map (kbd "<C-tab>")     'tabbar-forward)
  (define-key tabbar-mode-map (kbd "<C-S-tab>")   'tabbar-backward)
  (define-key tabbar-mode-map (kbd "<C-M-tab>")   'tabbar-forward-group)
  (define-key tabbar-mode-map (kbd "<C-S-M-tab>") 'tabbar-backward-group)
  )


;;** key bindings

;;***  key modifiers and prefix keys

(when (eq window-system 'w32)
  ;;(setq w32-lwindow-modifier 'super)
  
  (setq w32-lwindow-modifier 'nil)  ;;<lwindow> used as a prefix key
  (setq w32-pass-lwindow-to-system t) ;;if set to nil, a single press <lwindow> would prevent Start Menu
  (define-key key-translation-map (kbd "<lwindow>") (kbd "<f11>"))

  ;; (setq w32-rwindow-modifier 'alt)      
  (setq w32-rwindow-modifier 'hyper)
  (setq w32-pass-rwindow-to-system nil)
  ;;(define-key key-translation-map (kbd "<rwindow>") (kbd "C-c"))
  
  (setq w32-apps-modifier 'hyper)
  
  (setq w32-scroll-lock-modifier nil)
  )
  
;;FIXME: not work
(when (eq window-system 'x)
  (define-key key-translation-map (kbd "<super>") (kbd "<f11>"))
  )

;;***  <f1> .. <f12> as prefix key

;;** editing

;;***  CUA

(transient-mark-mode t)
(setq shift-select-mode t)
(delete-selection-mode t)

(setq cua-enable-cua-keys nil)
;;(setq cua-rectangle-modifier-key 'hyper)  ;;leave C-RET
(cua-mode t)

(global-set-key (kbd "C-c RET") 'cua-set-rectangle-mark)

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
(if (require 'undo-tree nil 'noerror)
    (progn
      (global-undo-tree-mode t)
      (global-set-key (kbd "C-c C-z") 'undo-tree-undo)
      (global-set-key (kbd "C-c C-y") 'undo-tree-redo)
      )
  (message "%s: failed to load `undo-tree'."  load-file-name))

(setq highlight-changes-visibility-initial-state nil)
(global-highlight-changes-mode t)

(setq diff-switches "-u")    ;;I prefer the unified format
(global-set-key (kbd "C-c d") 'diff-buffer-with-file)

;;***  quickly swap lines
(eval-after-load "drag-stuff"
  `(progn
     ;;    (setq drag-stuff-modifier 'hyper)
     (add-to-list 'drag-stuff-except-modes 'org-mode)
     (drag-stuff-global-mode t)))

(load "drag-stuff" t)

;;***  vi-style join-line
(defun join-line ()
  "Join the following line with current line"
  (interactive)
  (delete-indentation 1))

(global-set-key (kbd "C-c J") 'join-line)

;;***  misc

(global-set-key (kbd "C-=") 'align-regexp)


;;** minibuffer

;;***  icomplete
(icomplete-mode t)  ;; completion for minibuffer (commands (M-x)
                    ;; variables (C-h v, customize-variable), functions (C-h f))

;;***  ido
(require 'ido)

(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point 'guess)
(ido-mode 'buffers)

;;***  anything
(if (and (load "anything" t)
         (load "anything-config" t))
    (progn
      ;;enable multiple keyword/regexp match
      ;;(load "anything-match-plugin" t) ;;FIXME: would cause crash?
      ;;(global-set-key (kbd "M-x") 'anything-M-x)
  
      (global-set-key (kbd "<f5> r") 'anything-recentf)
      (global-set-key (kbd "<f5> b") 'anything-buffers+)
      (global-set-key (kbd "<f5> B") 'anything-bookmarks)
      (global-set-key (kbd "<f5> l") 'anything-locate)
      (global-set-key (kbd "<f5> c") 'anything-browse-code)
      (global-set-key (kbd "<f5> i") 'anything-imenu)
      (global-set-key (kbd "<f5> o") 'anything-occur)

      (define-key minibuffer-local-map (kbd "<f5>") 'anything-minibuffer-history)
      )
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

;;***  outline
(require 'outline)
(eval-after-load "outline"
  `(progn
     (global-set-key (kbd "C-z")   outline-mode-prefix-map)
     
     (global-set-key (kbd "C-z <up>")     'outline-previous-visible-heading)
     (global-set-key (kbd "C-z <down>")   'outline-next-visible-heading)
     
     (define-key outline-mode-prefix-map (kbd "<left>")  'hide-subtree)
     (define-key outline-mode-prefix-map (kbd "<right>") 'show-subtree)

     (global-set-key (kbd "M-+")   'outline-toggle-children)
     (global-set-key (kbd "<C-wheel-up>") 'outline-previous-visible-heading)
     (global-set-key (kbd "<C-wheel-down>") 'outline-next-visible-heading)
     (global-set-key (kbd "<C-mouse-1>")  'outline-toggle-children)
     (global-set-key (kbd "<C-mouse-3>")  'hide-sublevels)
     (global-set-key (kbd "<C-mouse-2>")  'show-all)
  ))

;;** some visual effect

;;***  highlight-symbol
(require 'highlight-symbol nil t)

(global-set-key (kbd "C-c j")          'highlight-symbol-at-point)
(define-key search-map (kbd "j")       'highlight-symbol-at-point)
(define-key search-map (kbd "#")    'highlight-symbol-prev)
(define-key search-map (kbd "*")  'highlight-symbol-next)

(global-set-key (kbd "<double-mouse-1>")  'highlight-symbol-at-point)
(global-set-key (kbd "<S-wheel-up>")      'highlight-symbol-prev)
(global-set-key (kbd "<S-wheel-down>")    'highlight-symbol-next)

;;***  bm
(require 'bm nil t)

(global-set-key (kbd "<C-f2>")    'bm-toggle)
(global-set-key (kbd "<M-f2>")    'bm-next)
(global-set-key (kbd "<S-f2>")    'bm-previous)
(global-set-key (kbd "<H-f2>")    'bm-show)

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

;;** programming

;;(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(which-func-mode t)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

;;***  imenu
(autoload 'idomenu "idomenu" "Switch to a buffer-local tag from Imenu via Ido." t)
(define-key goto-map "i" 'idomenu)
(define-key goto-map "I" 'imenu)

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
;;*** dired
(define-key goto-map "d" 'dired-jump) ;;C-x C-j

;;*** eshell
;;FIXME:

;;** misc
(column-number-mode t)

;;*** color-theme
;;FIXME:

;;*** terminal
;;FIXME:


	 
  

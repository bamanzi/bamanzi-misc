;;{{{ some basic settings
(tool-bar-mode -1)
(setq frame-title-format '("%b (%m) - Emacs "
			   (:eval emacs-version)))

(if (<= emacs-major-version 23) ;; emacs < 23.2
    (setq tab-always-indent nil)
  (setq tab-always-indent 'complete)) ;; emacs >= 23.2

(when (eq window-system 'x)
    (setq x-select-enable-clipboard t))
;;  (setq x-select-enable-primary t)


(setq shift-select-mode t)
(delete-selection-mode t)

(setq cua-enable-cua-keys nil)
(cua-mode)

(setq-default truncate-lines t)

(require 'recentf)
(setq recentf-max-saved-items 100)
(recentf-mode)
;;}}}

;;{{{ key bindings

(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(global-set-key (kbd "<C-tab>") 'previous-buffer)
(global-set-key (kbd "<C-S-tab>") 'next-buffer)
(global-set-key (kbd "C-`") 'previous-buffer)

;; make M-z behave more as zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
    (insert char)
    (forward-char -1))

;; Move line up/down. Stolen from org-mode's M-up/down
;; TODO: support region (move region line up/down)
(defun swap-line-up ()
  "Swap the current line with the line above."
  (interactive)
  (transpose-lines 1)
  (beginning-of-line -1))

(defun swap-line-down ()
  "Swap current line with the line below."
  (interactive)
  (beginning-of-line 2) (transpose-lines 1) (beginning-of-line 0))

(global-set-key (kbd "<M-up>") 'swap-line-up)
(global-set-key (kbd "<M-down>") 'swap-line-down)


;; <f2>: 2-columns, diff, bookmarks
;; <f3>: operations on current symbol
;; <f4>: cua-like
;; <f5>: selection and perform
;; <f5> a: anything
;; <C-f10>:  some settings
;; <f11>: window-related commands
;; <f12>: misc stuff

(when (eq window-system 'w32)
  ;;(setq w32-lwindow-modifier 'super)
  (setq w32-rwindow-modifier 'alt)
  (setq w32-apps-modifier 'hyper)
  
  (setq w32-lwindow-modifier 'nil)
  (setq w32-pass-lwindow-to-system nil)
  (define-key key-translation-map (kbd "<lwindow>") (kbd "<f11>")))


(when (load "idle-require" t)
  ;;FIXME
  ;; (setq idle-require-symbols '(cedet nxml-mode)) ;; <- Specify packages here.
  ;;(idle-require 'my-cua-keys)
  ;;(idle-require 'my-win-fns)
  ;;(idle-require 'my-misc)
  ;;(idle-require 'my-one-key)   ;; <- Or like this.
  (idle-require-mode 1))

(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
(global-set-key (kbd "<f3> f") 'find-function-at-point)
(global-set-key (kbd "<f3> v") 'find-variable-at-point)
(global-set-key (kbd "<f3> l") 'find-library)
(global-set-key (kbd "<f3> F") 'ffap-other-window)

(global-set-key (kbd "<f12> l") 'load-library)
(global-set-key (kbd "<f12> b") 'eval-buffer)
(global-set-key (kbd "<f12> r") 'eval-region)

;;}}}

;;{{{ some important third party libraries

(ignore-errors
  (require 'undo-tree)
  (global-undo-tree-mode))

(when (and (load "anything" t)
	  (load "anything-config" t))
  (global-set-key (kbd "<f5> r") 'anything-recentf)
  (global-set-key (kbd "<f5> b") 'anything-buffers+)
  (global-set-key (kbd "<f5> B") 'anything-bookmarks)
  (global-set-key (kbd "<f5> l") 'anything-locate)
  (global-set-key (kbd "<f5> c") 'anything-browse-code)
  (global-set-key (kbd "<f5> i") 'anything-imenu)
  (global-set-key (kbd "<f5> o") 'anything-occur)
  )
  

(if (and (load "auto-complete" t)
	  (load "auto-complete-config" t))
    (progn
      (ac-config-default)
      (add-hook 'lisp-interaction-mode 'ac-emacs-lisp-mode-setup)
      (load "auto-complete-scite-api")))

;; because it would group buffers into two: editing buffer, emacs buffer
(load "ide-skel" t)

(when (or (featurep 'tabbar)
	 (load "tabbar" t))
      (global-set-key (kbd "<C-tab>") 'tabbar-forward)
      (global-set-key (kbd "<C-S-tab>") 'tabbar-backward))

(when (load "highlight-symbol" t)
  (global-set-key (kbd "<double-down-mouse-1>") 'highlight-symbol-at-point)
  )
;;}}}

(load "my-word-ops" t)
(load "my-cua-keys" t)
(load "my-win-fns" t)
;;(load "my-options-cmds" t) ;;TODO
(load "my-misc" t)
(load "my-one-key" t)
;;(load "hyper-key-bindings" t)





	 
  

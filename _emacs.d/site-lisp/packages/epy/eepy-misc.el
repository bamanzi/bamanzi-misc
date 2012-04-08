;;* misc stuff that not closely related to Python

;;** browsing code within buffer
(autoload 'idomenu  "idomenu"
  "Switch to a buffer-local tag from Imenu via Ido." t)
(autoload 'anything-imenu "anything-config"
  "Preconfigured `anything' for `imenu'." t)
(autoload 'anything-browse-code  "anything-config"
  "Preconfigured anything to browse code." t)

(defun idomenu-or-imenu ()
  (interactive)
  (require 'idomenu nil t)
  (if (fboundp 'idomenu)
      (call-interactively 'idomenu)
    (call-interactively 'imenu)))

(defun eepy-go-to-symbol-within-buffer ()
  "Go to symbol (definition) within current buffer.

This would get rid of some annoyance:
- imenu hererachy (anything flats them)
- imenu not working because of cedet
"
  (interactive)
  (anything
   :prompt "Go to: "
   :input (thing-at-point 'symbol)
   :sources
   '(anything-c-source-imenu
     anything-c-source-browse-code)))

(unless (global-key-binding (kbd "M-g s"))
  (define-key goto-map "s" 'eepy-go-to-symbol-within-buffer))

;;TODO: fix-imenu-create-index-function

(autoload 'imenu-tree  "imenu-tree"
  "Display tree view of imenu." t)
(autoload 'tags-tree  "tags-tree"
  "Display tree view of tags." t)


;;** code folding
(autoload 'hideshowvis-minor-mode  "hideshowvis"
  "Toggle Hideshowvis minor mode on or off." t)
(eval-after-load "hideshowvis"
  `(load "hideshow-fringe" 'noerror))

(autoload 'qtmstr-outline-mode "qtmstr-outline"
  "TODO" t)


;;*** highlighting something
(autoload 'pretty-lambda-mode "pretty-lambdada"
  "Buffer-local minor mode to display the word `lambda' as the Greek letter." t)
(autoload 'highlight-indentation  "highlight-indentation"
  "Toggle highlight indentation." t)


;;*** completion
(autoload 'smart-operator-mode  "smart-operator"
  "Insert operators with surrounding spaces smartly." t)

;; autopair
(setq autopair-autowrap t)
(autoload 'autopair-mode  "autopair"
  "Automagically pair braces and quotes like in TextMate." t)
(autoload 'autopair-global-mode "autopair"
  "Toggle Autopair mode in every possible buffer." t)

;; Matching parentheses for all languages and so on
(eval-after-load "autopair"
  `(progn
     ;;(autopair-global-mode t)
     
     ;; Fix for triple quotes in python
     (add-hook 'python-mode-hook
               #'(lambda ()
                   (setq autopair-handle-action-fns
                         (list #'autopair-default-handle-action
                               #'autopair-python-triple-quote-action))))
     ))


;; some helper for `auto-complete'
(defun ac-toggle-source (source &optional desire)
  "Add or remove a SOURCE in `ac-sources'.

If DESIRE given, this source would be absolutely added (if DESIRE > 0) or
remove (if DESIRE <= 0). If DESIRE not given, it would be toggled."
  (interactive
   (list (intern-soft (ido-completing-read "Source: "
                                           (loop for x being the symbols
                                                 if (and (boundp x)
                                                         (string-match "^ac-source-" (symbol-name x)))
                                                 collect (symbol-name x))))))
  (when (and source (symbolp source))
    (if desire
        (if (> desire 0)
            (add-to-list 'ac-sources source)
          (setq ac-sources (remq source ac-sources)))
      (if (memq source ac-sources)
          (setq ac-sources (remq source ac-sources))
        (add-to-list 'ac-sources source)))
    (message "Source `%s' %s." source (if (memq source ac-sources)
                                          "enabled"
                                        "disabled"))))

;;*** misc
(autoload 'iedit-mode "iedit"
  "Edit multiple regions with the same content simultaneously." t)

(defun rename-symbol-in-defun ()
  "Enter `iedit-mode' to rename the symbol in current function, or exit it."
  (interactive)
  (let ( (symbol (thing-at-point 'symbol)) )
    ;;FIXME: judge the symbol type
    (if symbol
        (if (assq 'iedit-mode minor-mode-map-alist)
            (progn
              (iedit-mode -1)
              (widen))
          (narrow-to-defun)
          (iedit-mode t)
          (message "When done, run `rename-symbol-in-defun' again to quit."))
      (message "You need to put cursor on an identifier."))))


(provide 'eepy-misc)

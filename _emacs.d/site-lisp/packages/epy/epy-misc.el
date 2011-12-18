;;* misc stuff that not closely related to Python

;;** browsing code within buffer
(autoload 'idomenu  "idomenu"
  "Switch to a buffer-local tag from Imenu via Ido." t)
(autoload 'anything-imenu "anything-config"
  "Preconfigured `anything' for `imenu'." t)
(autoload 'anything-browse-code  "anything-config"
  "Preconfigured anything to browse code." t)

(defun epy-imenu ()
  (interactive)
  (require 'idomenu nil t)
  (if (fboundp 'idomenu)
      (call-interactively 'idomenu)
    (call-interactively 'imenu)))

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
(autoload 'autopair-mode  "autopair"
  "Automagically pair braces and quotes like in TextMate." t)

;;*** misc
(autoload 'iedit-mode "iedit"
  "Edit multiple regions with the same content simultaneously." t)


(provide 'epy-misc)

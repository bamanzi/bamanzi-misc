;;;_ . epy menu
(require 'easymenu)

(require 'epy-ropemacs)
(require 'epy-checker)
(require 'epy-misc)

(defun define-menu-for-epy (keymap)
  (easy-menu-define epy-menu keymap
    "Menu for emacs-for-python package."
    '("epy"
      ["Choose method (imenu)..." imenu t]
      ["Choose method (anything)...)" anything-browse-code :active (fboundp 'anything-browse-code) ]
      ["Imenu tree" imenu-tree t]
      ["Tags tree" tags-tree t]
      "--"
      ("Code folding"
       ["hs-minor-mode" hs-minor-mode :style toggle :selected hs-minor-mode]
       ["  hideshowvis" hideshowvis-minor-mode :style toggle :selected hideshowvis-minor-mode
        :active hs-minor-mode]
       ["outline-minor-mode" outline-minor-mode :style toggle :selected outline-minor-mode]
       ["  hide sublevels" hide-sublevels t]
       ["  show all" show-all t]
       )
      ["Pretty lambda mode" pretty-lambda-mode :style toggle :selected lambda-mode
       :help "Pretty-print lambdas"]
      ["Highlight indentation mode" highlight-indentation :style toggle
       :selected highlight-indent-active :help "Highlight indentation."]
      "--"
      ["Smart operator mode" smart-operator-mode :style toggle :selected smart-operator-mode
       :help "Insert operators with surrounding spaces smartly."]
      ["Autopair mode" autopair-mode :style toggle :selected autopair-mode
       :help "Automagically pair braces and quotes like TextMate."]
      ("Yasnippets"
       ["on/off" yasnippet :style: toggle :selected nil]
       ["Load Django templates" nil nil]
       )
      ("Auto-Complete"
       ["on/off" auto-complete-mode :style toggle :selected auto-complete-mode]
       ["ropemacs source" epy-enable-ac-ropemacs-source :style toggle :selected (memq 'ac-source-nropemacs ac-sources)]
       ["yasnippets source" ac-python-mode-setup :style radio]
       ["scite-api source" nil :style toggle :selected nil]
       )
      ["pycomplete source" nil :active (fboundp 'py-shell) :style toggle]
      "--"
      ("Syntax Check"
       ["Pyflakes" nil t]
       ["Pylint" epy-pylint t]
       ["Pychecker" nil t]
       ["PEP8" epy-pep8 t]    
       )
      ("Flymake"
       ["on/off"          flymake-mode                    :style toggle :selected flymake-mode]
       ["Pylint"          (epy-flymake-with "epylint")    :style radio  :selected (string= epy-flymaker "epylint")]
       ["PEP8"            (epy-flymake-with "pep8")       :style radio  :selected (string= epy-flymaker "pep8")]
       ["Pyflakes"        (epy-flymake-with "pyflakes")   :style radio  :selected (string= epy-flymaker "pyflakes")]
       ["Pycheckers"      (epy-flymake-with "pychecker")  :style radio  :selected (string= epy-flymaker "pycheckers")]
       ["Set default checker..." (customize-variable 'epy-flymaker)]
       "--"
       ["next error"      flymake-goto-next-error :active flymake-mode]
       ["previous error"  flymake-goto-prev-error :active flymake-mode]
       )    
      ("Debug"
       ["pdb" nil t]
       ["ipdb" nil t]
       ["pydb" nil t]
       )
      ["byte-compile" nil t]
      "--"
      ["eldoc-mode" eldoc-mode :style toggle :selected eldoc-mode]
      ["pylookup" pylookup-lookup :active nil]
      ["python.chm (windows)" epy-python-chm-keyword :active (eq window-system 'w32)]
      "--"
      ["eproject" eproject t]
      ["refactoring: rename current symbol" iedit-mode
       :help "Use `iedit-mode' to replace all occurren of current symbol in whole buffer."]
      ["refactoring: rename current symbol in function"  epy-iedit-in-defun
       :style toggle :selected iedit-mode :help "Use `narrow-to-defun' and 'iedit-mode' to replace all "]
      )))

(define-menu-for-epy python-mode-map)

(when (require 'python-mode nil t)
  (define-menu-for-epy py-mode-map)
  (setq outline-regexp "[[:space:]]*\\(?:\\(?:class\\|def\\)\\)\\_>")
  )

(provide 'epy-menu)

;;;_ . eepy menu
(require 'easymenu)

(require 'eepy-ropemacs)
(require 'eepy-checker)
(require 'eepy-misc)

(defun define-menu-for-eepy (keymap)
  (easy-menu-define eepy-menu keymap
    "Menu for emacs-for-python package."
    '("eepy"
      ["Choose method (imenu)..." imenu t]
      ["Choose method (anything)...)" anything-browse-code
       :active (fboundp 'anything-browse-code) ]
      ["Imenu tree" imenu-tree t]
      ["Tags tree" tags-tree t]
      "--"
      ("Code folding"
       ["hs-minor-mode" hs-minor-mode
        :style toggle :selected hs-minor-mode]
       ["  hideshowvis" hideshowvis-minor-mode
        :style toggle :selected hideshowvis-minor-mode :active hs-minor-mode
        :help "+/- symbol on left-fringe for toggling folding."]
       ["outline-minor-mode" outline-minor-mode
        :style toggle :selected outline-minor-mode]
       ["  hide sublevels" hide-sublevels t]
       ["  show all" show-all t]
       )
      ;; ["Pretty lambda mode" pretty-lambda-mode
      ;;  :style toggle :selected lambda-mode
      ;;  :help "Pretty-print lambdas"]
      ["Develock mode" develock-mode
       :help "A lightweight way to highlight code formatting problems (indentation, whitespaces, long lines...)"]
      ["Highlight indentation mode" highlight-indentation
       :style toggle :selected highlight-indent-active
       :help "Highlight indentation."]
      "--"
      ["Smart operator mode" smart-operator-mode
       :style toggle :selected smart-operator-mode
       :help "Insert operators with surrounding spaces smartly."]
      ["Autopair mode" autopair-mode
       :style toggle :selected autopair-mode
       :help "Automagically pair braces and quotes like TextMate."]
      ("Auto-Complete"
       ["on/off" auto-complete-mode
        :style toggle :selected auto-complete-mode]
       ["Emacs builtin completion" (ac-toggle-source 'ac-source-python-builtin)
        :style toggle :selected (memq ac-source-python-builtin ac-sources)]
       ["pycompletemine source"    (ac-toogle-source 'ac-source-pycompletemine)
        :active (boundp 'ac-source-pycompletemine) ;;and (boundp 'py-shell)
        :style toggle :selected (memq ac-source-pycompletemine ac-sources)]
       ["ropemacs source"          (ac-toggle-source 'ac-source-nropemacs)
        :active nil
        :style toggle :selected (memq 'ac-source-nropemacs ac-sources)]
       ["yasnippets source"        (ac-toggle-source 'ac-source-yasnippet)
        :style toggle :selected (memq 'ac-source-yasnippet ac-sources)]
       ["scite-api source"         (ac-toogle-source 'ac-source-scite-api)
        :active (boundp 'ac-source-scite-api)
        :style toggle :selected (memq 'ac-source-scite-api ac-sources)]
       )
      ("Yasnippets"
       ["on/off" yasnippet-mode :style: toggle :selected nil]
       ["Load Django templates" nil nil]
       )
      "--"
      ("Syntax Check"
       ["Pylint"    eepy-pylint t]
       ["PEP8"      eepy-pep8 t]
       ["Pyflakes"  eepy-pyflake t]
       ["Pychecker" eepy-pychecker t]
       )
      ("Flymake"
       ["on/off"          flymake-mode
        :style toggle :selected flymake-mode]
       ["Epylint"               (eepy-flymake-with            eepy-flymake-cmdline-epylint)
        :style radio  :selected (string= eepy-flymake-cmdline eepy-flymake-cmdline-epylint)]
       ["PEP8"                  (eepy-flymake-with            eepy-flymake-cmdline-pep8)
        :style radio  :selected (string= eepy-flymake-cmdline eepy-flymake-cmdline-pep8)]
       ["Pyflakes"              (eepy-flymake-with            eepy-flymake-cmdline-pyflakes)
        :style radio  :selected (string= eepy-flymake-cmdline eepy-flymake-cmdline-pyflakes)]
       ["Pychecker"             (eepy-flymake-with            eepy-flymake-cmdline-pychecker)
        :style radio  :selected (string= eepy-flymake-cmdline eepy-flymake-cmdline-pychecker)]
       ["Set default checker..." (customize-variable 'eepy-flymake-cmdline)]
       "--"
       ["next error"      flymake-goto-next-error :active flymake-mode]
       ["previous error"  flymake-goto-prev-error :active flymake-mode]
       )
      ["Python shell" python-shell]
      ["IPython shell" ipython
       :active (fboundp 'ipython)]  ;;FIXME: python-mode.el needed
      ("Debug"
       ["pdb" pdb t]
       ["ipdb" ipdb t]
       ["pydb" pydb t]
       )
      ;;["byte-compile" nil t]
      "--"
      ["eldoc-mode" eldoc-mode
       :style toggle :selected eldoc-mode]
      ["pylookup" pylookup-lookup
       :active nil]
      ["python.chm (windows)" eepy-python-chm-keyword
       :active (eq window-system 'w32)]
      "--"
      ("Project"
       ["open rope project..." eepy-rope-open-project]
       ["auto-open ropeproject if found" eepy-toggle-auto-detect-rope-project
        :style toggle :selected eepy-auto-detect-rope-project]
       ["eproject" eproject t]
      )
      ("Misc"
       ["refactoring: rename current symbol" iedit-mode
       :help "Use `iedit-mode' to replace all occurren of current symbol in whole buffer."]
       ["refactoring: rename current symbol in function"  eepy-iedit-in-defun
       :style toggle :selected iedit-mode
       :help "Use `narrow-to-defun' and 'iedit-mode' to replace all "]
       )
      )))

(define-menu-for-eepy python-mode-map)

(eval-after-load "python-mode"
  `(progn
     (define-menu-for-eepy py-mode-map)
     ))

(provide 'eepy-menu)

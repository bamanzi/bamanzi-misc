(require 'pascal)
(require 'delphi)

;;;_. highlight more - pascal-mode
;;; for better supporting Object Pascal 
(font-lock-add-keywords 'pascal-mode              
    '( ("\\<\\(class\\|uses\\|as\\|is\\|while\\|until\\|unit\\|try\\|except\\|finally\\|private\\|public\\|protected\\|interface\\|implementation\\|initialization\\|finalization\\|resourcestring\\)\\>"  
        1 font-lock-keyword-face)  
       ("\\<\\(integer\\|string\\|char\\|shortint\\|smallint\\|longint\\|longword|\\|int64\\|byte\\|word\\|cardinal\\|dword\\|qword\\|null\\|variant\\|pointer\\|set\\|tdatetime\\)\\>"
        1 font-lock-type-face)  
       ("\\<\\(exit\\|break\\|continue\\|assert\\|inc\\|dec\\|copy\\|length\\|setlength\\|sizeof\\|assigned\\|ord\\|pred\\|succ\\|new\\|dispose\\|allocmem\\|getmem\\|freemem\\|low\\|high\\|lo\\|hi\\|include\\|exclude\\)\\>"
        1 font-lock-function-name-face)  
       ("\\<\\(true\\|false\\|nil\\)\\>"
        1 font-lock-constant-face)  
       ("\\<\\(FIXME\\|TODO\\):"
        1 font-lock-warning-face prepend))) 


;;;_. use pascal-mode for object pascal
;;; as delphi-mode is implemented in a weired way, which prevent hightlight-symbol, highlight-indentation working
(defun pascal-mode-for-objpas-init ()
  ;; make // starts the comment line
  (modify-syntax-entry ?/   ". 12b" pascal-mode-syntax-table)
  (modify-syntax-entry ?\n  "> b"   pascal-mode-syntax-table)

  (setq comment-start "// "
        comment-end "")
  ;;add try/except/finally
  (defconst pascal-beg-block-re "\\<\\(begin\\|case\\|record\\|repeat\\|try\\|except\\finally\\)\\>")
  ;; add finally
  (defconst pascal-noindent-re "\\<\\(begin\\|end\\|until\\|else\\|finally\\)\\>")

  )

(add-hook 'pascal-mode-hook 'pascal-mode-for-objpas-init)



;;;_. highlight more - delphi-mode
;;; font-lock-add-keywords won't work for delphi-mode,
;;; thus I have to override `delphi-face-of' and `delphi-word-token-at'

(defconst delphi-data-types  
  '( integer shortint smallint longint longword int64 byte word cardinal dword qword  
             boolean bytebool longbool real     
             char string shortstring ansistring widestring pchar  
             array record set file  pointer variant tdatetime  
             )  
  "Delphi/FreePascal built-in data types")  
  
(defconst delphi-system-funcs  
  '( allocmem assert assigned break continue copy dec dispose exit exclude freemem  
              getmem hi high inc include length lo low new  
              ord pred reallocmem setlength sizeof str succ val)  
  "Delphi/FreePascal functions in System unit")  

(defcustom delphi-datatype-face 'font-lock-type-face
  "*Face used to color delphi data types."
  :type 'face
  :group 'delphi)

(defcustom delphi-function-face 'font-lock-function-name-face
  "*Face used to color delphi `built-in` functions."
  :type 'face
  :group 'delphi)

;; re-define delphi-face-of
(defun delphi-face-of (token-kind)
  ;; Returns the face property appropriate for the token kind.
  (cond ((delphi-is token-kind delphi-comments) delphi-comment-face)
        ((delphi-is token-kind delphi-strings) delphi-string-face)
        ((string= token-kind '_type) delphi-datatype-face)
        ((string= token-kind '_func) delphi-function-face)
        ((delphi-is token-kind delphi-keywords) delphi-keyword-face)
        (delphi-other-face)))

(defun delphi-word-token-at (p)
  ;; If point p is over a word (i.e. identifier characters), then return a word
  ;; token. If the word is actually a keyword, then return the keyword token.
  (let ((word (delphi-charset-token-at p delphi-word-chars 'word)))
    (when word
      (let* ((word-image (downcase (delphi-token-string word)))
             (keyword (intern-soft word-image)))
        (when (or keyword (string= "nil" word-image))
              (if (delphi-is keyword delphi-keywords)
                  (delphi-set-token-kind word keyword)
                (if (delphi-is keyword delphi-data-types)
                    (delphi-set-token-kind word '_type)
                  (if (delphi-is keyword delphi-system-funcs)
                      (delphi-set-token-kind word '_func)))))
        word))))

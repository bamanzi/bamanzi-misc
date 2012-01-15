;;* Org-mode

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

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c q") 'org-capture)


;;** buffer-local faces for org-mode
(defun font-exists-p (font)
  "Test if FONT is available."
   (if (null (list-fonts (font-spec :family font)))
              ;; 2008-02-26 function of the new font backend (Emacs 23),
              ;; instead of `x-list-fonts'
       nil
     t))

;;Use some specific monospace font, so that cells in orgtbl would align well
;; when Chinese/English are mixed
(defun org-mode-init-face ()
  (message "Customize face `variable-pitch' to change fonts for `org-mode'.")
  ;; (if (font-exists-p "Inconsolata")
  ;;     (set-face-attribute 'variable-pitch nil
  ;;                         :family  "Inconsolata"
  ;;                         :foundry "outline"
  ;;                         :font    "-outline-Inconsolata-normal-normal-normal-mono-*-*-*-*-c-*-iso8859-1")
  ;;   (if (font-exists-p "Consolas")
  ;;     (set-face-attribute 'variable-pitch nil
  ;;                         :family  "Consolas"
  ;;                         :foundry "outline"
  ;;                         :font    "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-iso8859-1"
  ;;                         :fontset "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-*-*")
  ;;       (message "No suitable font found for org-mode. ")))
  
  (set (make-variable-buffer-local 'line-spacing) 2) 
  (buffer-face-mode t)

  (bmz/init-org-level-faces (selected-frame))
  )

(defun bmz/init-org-level-faces (frame)
  (interactive (list (selected-frame)))
  (set-face-attribute 'org-level-1 frame :height 1.5 :bold t)
  (set-face-attribute 'org-level-2 frame :height 1.3 :bold t)
  (set-face-attribute 'org-level-3 frame :height 1.1))

;;FIXME: not work?
(add-hook 'org-mode-hook 'org-mode-init-face)
(add-hook 'after-make-frame-functions 'bmz/init-org-level-faces)

;;*** misc
;;FIXME: '<s' template
(defun org-quote-region (begin end)
  (interactive "r")
  (if (not (and transient-mark-mode mark-active))
      (error "You should make a region")
    (let* ( (choices '("SRC" "EXAMPLE" "QUOTE" "HTML" "VERSE" "COMMENT" "LATEX"
                    "*" "_" "/" "=" "~"
                    ))
            (choice (ido-completing-read "With:" choices)) )
      (if (member choice '("*" "_" "/" "=" "~"))
          (progn
            (goto-char end)
            (insert-string choice)
            (goto-char begin)
            (insert-string choice))
        (progn
          (goto-char end)
          (insert-string (concat "\n#+END_" choice "\n"))
          (goto-char begin)
          (insert-string (concat "#+BEGIN_" choice "\n")))))))

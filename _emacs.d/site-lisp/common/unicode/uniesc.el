;;; uniesc.el --- processing Java-style Unicode character escapes    -*-coding: utf-8;-*-

;; Copyright (C) 2006  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: data, languages
;; $Revision:$

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; C99 defines escape sequences for Unicode characters in strings and
;; languages like Python do similarly.  This package defines functions
;; for encoding, decoding and composing (i.e. displaying with the
;; appropriate character) such sequences.

;; The basic form of such sequences is `\uXXXX' or `\UXXXXXXXX', where
;; X is a hex digit.  The first is obviously confined to the BMP and
;; the latter can represent all of UCS.  In fact, Java uses utf-16
;; (sigh), not \U escapes, so supplementary characters appear as
;; surrogate pairs from \u sequences
;; <URL:http://java.sun.com/docs/books/jls/third_edition/html/lexical.html#3.3>,

;; Java also specifies that the `u' in the escape can be repeated,
;; which C and Python don't support.  Escapes may (Java) or may not
;; (Python?) be allowed outside strings; you can control that (in
;; Emacs 22 only) with `uniesc-strings-only'.

;; A possible use is to put `uniesc-compose-buffer' on a mode hook for
;; Python, Java, &c. e.g.:

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (require 'uniesc)
;;             (set (make-local-variable 'uniesc-match-regexp)
;;                  uniesc-c-style-match)
;;             (set (make-local-variable 'uniesc-strings-only) t)
;;             (uniesc-compose-buffer)))

;; Fixme:  There should probably be font-lock support for maintaining
;; the composition; I'm not sure how to balance dealing with multi-u
;; escapes between command prefix args and mode-specific local
;; variables.  Java escapes actually represent utf-16 sequences,
;; including surrogate pairs, but surrogates aren't currently handled.

;; Test cases: \u00ff \uu0078 \\u00fe \U0010000 \u03B1 β

;;; Code:

;; Emacs <= 21.3 doesn't expand rx at compile time and has a single arg.
(eval-when-compile
  (unless (equal "" (macroexpand '(rx "")))
    (defmacro rx (&rest regexps)
      (if (cdr regexps)
	  (rx-to-string `(and ,@regexps) t)
	(rx-to-string (car regexps) t)))))

(require 'syntax nil t)		 ; might be available outside Emacs 22

(eval-and-compile
  (if (fboundp 'syntax-ppss)
      (defsubst uniesc-context ()
	(if uniesc-strings-only
	    (nth 3 (syntax-ppss))	; in a string
	  t))
    (defsubst uniesc-context ()
      t)))

(defconst uniesc-java-style-match
  (rx ?\\ (1+ ?u) (group (repeat 4 hex-digit)))
  "Regexp matching Unicode escapes appropriate for Java and maybe more.
May have the `u' repeated after the backslash.")

(defconst uniesc-c-style-match
  (rx (or (and ?\\ ?u (group (repeat 4 hex-digit)))
	  (and ?\\ ?U (group (repeat 8 hex-digit)))))
  "Regexp matching Unicode escapes appropriate for C/Python and maybe more.
Only a single `u' is allowed after the backslash.")

(defvar uniesc-match-regexp uniesc-c-style-match
  "Regexp for matching Unicode escapes.
Should be the value of either `uniesc-java-style-match' or
`uniesc-c-style-match'.")

(defvar uniesc-strings-only nil
  "*Non-nil means only do encoding/decoding within strings.
Not effective unless Emacs 22's `syntax' library is available.")

(defvar uniesc-java-convention nil
  "*Non-nil means whole-buffer encoding and decoding obey Java conventions.
These are that multiple-`u' escape sequences are decoded just by
removing one `u', and that existing escape sequences are encoded
by adding a `u' to the existing sequence.

This variable should probably be made buffer-locally appropriate
for the language concerned.")

(defun uniesc-un-process (beg end &optional compose reduce)
  "Process Unicode escapes in the region BEG to END.
COMPOSE non-nil means to compose rather than decode the sequences.
REDUCE non-nil means to reduce multiple-`u' sequences to single-`u'
ones when COMPOSE is non-nil."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (case-fold-search c)
	(while (re-search-forward uniesc-match-regexp nil t)
	  (when (and (/= ?\\ (char-before (match-beginning 0)))	; real escape
		     (uniesc-context))
	    (if (and reduce (memq (char-after (+ 2 (match-beginning 0)))
				  '(?u ?U)))
		;; Multiple-u -- just delete one.
		(progn (goto-char (+ 2 (match-beginning 0)))
		       (delete-char 1))
	      (when (setq c (decode-char ; Emacs char (or nil)
			     'ucs (string-to-number
				   (buffer-substring (or (match-beginning 1)
							 (match-beginning 2))
						     (match-end 0))
				   16)))
		(unless (and (>= c #xd800) (<= c #xdfff)) ; surrogate range
		  (if compose
		      (compose-region (match-beginning 0) (match-end 0) c)
		    (replace-match (string c) t t)))))))))))

(defun uniesc-composition-function (from to pattern &optional string)
  "Function suitable for use in `composition-function-table' for Java.
Composes `\\uXXXX'-type escapes to the appropriate character."
  (if (< (1+ from) to)
      (let* ((text (if string
		       (if (eq ?U (aref string 1))
			   (substring string (- to 4) to)
			 (substring string (- to 8) to))
		     (if (eq ?U (char-after (1+ from)))
			 (buffer-substring (- to 8) to)
		     (buffer-substring (- to 4) to))))
	     (c (decode-char 'ucs (string-to-number text 16))))
	(when c
	  (if string
	      (compose-string string from to c)
	    (if (uniesc-context)
		(compose-region from to c)))
	  (- to from)))))

(defun uniesc-set-composition-function-table ()
  "Set `composition-function-table' locally to include escape rules."
  (let ((table (make-char-table 'composition-function-table)))
    ;; Add to the default one.
    (set-char-table-parent table composition-function-table)
    (aset table
	  ?\\ (list (cons uniesc-match-regexp #'uniesc-composition-function)))
    (set (make-local-variable 'composition-function-table) table)))

;;;###autoload
(defun uniesc-decode-region (beg end &optional reduce)
  "Decode Unicode escapes in the region into Emacs characters.
With prefix arg REDUCE, don't decode sequences with multiple `u's
directly -- just shorten the `u' sequence by one, i.e. \\uuXXXX -> \\uXXXX."
  (interactive "r\nP")
  (uniesc-un-process beg end nil reduce))

;;;###autoload
(defun uniesc-decode-buffer ()
  "Run `uniesc-decode' on the whole buffer.
See also `uniesc-java-convention'."
  (interactive "P")
  (save-restriction
    (widen)
    (uniesc-decode-region 1 (point-max) uniesc-java-convention)))

;;;###autoload
(defun uniesc-compose-region (beg end)
  "Compose Unicode escapes in the region to the characters they represent."
  (interactive "r")
  ;; Arrange to deal with re-composition, but this probably isn't
  ;; actually useful.
  (unless (local-variable-p 'composition-function-table)
    (set (make-local-variable 'composition-function-table)
	 uniesc-composition-function-table))
  (uniesc-un-process beg end t))

;;;###autoload
(defun uniesc-compose-buffer ()
  "Compose Unicode escapes in the buffer to the characters they represent."
  (interactive)
  (save-restriction
    (widen)
    (uniesc-compose-region 1 (point-max))))

;;;###autoload
(defun uniesc-encode-region (beg end &optional expand)
  "Encode non-ASCII characters in the region as Unicode escapes.
Prefix arg EXPAND non-nil means add an extra `u' in existing escape
sequences à la Java."
  (interactive "r\nP")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (c)
	(while (re-search-forward (rx (or (group nonascii)
					  (and ?\\ (group (any "uU")))))
				  nil t)
	  (when (uniesc-context)
	    (if (and (match-beginning 2)
		     (/= ?\\ (char-before (match-beginning 0))))
		(if expand (insert (char-before)))
	      (if (setq c (encode-char (char-before) 'ucs))
		  (if (<= c #xffff)
		      (replace-match (format "\\u%04x" c) t t)
		    (replace-match (format "\\U%08x" c) t t))))))))))

;;;###autoload
(defun uniesc-encode-buffer ()
  "Run `uniesc-encode' on the whole buffer.
See also `uniesc-java-convention'."
  (interactive "P")
  (save-restriction
    (widen)
    (uniesc-encode-region 1 (point-max) uniesc-java-convention)))

(defun uniesc-setup-java-style ()
  "Setup Unicode escape variables appropriately for Java."
  (set (make-local-variable 'uniesc-java-convention) t)
  (set (make-local-variable 'uniesc-match-regexp) uniesc-java-style-match)
  (set (make-local-variable 'uniesc-strings-only) nil)
  (uniesc-set-composition-function-table))

(defun uniesc-setup-c-style ()
  "Setup Unicode escape variables appropriately for C/Python."
  (set (make-local-variable 'uniesc-java-convention) nil)
  (set (make-local-variable 'uniesc-match-regexp) uniesc-c-style-match)
  (set (make-local-variable 'uniesc-strings-only) t)
  (uniesc-set-composition-function-table))

(provide 'uniesc)
;;; uniesc.el ends here

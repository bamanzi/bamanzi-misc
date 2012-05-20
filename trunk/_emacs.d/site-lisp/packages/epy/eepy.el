;;; eepy.el --- top file for Enhanced Emacs for PYthon (eepy) suite

;; Author: Ba Manzi <bamanzi@gmail.com>
;; URL:    https://github.com/bamanzi/eepy
;; Version: 0.5

;; Based on Gabriele Lanaro’s Emacs-for-Python (epy) suite
;;   https://github.com/gabrielelanaro/emacs-for-python
;; Refer doc-epy/CONTRIBUTORS for names of contributors of epy.
;; Refer doc-epy/COPYING for copyright information of epy.

;;add current dir to `load-path'
(let ((eepy-install-dir  (file-name-directory (or load-file-name
                                                  (when (boundp 'bytecomp-filename) bytecomp-filename)
                                                  buffer-file-name))))
  (add-to-list 'load-path eepy-install-dir))
  


(require 'eepy-completion)
(require 'eepy-ropemacs)
(require 'eepy-checker)

(provide 'eepy)

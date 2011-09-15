(server-start)

(fset 'yes-or-no-p 'y-or-n-p)

(unless (fboundp 'idle-require)
  (defun idle-require (feature)
    (require feature nil t)))


;; disable VC, to improve library loading speed (if in vc dir)
(ignore-errors
  (remove-hook 'find-file-hook 'vc-find-file-hook))

;; unset predefined function key bindings
;; I'll use them as prefix keys

;; <f2>: 2-columns, diff, bookmarks
;; <f3>: operations on current symbol
;; <f4>: cua-like
;; <f5>: selection and perform
;; <f5> a: anything
;; <f6>: vi style commands
;; <f7>:
;; <f8>:
;; <f9>: M-f9: syntax check, C-f9: compile, f9: run
;; <f10>: toggle minor modes
;; <C-f10>:  some settings
;; <f11>: window-related commands (`super' modifier)
;; <f12>: misc stuff

(global-unset-key [f2])
(global-unset-key [f3])
(global-unset-key [f4])
(global-unset-key [f10])

;; SuSE would bind these by default, cancel them too
;;(global-unset-key [f1]) 
(global-unset-key [f5]) 
(global-unset-key [f6]) 
(global-unset-key [f7]) 
(global-unset-key [f8]) 
(global-unset-key [f9]) 
(global-unset-key [f11])
(global-unset-key [f12])


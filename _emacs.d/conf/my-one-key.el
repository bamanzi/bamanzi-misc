(require 'one-key)

;; ǰ��˵�ˣ��ҿ�ʼ�������ù��ܼ���ǰ׺������ʱ�����е�ǲ���������
;; emacswiki������һ��one-key.el�����Լ�һ������˵�̫�鷳�ˣ�
;; one-key-default.el Ҳ����һ�£���Щ��Ϊ���Ҳ������ġ������Լ�����һ
;; ����װ:

(defun one-key-bind-keymap (key keymap-prefix)
  "Generate an one-key menu for keys starting with `keymap-prefix', and bind it to `key'.

    It also bind `keymap-prefix'-? to the same one-key menu, if not in use.

    For example, when called with ('ESC <f1>', '<f1>'), it would
    generate one-key menu 'one-key-menu-<f1>', and bind 'ESC <f1>'
    and '<f1> ?' to one-key-menu-<f1>.
    "
  (if (keymapp (key-binding (read-kbd-macro keymap-prefix)))
      (let ( (one-key-menu-sym (intern (format "one-key-menu-%s"
                                               (replace-regexp-in-string " " "-" keymap-prefix)))) )
        (let ( (prefixed-?-key (read-kbd-macro (concat keymap-prefix " ?"))) )
          (unless (key-binding prefixed-?-key)
            (global-set-key prefixed-?-key one-key-menu-sym)))
        (with-temp-buffer
          (one-key-insert-template keymap-prefix keymap-prefix)
          (eval-buffer))
        (global-set-key (read-kbd-macro key) one-key-menu-sym))
    (message "'%s' is not a keymap-prefix" keymap-prefix)))

(defun one-key-refresh-fkey (fkey)
  (interactive "sfkey: ")
  (one-key-bind-keymap (concat "ESC " fkey) fkey))


;; ��one-key-default�Ĳ�ͬ���ڣ��������÷�ʽ�£�"ESC <f5>"��"<f5> ?"��
;; �󶨵�one-key-menu-<f5>����ԭ��<f5>���ܲ��ᱻȡ������������������
;; �İ�(����"<f5> k")��������"<f5> C-h"��ȡ<f5>��ͷ��bindings list��
;; ��Ȼ��one-key-menu-<f1>�����Զ����£�������ӵ�<f5> k"�����Զ�����
;; one-key-menu-<f5>����������ִ��һ��(one-key-bind-keymap "ESC <f5>"
;; "<f5>")������)

;;usage
(one-key-bind-keymap "ESC <f1>" "<f1>")
(one-key-bind-keymap "ESC <f2>" "<f2>") ;;bookmark, folding
(one-key-bind-keymap "ESC <f3>" "<f3>") ;;operations on current symbol
(one-key-bind-keymap "ESC <f4>" "<f4>") ;;CUA like

(one-key-bind-keymap "<f5> ESC a" "<f5> a") ;;anything
(one-key-bind-keymap "ESC <f5>" "<f5>") ;; anything, ido

;;(one-key-bind-keymap "ESC <C-f10>" "<C-f10>")
(one-key-bind-keymap "ESC <f11>" "<f11>")  ;; winndows-related



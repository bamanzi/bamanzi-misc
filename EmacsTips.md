# Windows #
  * Want to undo/redo window layout? turn `winner-mode` on.
  * Stick a buffer to a window: `M-: (set-window-dedicated-p (selected-window) t)`. This would prevent other buffer displaying in this window (but you can manually switch to other buffer, with `C-x b`).
  * Don't want Emacs split window automatically?  Set `split-width-threshold` and `split-height-threshold` to nil.
  * Want to move to above/below/left/right window directly?  `(require 'windmove)(windmove-default-keybindings 'super)`


# Commands #
  * built-in [icomplete-mode](http://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Options.html) is better than `smex`, it would show your keybinding for your command.

# Keybindings #
  * Don't know how to describe a keychord in `global-set-key` or `define-key`? Type `C-h k` then that keychord, Emacs would tell you something like _C-) is undefined_ or _&lt;M-down-mouse-1&gt; at that spot runs the command mouse-drag-secondary_. Now you can use `(kbd "C-)")` or `(kbd "<M-down-mouse-1>")`.

# Editing #
  * For copy/cut/paste in GUI Emacs, add `(setq x-select-enable-clipboard t)` to your dotemacs (whether X11 or not). For a fresh installation, you can use `M-x clipboard-kill-ring-save`, `M-x clipboard-kill-region` and `M-x clipboard-yank`.
  * `copy-from-above-command`: copy above line (or just a char). You need to load `misc.el` first.


Add your content here.  Format your content with:
  * Text in **bold** or _italic_
  * Headings, paragraphs, and lists
  * Automatic links to other wiki pages
# double-type.el

Provides a keyboard operation method corresponding to a mouse double-click, extending Emacs's keybinding capabilities.

## Core Concept

`double-type.el` offers a simple and powerful feature: it allows you to assign two different commands to the **same key**, distinguishing between a single press and a quick double-press (a "double type").

This enables you to bind two related actions to a single, easy-to-press key, reducing the need for modifier keys like `Ctrl` or `Alt` and creating a more intuitive and efficient workflow.

## Features

- **Simple**: With a single, focused purpose, it's easy to configure and unlikely to conflict with other packages.
- **Ergonomic**: Assign a common action and a related "power" version of that action to the same convenient key.
- **Non-modal**: Integrates seamlessly into the standard Emacs interaction model without introducing the cognitive overhead of switching modes.

## Installation

1.  Place `double-type.el` in a directory included in your `load-path`.
2.  Add the following line to your `init.el` or `.emacs` file:

```emacs-lisp
(require 'double-type)
```

## Basic Usage

Use the `double-type/define-key` function to define your keybindings.

```emacs-lisp
(double-type/define-key keymap key
                        :on-single-type command-for-single-press
                        :on-double-type command-for-double-press)
```

---

## A Crucial Note on Key Selection

The utility of this library depends heavily on choosing the right keys to bind.

When a key is defined with `double-type`, Emacs must briefly wait after the first press to see if a second one is coming. Because of this brief, built-in delay, **I strongly recommend *not* assigning `double-type` to keys that are often typed in rapid succession.**

For example, binding this to letters, numbers, or common movement keys like `C-f` and `C-b` will likely feel sluggish and annoying.

Instead, `double-type.el` is most effective when used with keys that are typically pressed in isolation, especially those combined with modifier keys like `Ctrl` or `Alt`.

---

## Practical Examples

Here are several practical examples that showcase the power of this library when used as intended.

### 1. Jump to Definition and Back (`M-.`)

Combine the essential programming actions of jumping to a definition and returning to your original location, all on a single key.

- **Single-press**: Jump to definition (`xref-find-definitions`).
- **Double-press**: Pop back from the jump (`xref-pop-marker-stack`).

```emacs-lisp
(double-type/define-key global-map (kbd "M-.")
                        :on-single-type 'xref-find-definitions
                        :on-double-type 'xref-pop-marker-stack)
```

### 2. Create a Window-Management Prefix Key (`M-w`)

Retain the original function of `M-w` (copy) by turning the double-press into a prefix key for a custom keymap of window-management commands.

- **Single-press**: Copy region (`copy-region-as-kill`).
- **Double-press**: A custom keymap for window commands.

```emacs-lisp
;; 1. Define a prefix keymap for window commands
(define-prefix-command 'my-window-map)
(define-key my-window-map (kbd "s") 'split-window-below)
(define-key my-window-map (kbd "v") 'split-window-right)
(define-key my-window-map (kbd "0") 'delete-window)
(define-key my-window-map (kbd "1") 'delete-other-windows)

;; 2. Assign the keymap to the double-press of M-w
(double-type/define-key global-map (kbd "M-w")
                        :on-single-type 'copy-region-as-kill
                        :on-double-type 'my-window-map)
```
Now you can use sequences like `M-w M-w s` to split a window or `M-w M-w 0` to close one.

### 3. Streamline Magit Workflow (`<f8>`)

Enhance your daily Git workflow with Magit. A function key like `<f8>` is a great candidate because it's easy to press and typically unassigned.

- **Single-press**: Open the Magit status window (`magit-status`).
- **Double-press**: Initiate a commit (`magit-commit`).

```emacs-lisp
(double-type/define-key global-map (kbd "<f8>")
                        :on-single-type 'magit-status
                        :on-double-type 'magit-commit)
```

### 4. Escalate Query Replace (`M-%`)

Unify the query-replace commands, escalating from a standard replace to a more powerful regex replace.

- **Single-press**: Standard query-replace (`query-replace`).
- **Double-press**: Regex query-replace (`query-replace-regexp`).

```emacs-lisp
(double-type/define-key global-map (kbd "M-%")
                        :on-single-type 'query-replace
                        :on-double-type 'query-replace-regexp)
```

### 5. Centering and Display Prefix Key (`C-l`)

Keep the convenient recentering function of `C-l` while using its double-press to activate a custom keymap for display and buffer-related commands.

- **Single-press**: Recenter the view (`recenter-top-bottom`).
- **Double-press**: A custom keymap for display/buffer commands.

```emacs-lisp
;; 1. Define a prefix keymap for display/buffer commands
(define-prefix-command 'my-display-map)
(define-key my-display-map (kbd "b") 'switch-to-buffer)
(define-key my-display-map (kbd "k") 'kill-current-buffer)
(define-key my-display-map (kbd "+") 'text-scale-increase)
(define-key my-display-map (kbd "-") 'text-scale-decrease)
(define-key my-display-map (kbd "f") 'toggle-frame-fullscreen)

;; 2. Assign the keymap to the double-press of C-l
(double-type/define-key global-map (kbd "C-l")
                        :on-single-type 'recenter-top-bottom
                        :on-double-type 'my-display-map)
```

## Configuration

The timeout for detecting a double-press can be customized by setting the `double-type/timeout` variable (default is 0.4 seconds).

```emacs-lisp
(setq double-type/timeout 0.3) ;; Set to 0.3 seconds
```

## Development: Compile & Test

- Byte-compile:
  - Makefile: `make compile`
  - Direct: `emacs -Q --batch -L . -f batch-byte-compile double-type.el`

- Run tests (ERT):
  - Makefile: `make test`
  - Direct: `emacs -Q --batch -L . -l double-type.el -l test-double-type.el -f ert-run-tests-batch-and-exit`

- macOS custom Emacs path example:
  - `make test EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"`
  - Direct: `"/Applications/Emacs.app/Contents/MacOS/Emacs" -Q --batch -L . -l double-type.el -l test-double-type.el -f ert-run-tests-batch-and-exit`

- Temporary directory note (batch test on restricted environments):
  - `mkdir -p .tmp && TMPDIR=$PWD/.tmp emacs -Q --batch -L . -l double-type.el -l test-double-type.el -f ert-run-tests-batch-and-exit`

## Known Bugs

- Editing keyboard macros that include double-type key sequences with `kmacro-step-edit-macro` (`C-x C-k SPC`) may corrupt the macro contents.
  - Workaround: Use `edit-kbd-macro` (`C-x C-k e`) to edit, and finish with `C-c C-c`.
  - This limitation applies only when the macro contains double-type sequences; ordinary macros are unaffected.

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

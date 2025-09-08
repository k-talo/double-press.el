# double-press.el

Provides a keyboard operation method corresponding to a mouse double-click, extending Emacs's keybinding capabilities.

## Core Concept

`double-press.el` offers a simple and powerful feature: it allows you to assign two different commands to the **same key**, distinguishing between a single press and a quick double-press (a "double press").

This enables you to bind two related actions to a single, easy-to-press key, reducing the need for modifier keys like `Ctrl` or `Alt` and creating a more intuitive and efficient workflow.

## Features

- **Simple**: With a single, focused purpose, it's easy to configure and unlikely to conflict with other packages.
- **Ergonomic**: Assign a common action and a related "power" version of that action to the same convenient key.
- **Non-modal**: Integrates with standard Emacs keys without mode switches. A common pattern is to keep the original action on single-press and open a small, personal prefix on double-press (discoverable with `C-h`/`<f1>`), reducing finger travel and memory load.

## Installation

1.  Place `double-press.el` in a directory included in your `load-path`.
2.  Add the following line to your `init.el` or `.emacs` file:

```emacs-lisp
(require 'double-press)
```

## Compatibility

- Supported: Emacs 24.4+ (tested on 26-30).

## Basic Usage

Use the `double-press/define-key` function to define your keybindings.

```emacs-lisp
(double-press/define-key keymap key
                         :on-single-press command-for-single-press
                         :on-double-press command-for-double-press)
```

Tip: When a double-press leads to a prefix keymap, you can press `C-h`
or `<f1>` at the prompt to see the keymap's help. On newer Emacs,
this uses `describe-keymap` and shows a short header with the bound key.

## How Double-Press Works

- Single key: quickly press the same key twice within the time window.
  - Example: press `<f8>` twice quickly -> `<double> <f8>`.
- With modifiers: hold the modifier(s) (e.g., `Meta`/`Ctrl`/`Alt`/`Super`) and quickly press the non-modifier key twice.
  - Example: hold `Meta` and press `.` twice quickly -> `<double> M-.`.
- Do not double-tap the modifier itself; keep it held while double-pressing the target key.
- The timeout (maximum interval between the two presses) is controlled by `double-press/timeout`.

---

## A Crucial Note on Key Selection

The utility of this library depends heavily on choosing the right keys to bind.

When a key is defined with `double-press`, Emacs must briefly wait after the first press to see if a second one is coming. Because of this brief, built-in delay, **I strongly recommend *not* assigning `double-press` to keys that are often pressed in rapid succession.**

For example, binding this to letters, numbers, or common movement keys like `C-f`, `C-b`, `C-a`, `C-e`, `C-n`, and `C-p` will likely feel sluggish and annoying.

Instead, `double-press.el` is most effective when used with keys that are typically pressed in isolation, especially those combined with modifier keys like `Ctrl` or `Alt`.

---

## Why a Personal Prefix?

- Less finger travel: one comfortable key plus a short mnemonic letter.
- Lower memory load: keep related actions under one place you remember.
- Zero disruption: single-press keeps the original behavior intact.
- Discoverability: press `C-h`/`<f1>` inside the prefix to see all options.

A common pattern is to keep the original action on single-press and open a
small, personal prefix on double-press. For example, keep copy on `M-w`, and
put window commands under `M-w M-w`.

See more patterns in [More Examples](docs/EXAMPLES.md).

---

## Practical Examples

Here are several practical examples that showcase the power of this library when used as intended.

### 1. Jump to Definition and Back (`M-.`)

Combine the essential programming actions of jumping to a definition and returning to your original location, all on a single key.

- **Single-press**: Jump to definition (`xref-find-definitions`).
- **Double-press**: Pop back from the jump (`xref-pop-marker-stack`).

```emacs-lisp
(double-press/define-key global-map (kbd "M-.")
                         :on-single-press 'xref-find-definitions
                         :on-double-press 'xref-pop-marker-stack)
```

### 2. Escalate Query Replace (`M-%`)

Unify the query-replace commands, escalating from a standard replace to a more powerful regex replace.

- **Single-press**: Standard query-replace (`query-replace`).
- **Double-press**: Regex query-replace (`query-replace-regexp`).

```emacs-lisp
(double-press/define-key global-map (kbd "M-%")
                         :on-single-press 'query-replace
                         :on-double-press 'query-replace-regexp)
```

### 3. Personal Prefix for Display (`C-l`)

Turn a convenient key into a small prefix for display and zoom commands on double-press:

- **Single-press**: Run standard action (`copy-region-as-kill`).
- **Double-press**: Open personal prefix (`my-display-map`)

```emacs-lisp
(define-prefix-command 'my-display-map)
(define-key my-display-map (kbd "+") 'text-scale-increase)
(define-key my-display-map (kbd "-") 'text-scale-decrease)
(define-key my-display-map (kbd "C-s") 'text-scale-mode)
(define-key my-display-map (kbd "C-f") 'toggle-frame-fullscreen)
(define-key my-display-map (kbd "C-m") 'toggle-frame-maximized)
(define-key my-display-map (kbd "C-h") 'set-frame-height)
(define-key my-display-map (kbd "C-w") 'set-frame-width)

(double-press/define-key global-map (kbd "C-l")
  :on-single-press 'recenter-top-bottom
  :on-double-press 'my-display-map)
```

### 4. MORE EXAMPLES

See more patterns and ready-to-use snippets in [docs/Examples.md](docs/EXAMPLES.md).

## Configuration

The timeout for detecting a double-press can be customized by setting the `double-press/timeout` variable (default is 0.4 seconds).

```emacs-lisp
(setq double-press/timeout 0.3) ;; Set to 0.3 seconds
```

## Development: Compile & Test

- Byte-compile:
  - Makefile: `make compile`
  - Direct: `emacs -Q --batch -L . -f batch-byte-compile double-press.el`

- Run tests (ERT):
  - Makefile: `make test`
  - Direct: `emacs -Q --batch -L . -l double-press.el -l test-double-press.el -f ert-run-tests-batch-and-exit`

- macOS custom Emacs path example:
  - `make test EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"`
  - Direct: `"/Applications/Emacs.app/Contents/MacOS/Emacs" -Q --batch -L . -l double-press.el -l test-double-press.el -f ert-run-tests-batch-and-exit`

- Temporary directory note (batch test on restricted environments):
  - `mkdir -p .tmp && TMPDIR=$PWD/.tmp emacs -Q --batch -L . -l double-press.el -l test-double-press.el -f ert-run-tests-batch-and-exit`

 

## Known Bugs

- Editing keyboard macros that include double-press key sequences with `kmacro-step-edit-macro` (`C-x C-k SPC`) may corrupt the macro contents.
  - Workaround: Use `edit-kbd-macro` (`C-x C-k e`) to edit, and finish with `C-c C-c`.
  - This limitation applies only when the macro contains double-press sequences; ordinary macros are unaffected.

## License

GPLv3 or later. See the [LICENSE](LICENSE) file for the full text.

Copyright (c) 2010-2012, 2025 K-talo Miyazaki

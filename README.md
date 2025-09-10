# double-press.el

Provides a keyboard operation method corresponding to a mouse double-click, extending Emacs's keybinding capabilities.


## Core Concept

`double-press.el` offers a simple and powerful feature: it allows you to assign two different commands to the **same key**, distinguishing between a single press and a quick double-press (a "double press").

This enables you to bind two related actions to a single, easy-to-press key, reducing the need for modifier keys like `Ctrl` or `Alt` and creating a more intuitive and efficient workflow.


## Features

- **Simple**: With a single, focused purpose, it's easy to configure and unlikely to conflict with other packages.
- **Ergonomic**: Assign a common action and a related "power" version of that action to the same convenient key.
- **Non-modal**: Integrates with standard Emacs keys without mode switches. A common pattern is to keep the original action on single-press and open a small, personal prefix on double-press (discoverable with `C-h`/`<f1>`), reducing finger travel and memory load.

---

## Installation

1.  Place `double-press.el` in a directory included in your `load-path`.
2.  Add the following line to your `init.el` or `.emacs` file:

```emacs-lisp
(require 'double-press)
```
 
## Compatibility

- Supported: Emacs 24.4+ (tested on 26-30).

## Basic Usage

Use the `double-press-define-key` function to define your keybindings.

```emacs-lisp
(double-press-define-key keymap key
                         :on-single-press command-for-single-press
                         :on-double-press command-for-double-press)
```

Tip: When a double-press leads to a prefix keymap, you can press `C-h`
or `<f1>` at the prompt to see the keymap's help. On newer Emacs,
this uses `describe-keymap` and shows a short header with the bound key.

## Examples

### 1. Magit on `<f8>`

```emacs-lisp
(double-press-define-key global-map (kbd "<f8>")
  :on-single-press 'magit-status
  :on-double-press 'magit-commit)
```
- **Single-press**: Open status (`magit-status`).
- **Double-press**: Start a commit transient (`magit-commit`).


### 2. Escalate Query Replace (`M-%`)

```emacs-lisp
(double-press-define-key global-map (kbd "M-%")
                         :on-single-press 'query-replace
                         :on-double-press 'query-replace-regexp)
```
- **Single-press**: Standard query-replace (`query-replace`).
- **Double-press**: Regex query-replace (`query-replace-regexp`).

### More Examples

See more patterns, including personal prefix examples, in [docs/EXAMPLES.md](docs/EXAMPLES.md).

---

## How Double-Press Works

- Single key: quickly press the same key twice within the time window.
  - Example: press `<f8>` twice quickly -> `<double> <f8>`.
- With modifiers: hold the modifier(s) (e.g., `Meta`/`Ctrl`/`Alt`/`Super`) and quickly press the non-modifier key twice.
  - Example: hold `Meta` and press `.` twice quickly -> `<double> M-.`.
- Do not double-tap the modifier itself; keep it held while double-pressing the target key.
- The timeout (maximum interval between the two presses) is controlled by `double-press-timeout`.


<a id="dp-configuration"></a>
## Configuration

The timeout for detecting a double-press can be customized by setting the `double-press-timeout` variable (default is 0.4 seconds).

Guidelines
- Typical thresholds: 0.3-0.5 s (common defaults on desktop UIs)
- Start points: 0.35-0.40 for stability; 0.25-0.30 for a snappier feel
- Tuning: if single-presses feel delayed, lower it; if double-presses are missed, raise it

```emacs-lisp
(setq double-press-timeout 0.35) ;; Example: stable starting point
```

## A Crucial Note on Key Selection

The utility of this library depends heavily on choosing the right keys to bind.

When a key is defined with `double-press`, Emacs must briefly wait after the first press to see if a second one is coming. Because of this brief, built-in delay, **I strongly recommend *not* assigning `double-press` to keys that are often pressed in rapid succession.**

For example, binding this to letters, numbers, or common movement keys like `C-f`, `C-b`, `C-a`, `C-e`, `C-n`, and `C-p` will likely feel sluggish and annoying.

Instead, `double-press.el` is most effective when used with keys that are typically pressed in isolation, especially those combined with modifier keys like `Ctrl` or `Alt`.

---

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

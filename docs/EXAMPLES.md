# Examples of Double-Press Usage

This page collects some ready‑to‑use examples for `double-press.el`.
Copy the snippets you want into your init file after loading:

```emacs-lisp
(require 'double-press)
```

Notes
- The timeout (maximum interval between the two presses) is controlled by `double-press/timeout`.
- If a double‑press leads to a prefix keymap, you can press `C-h` or `<f1>` to see its help.
- Prefer keys you don't press repeatedly during normal editing.
- Looking for overview and setup? See the [README](../README.md).

---

## Navigation

Jump to definition on single‑press; pop back on double‑press:

```emacs-lisp
(double-press/define-key global-map (kbd "M-.")
  :on-single-press 'xref-find-definitions
  :on-double-press 'xref-pop-marker-stack)
```

---

## Movement Keys (use with caution)

Movement keys like `C-a`/`C-e`/`C-f`/`C-b`/`C-n`/`C-p` are often pressed in rapid
succession during normal editing. Assigning double‑press to them can
introduce a slight lag while Emacs waits to see if a second press is
coming. If you still want to try it, consider a longer timeout and see
how it feels in your workflow.

Examples (use with caution):

```emacs-lisp
;; C-a: visual line start; double‑press to the true beginning of the line
(double-press/define-key global-map (kbd "C-a")
  :on-single-press 'beginning-of-visual-line
  :on-double-press 'move-beginning-of-line)

;; C-e: visual line end; double‑press to the true end of the line
(double-press/define-key global-map (kbd "C-e")
  :on-single-press 'end-of-visual-line
  :on-double-press 'move-end-of-line)
```

---

## Search & Replace

Incremental search on single‑press; regex search on double‑press:

```emacs-lisp
(double-press/define-key global-map (kbd "C-s")
  :on-single-press 'isearch-forward
  :on-double-press 'isearch-forward-regexp)

(double-press/define-key global-map (kbd "C-r")
  :on-single-press 'isearch-backward
  :on-double-press 'isearch-backward-regexp)
```

Query replace on single‑press; regex query replace on double‑press:

```emacs-lisp
(double-press/define-key global-map (kbd "M-%")
  :on-single-press 'query-replace
  :on-double-press 'query-replace-regexp)
```

---

## Personal Prefix for Window Commands

Turn a convenient key into a small prefix for window commands on double‑press:

```emacs-lisp
(define-prefix-command 'my-window-map)
(define-key my-window-map (kbd "s") 'split-window-below)
(define-key my-window-map (kbd "v") 'split-window-right)
(define-key my-window-map (kbd "o") 'other-window)
(define-key my-window-map (kbd "0") 'delete-window)
(define-key my-window-map (kbd "1") 'delete-other-windows)

(double-press/define-key global-map (kbd "M-w")
  :on-single-press 'copy-region-as-kill
  :on-double-press 'my-window-map)
```

The example `my-window-map` is like a compact, personal subset of
`ctl-x-map`. Collecting your most frequently used window/buffer
commands into a small, memorable prefix can make everyday navigation
and layout tweaks much faster. Feel free to curate your own mini
keymap and assign it to a convenient double‑press.


---

## Personal Prefix for Display Commands

Recenter on single‑press; open a small display prefix on double‑press:

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

---

## VC / Git (Magit)

Open Magit status on single‑press; start a commit on double‑press:

```emacs-lisp
(double-press/define-key global-map (kbd "<f8>")
  :on-single-press 'magit-status
  :on-double-press 'magit-commit)
```

---

## Keyboard Macros

Group frequently‑used macro commands under a function key `<f3>`, while keeping
the default repeat behavior on `<f4>` untouched:

```emacs-lisp
(require 'kmacro) ;; for `kmacro-keymap'

;; <f3>: start recording (or insert counter), double-press to open kmacro prefix
(double-press/define-key global-map (kbd "<f3>")
  :on-single-press 'kmacro-start-macro-or-insert-counter
  :on-double-press  kmacro-keymap)
```

Note:
- By default, `<f4>` is bound to `kmacro-end-or-call-macro` and is often used to
  repeat the last macro by pressing it rapidly. To keep that experience snappy,
  it is recommended not to assign a double‑press action to `<f4>`.

---

## Tips

- Start with a small set of keys and tune `double-press/timeout`.
- Prefer keys that aren’t typed repetitively; function keys and Meta‑
  modified keys often work well.
- For discoverability, use `where-is` and press `C-h`/`<f1>` when in a
  double‑press prefix map to see available bindings.

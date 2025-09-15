# Examples of Double-Press Usage

This page collects some ready-to-use examples for `double-press.el`.
Copy the snippets you want into your init file after loading:

```emacs-lisp
(require 'double-press)
```

Notes
- The timeout (maximum interval between the two presses) is controlled by `double-press-timeout` (see [README Configuration](../README.md#configuration) for thresholds and tuning).
- For `<double> + modifiers + key`: hold the modifier(s) and quickly press the non-modifier key twice; do not double-tap the modifier itself (see [README: How Double-Press Works](../README.md#how-double-press-works)).
- If a double-press leads to a prefix keymap, you can press `C-h` or `<f1>` to see its help.
- Looking for overview and setup? See the [README](../README.md).
- To keep `where-is` output in sync after rebinds, you can enable the optional helper via Customize or command (see README).

---

## Magit on `<f8>`

Open status on single-press; start a commit transient on double-press:

```emacs-lisp
(double-press-define-key global-map (kbd "<f8>")
                         :on-single-press 'magit-status
                         :on-double-press 'magit-commit)
```

Tips
- Prefer `magit-dispatch` on double-press if you want broader actions.

---

## Search & Replace: Query Replace Regexp on `<double> M-%`

Keep standard query replace on single-press; escalate to regex on double-press:

```emacs-lisp
(double-press-define-key global-map (kbd "M-%")
                         :on-single-press 'query-replace
                         :on-double-press 'query-replace-regexp)
```

---

## Editing: Comment Region on `<double> C-q`

Keep the default quoted insert on single-press; comment the active region on double-press. With a plain `C-u` prefix argument, this uncomments the region instead.

```emacs-lisp
(double-press-define-key global-map (kbd "C-q")
                         :on-single-press 'quoted-insert
                         :on-double-press 'comment-region)
```

Tips
- Comment region: `<double> C-q`
- Uncomment region: `C-u <double> C-q`

This uses the interactive behavior of `comment-region`: a plain `C-u` prefix uncomments, and a numeric prefix controls how many comment characters to add (a negative numeric prefix also uncomments).

---

## Keyboard Macros: Kmacro-Keymap on `<double> <f3>`

Group macro commands under `<f3>` while keeping `<f4>` repeat behavior untouched:

```emacs-lisp
(require 'kmacro) ;; for `kmacro-keymap'

;; <f3>: start recording (or insert counter), double-press to open kmacro prefix
(double-press-define-key global-map (kbd "<f3>")
                         :on-single-press 'kmacro-start-macro-or-insert-counter
                         :on-double-press kmacro-keymap)
```

Tips
- By default, `<f4>` calls/ends macros; avoid assigning double-press to `<f4>` to keep rapid repeats snappy.

---

## Xref: Definitions, References, and History (25.1+)

Keep default singles; use doubles for the complementary action:

```emacs-lisp
;; M-. : find definitions (single), find references (double)
(double-press-define-key global-map (kbd "M-.")
                         :on-single-press 'xref-find-definitions
                         :on-double-press 'xref-find-references)

;; M-, : go back (single), go forward (double)
;; Emacs 29.1+: xref-go-back/xref-go-forward.
;; Older Emacs: use xref-pop-marker-stack instead of xref-go-back; omit xref-go-forward.
(double-press-define-key global-map (kbd "M-,")
                         :on-single-press 'xref-go-back
                         :on-double-press 'xref-go-forward)
```

Tips
- Preserves default single-press bindings; no muscle-memory break.
- Double-press provides the complementary action for quick navigation.

---

## Display/Zoom via Personal Prefix on `<double> M-w`

Keep copy on single-press; open a small display/zoom prefix on double-press.
A personal prefix is a small keymap you open on double-press to group
related commands under one convenient key while keeping the original
single-press behavior intact:

```emacs-lisp
(define-prefix-command 'my-display-map)
(define-key my-display-map (kbd "+") 'text-scale-increase)
(define-key my-display-map (kbd "-") 'text-scale-decrease)
(define-key my-display-map (kbd "M-f") 'toggle-frame-fullscreen)
(define-key my-display-map (kbd "M-m") 'toggle-frame-maximized)
(define-key my-display-map (kbd "M-h") 'set-frame-height)
(define-key my-display-map (kbd "M-w") 'set-frame-width)

(double-press-define-key global-map (kbd "M-w")
                         :on-single-press 'copy-region-as-kill
                         :on-double-press 'my-display-map)
```

Tips
- Less finger travel: one comfortable key plus a short mnemonic.
- Lower memory load: keep related actions under one place you remember.
- Zero disruption: single-press keeps the original behavior intact.
- Discoverability: press `C-h`/`<f1>` inside the prefix to list bindings.

---

## Not Recommended Settings

Avoid assigning double-press to keys that are commonly pressed in rapid succession (e.g., letters, numbers, and frequent movement keys like `C-f`, `C-b`, `C-a`, `C-e`, `C-n`, `C-p`). Emacs must briefly wait after the first press to detect a double-press, which can make repeated keystrokes feel sluggish. Prefer keys that are typically used in isolation or with modifiers.

---

## Tips

- Start with a small set of keys and expand gradually.
- For discoverability, use `where-is` and press `C-h`/`<f1>` inside a double-press prefix map.
- Function keys and Meta-modified keys often work well.

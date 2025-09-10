;;; double-press.el --- Double-press key dispatcher -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2010-2012, 2025 K-talo Miyazaki, all rights reserved.

;; Author: K-talo Miyazaki <Keitaro dot Miyazaki at gmail dot com>
;; Created: Fri Dec 24 02:33:06 2010 JST
;; Keywords: abbrev, convenience, emulations, wp
;; URL: https://github.com/k-talo/double-press.el
;; Maintainer: K-talo Miyazaki <Keitaro dot Miyazaki at gmail dot com>
;; Version: 3.0.0
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; double-press.el lets you bind two actions to the same key by timing:
;; a single press vs a quick double-press - no mode switch required.
;;
;; Quick start:
;;
;;   (require 'double-press)
;;   ;; Keep copy on M-w; open a small window prefix on M-w M-w
;;   (define-prefix-command 'my-window-map)
;;   (define-key my-window-map (kbd "s") 'split-window-below)
;;   (define-key my-window-map (kbd "v") 'split-window-right)
;;   (double-press-define-key global-map (kbd "M-w")
;;     :on-single-press 'copy-region-as-kill
;;     :on-double-press 'my-window-map)
;;
;; Customization:
;;   - double-press-timeout    ;; maximum interval between presses (seconds)
;;   - double-press-use-prompt ;; show a prompt when reading from a prefix map
;;   - Press C-h/<f1> inside a double-press prefix to see its bindings.
;;
;; See also: README.md (overview, setup) and docs/EXAMPLES.md (snippets).

;;; Change Log:

;; v3.0.0  Wed Sep 10 10:30:00 2025 JST
;;   - Breaking: rename all public and private APIs to dash style (-)
;;     per MELPA guidelines.
;;   - Remove slash-style names; no aliases provided.
;;   - Update docs and tests to use dash names.

;; v2.0.0  Mon Sep  8 10:16:00 2025 JST
;;   - Drop Emacs 23 support; require Emacs 24.4+.

;; v1.0.1  Sun Sep  7 19:52:11 2025 JST
;;   - Fix prefix help: honor help bindings in maps
;;   - API: restore &key via defun* alias (Emacs 23+)
;;   - Docs: refine README/EXAMPLES; refine help and header comments
;;   - License/headers: add LICENSE, SPDX

;; v1.0.0   Sat Sep  6 10:25:30 2025 JST
;;   - Rename library to double-press; stabilize the API.
;;   - Switch public keywords to :on-single-press / :on-double-press.
;;   - Add dispatcher docstrings so `describe-key` shows single/double
;;     bindings and keymap listings.
;;   - Improve prefix-map help: honor help events (C-h/<f1>), use
;;     `describe-keymap` when available, and add a header.
;;   - Update `define-key` hook to use `advice-add` (clears `where-is` hints
;;     for single/double maps when rebinding; same behavior as before).
;;   - Add README with install/usage/examples; add Makefile.
;;   - Add/port ERT tests to new names and keywords.
;;
;; v0.10.0  Mon Feb 13 23:25:38 2012 JST
;;   - Show "single-press" and "double-press" bindings by `where-is'.
;;   - Cope with "emacs -nw".
;;   - Deal with keyboard macro.

;;; Code:

(defconst double-press-version "3.0.0"
  "Version of the double-press package.")

;; Emacs 24.4+ baseline: use cl-lib directly.
(require 'cl-lib)


;;; ===========================================================================
;;;
;;;  User customizable things.
;;;
;;; ===========================================================================

;;;###autoload
(defgroup double-press nil
  "\"Double Press\" key event."
  :group 'convenience)

;;;###autoload
(defcustom double-press-timeout 0.4
  "Interval of a \"double press\" key event in seconds."
  :type  'float
  :group 'double-press)

;;;###autoload
(defcustom double-press-use-prompt t
  "Non-nil means show a prompt for double-press prefix keys."
  :type  'boolean
  :group 'double-press)


;;; ===========================================================================
;;;
;;;  Public Functions.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;  (double-press-define-key keymap key &key on-single-press on-double-press)
;;                            => DISPATCHER FUNCTION (as an uninterned symbol)
;; ----------------------------------------------------------------------------
;;;###autoload
(cl-defun double-press-define-key (keymap key &key on-single-press on-double-press)
  "Bind KEY in KEYMAP to two actions: ON-SINGLE-PRESS and ON-DOUBLE-PRESS.

Returns the dispatcher (an uninterned symbol bound to an interactive
command) installed at KEY. The dispatcher decides between the two
bindings by timing: a single press or a quick double-press within
`double-press-timeout\\=' seconds.

KEY may be a string or a vector of events. Using [t] as KEY creates a
default definition used for any event not otherwise defined in KEYMAP.

ON-SINGLE-PRESS and ON-DOUBLE-PRESS accept any key definition:
  - nil (leave KEY undefined in this map)
  - command (an interactive function)
  - string or vector (treated as a keyboard macro)
  - keymap (makes KEY a prefix; press `C-h\\=' or `<f1>\\=' inside to see help)
  - symbol (indirect; resolved at lookup time)

For better discoverability, this also updates the auxiliary [single]
and [double] submaps so `where-is\\=' can show both single-press and
double-press bindings.

Example:

  (define-prefix-command \\='my-window-map)
  (define-key my-window-map (kbd \"s\") \\='split-window-below)
  (double-press-define-key global-map (kbd \"M-w\")
    :on-single-press \\='copy-region-as-kill
    :on-double-press \\='my-window-map)

See also `define-key\\=', `double-press-timeout\\=', and
`double-press-use-prompt\\='."
  (let* ((dispatcher (gensym "double-press-dispatcher-"))
         (single-map (or (lookup-key keymap [single])
                         (define-key keymap [single] (make-sparse-keymap))))
         (double-map (or (lookup-key keymap [double])
                         (define-key keymap [double] (make-sparse-keymap))))
         (doc-string (double-press--doc-dispatcher-desc on-single-press on-double-press)))
    (put dispatcher 'double-press-dispatcher-p t)
    
    ;; Bind a closure (lexical-binding) that dispatches based on timing.
    (setf (symbol-function dispatcher)
          (let ((on-single-press on-single-press)
                (on-double-press on-double-press))
            (lambda ()
              (interactive)
              (funcall #'double-press--track-event
                       (list :single-press on-single-press
                             :double-press on-double-press)))))

    ;; Attach docstring to the dispatcher symbol for `describe-key`.
    (put dispatcher 'function-documentation doc-string)
    
    ;; advice for define-key clear hints for `where-is', so
    ;; call it before giving hints for `where-is'.
    (define-key keymap key dispatcher)

    ;; Hints for `where-is'.
    (define-key single-map key on-single-press)
    (define-key double-map key on-double-press)

    dispatcher))


;;; ===========================================================================
;;;
;;;  Advices
;;;
;;; ===========================================================================

(defun double-press--define-key-advice (keymap key _def &optional _remove)
  "Clear hints for `where-is' in KEYMAP for KEY.

This runs as a :before advice for `define-key' and removes
mirror entries from the [single] and [double] submaps so that
`where-is' does not show stale bindings.  DEF and REMOVE are
ignored."
  (let ((key-def (lookup-key keymap key)))
    (when (and (symbolp key-def)
               (get key-def 'double-press-dispatcher-p))
      (let ((single-map (lookup-key keymap [single]))
            (double-map (lookup-key keymap [double])))
        ;; Clear hints for `where-is'.
        (and (keymapp single-map) (define-key single-map key nil))
        (and (keymapp double-map) (define-key double-map key nil))))))

(advice-add 'define-key :before #'double-press--define-key-advice)



;;; ===========================================================================
;;;
;;;  Private Functions.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;  (double-press--track-event EV-data) => VOID
;; ----------------------------------------------------------------------------
(defun double-press--track-event (ev-data)
  "Track key event.

EV-DATA is a list like:

  (:single-press BINDING
   :double-press BINDING)

EV-DATA will be used to handle a key event."
  ;; Start tracking "double-press" key event of a key.
  ;;
  (let* ((keys-tracking (this-command-keys-vector))
         (keys-read     nil)
         (keys-next     (cond
                         ;; Executing "double-press" event by kbd-macro.
                         ;;
                         ((and executing-kbd-macro
                               (<= (1+ executing-kbd-macro-index)
                                   (1- (length executing-kbd-macro)))
                               (eq
                                (elt executing-kbd-macro (1- executing-kbd-macro-index))
                                (elt executing-kbd-macro executing-kbd-macro-index))
                               (eq
                                (elt executing-kbd-macro (1+ executing-kbd-macro-index))
                                'double))
                          (setq executing-kbd-macro-index
                                (+ executing-kbd-macro-index 2))
                          keys-tracking)
                         ;; Executing "single-press" event by kbd-macro.
                         ;;
                         (executing-kbd-macro
                          nil)
                         ;; Not executing kbd-macro.
                         ;; Wait for "double-press" event, or another event.
                         (t
                          (with-timeout
                              (double-press-timeout 'timeout)
                            (setq keys-read
                                  (read-key-sequence-vector nil)))))))
    (cond
     ;; Got "double-press" event.
     ;;
     ((equal keys-next
             keys-tracking)
      
      (when defining-kbd-macro
        ;; Remember this event is "double-press" event.
        (store-kbd-macro-event 'double))
      
      (double-press--do-key :ev-keys keys-tracking
                            :ev-kind :double-press
                            :ev-data ev-data))
     
     ;; Got "single-press" event by timeout, or another event.
     ;;
     (t
      (when keys-read
        ;; Unread another event if any.
        (setq unread-command-events
              (append (listify-key-sequence keys-read)
                      unread-command-events)))
      
      (double-press--do-key :ev-keys keys-tracking
                            :ev-kind :single-press
                            :ev-data ev-data)))))

;; ----------------------------------------------------------------------------
;;  (double-press--do-key &key ev-keys ev-kind ev-data) => VOID
;; ----------------------------------------------------------------------------
(defun double-press--do-key (&rest options)
  "Run the action bound to the current key event.

OPTIONS is a plist with keys :ev-keys, :ev-kind, and :ev-data."
  (let* ((ev-keys (plist-get options :ev-keys))
         (ev-kind (plist-get options :ev-kind))
         (ev-data (plist-get options :ev-data))
         ;; FIXME: Write codes which handles `indirect entry'.
         (binding  (cadr (memq ev-kind ev-data)))
         (key-desc (format "%s%s"
                           (if (eq ev-kind :double-press) "<double>-" "")
                           (key-description ev-keys))))
    ;; When binding is a function, get `symbol-function'
    ;; of a binding.
    (setq binding
          (double-press--do-key-aux-expand-symbol-function
           binding key-desc))
    
    ;; When binding is a prefix key, read next event
    ;; then find binding from the prefix key.
    (setq binding
          (double-press--do-key-aux-read-with-prefix-key
           binding key-desc))
    
    (cond
     ;; Binding is a Command.
     ;;
     ((and (functionp binding)
           (commandp binding))
      (let ((this-command binding))
        (call-interactively binding)))
     
     ;; Binding is a Keyboard Macro.
     ;;
     ((arrayp binding)
      (execute-kbd-macro binding))

     ((null binding)
      ;; Do nothing
      )
     (t
       (error "Unknown binding type: \"%s\""
              binding)))))

;; ----------------------------------------------------------------------------
;;  (double-press--do-key-aux-expand-symbol-function binding key-desc) => OBJECT
;; ----------------------------------------------------------------------------
(defun double-press--do-key-aux-expand-symbol-function (binding key-desc)
  "Return the symbol function of BINDING if it is a symbol.

KEY-DESC is the human-readable description of the triggering key."
  (let (seen)
    (while (and (not (commandp binding))
                (symbolp binding)
                (not (eq (symbol-function binding)
                         binding)))
      (when (memq binding seen)
        (error "Loop in binding of %s" key-desc))
      (unless (memq binding seen)
        (push binding seen))
      (setq binding (symbol-function binding)))
    binding))

;; ----------------------------------------------------------------------------
;;  (double-press--do-key-aux-read-with-prefix-key binding key-desc) => OBJECT
;; ----------------------------------------------------------------------------
(defun double-press--do-key-aux-read-with-prefix-key (binding key-desc)
  "Read a key and return the binding for that key in BINDING.
When BINDING is a prefix key, read the next event and look it up.
KEY-DESC is the human-readable description of the triggering key.
Handles help events explicitly."
  (when (keymapp binding)
    (let* ((global-map nil)
           (overriding-local-map binding)
           (prompt (and double-press-use-prompt
                        (let ((prompt (format "%s-" key-desc)))
                          (if (fboundp 'help--append-keystrokes-help)
                              (help--append-keystrokes-help prompt)
                            prompt))))
           key key-def)
      (while (and
              (not key-def)
              ;; Use a compat reader so help events can be handled explicitly here.
              (setq key (double-press--read-one-event prompt))
              (cond ((and (double-press--help-key-p key)
                          ;; If the key is bound in the binding,
                          ;; that definition takes precedence.
                          (not (lookup-key binding
                                           (if (vectorp key) key (vector key)))))
                     ;; On a help key, display help then exit.
                     (double-press--display-help binding key-desc)
                     (setq binding nil))
                    ;; Otherwise, continue.
                    (t t)))
        (setq key (if (vectorp key) key (vector key)))
        (setq key-def (and key (lookup-key binding key)))
        (when (not key-def)
          (error "%s %s is undefined"
                 key-desc
                 (key-description key)))
        (setq binding key-def))))
  binding)

;; ----------------------------------------------------------------------------
;;  (double-press--read-one-event prompt) => event
;; ----------------------------------------------------------------------------
(defun double-press--read-one-event (prompt)
  "Read one input event with PROMPT, compatible down to Emacs 23."
  (if (fboundp 'read-key)
      (read-key prompt)
    ;; FIXME: Not tested yet this one.
    (aref (read-key-sequence prompt) 0)))

;; ----------------------------------------------------------------------------
;;  (double-press--help-key-p key) => boolean
;; ----------------------------------------------------------------------------
(defun double-press--help-key-p (key)
  "Return non-nil if KEY is a help request per `help-event-list'."
  (memq key (cons help-char help-event-list)))

;; ----------------------------------------------------------------------------
;;  (double-press--display-help keymap key-desc) => VOID
;; ----------------------------------------------------------------------------
(defun double-press--display-help (keymap key-desc)
  "Show help for KEYMAP with a header based on KEY-DESC.
Uses `describe-keymap' when available."
  (let ((help-header (format "Bindings in %s:\n\n" key-desc)))
    (cond
     ;; When `describe-keymap' is available
      ((fboundp 'describe-keymap)
       (funcall #'describe-keymap keymap)
      (save-excursion
        (with-current-buffer "*Help*"
          (let ((buffer-read-only nil))
            (goto-char 0)
            (insert help-header)))))
     ;; Fallback: generate the help buffer ourselves.
     (t
      (with-help-window (help-buffer)
        (princ help-header)
        (princ (double-press--doc-keymap-desc keymap)))))))



;;; ===========================================================================
;;;
;;;  Manipulate doc string.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;  (double-press--doc-dispatcher-desc single-key-def double-key-def) => string
;; ----------------------------------------------------------------------------
(defun double-press--doc-dispatcher-desc (single-key-def double-key-def)
  "Return a dispatcher docstring from SINGLE-KEY-DEF and DOUBLE-KEY-DEF.
This helps `describe-key' show single-press and double-press bindings."
  (let* ((single-key-def-desc (double-press--doc-key-def-desc single-key-def))
         (double-key-def-desc (double-press--doc-key-def-desc double-key-def))
         (single-keymap-sym (and (keymapp single-key-def)
                                 (double-press--doc-find-keymap-var-name single-key-def)))
         (double-keymap-sym (and (keymapp double-key-def)
                                 (double-press--doc-find-keymap-var-name double-key-def)))
         (single-keymap-desc (cond
                              (single-keymap-sym
                               (format ":\n\\{%s}" single-keymap-sym))
                              ((keymapp single-key-def)
                               (concat "\n" (double-press--doc-keymap-desc single-key-def)))
                              (t "")))
         (double-keymap-desc (cond
                              (double-keymap-sym
                               (format ":\n\\{%s}" double-keymap-sym))
                              ((keymapp double-key-def)
                               (concat "\n" (double-press--doc-keymap-desc double-key-def)))
                              (t "")))
         (doc-string (format (concat
                              "A double-press event dispatcher created by `double-press-define-key'.\n"
                              "Dispatches by timing: single on one press; double on two quick presses.\n\n"
                              "[On Single Press]: %s%s\n"
                              "\n[On Double Press]: %s%s")
                             single-key-def-desc single-keymap-desc
                             double-key-def-desc double-keymap-desc)))
    doc-string))

;; ----------------------------------------------------------------------------
;;  (double-press--doc-key-def-desc key-def) => string
;; ----------------------------------------------------------------------------
(defun double-press--doc-key-def-desc (key-def)
  "Return a human-readable description for KEY-DEF."
  (cond ((null key-def) "none")
        ((keymapp key-def)
         (or (and (symbolp key-def) (boundp key-def)
                  (format "bound to the keymap `%s'" key-def))
             (and (double-press--doc-find-keymap-var-name key-def)
                  (format "bound to the keymap `%s'"
                          (double-press--doc-find-keymap-var-name key-def)))
             "bound to an unnamed keymap"))
        ((symbolp key-def) (format "bound to `%s'" (symbol-name key-def)))
        ((vectorp key-def) (format "bound to a keyboard macro:\n[%s]" (format-kbd-macro key-def)))
        ((stringp key-def) (format "bound to a keyboard macro:\n\"%s\"" key-def))
        ((functionp key-def) (format "bound to a function:\n%s" key-def))
        (t (format "%S" key-def))))

;; ----------------------------------------------------------------------------
;;  (double-press--doc-keymap-desc keymap) => string
;; ----------------------------------------------------------------------------
(defun double-press--doc-keymap-desc (keymap)
  "Return a human-readable listing for KEYMAP bindings."
  (with-temp-buffer
    (map-keymap
     (lambda (ev key-def)
       (let* ((vec (vector ev))
              (key-desc (condition-case _
                            (key-description vec)
                          (error (format "%S" ev))))
              (key-def-desc (double-press--doc-key-def-desc key-def)))
         (insert (format "  %s -> %s\n" key-desc key-def-desc))))
     keymap)
    (buffer-string)))

;; ----------------------------------------------------------------------------
;;  (double-press--doc-find-keymap-var-name keymap) => string
;; ----------------------------------------------------------------------------
(defun double-press--doc-find-keymap-var-name (keymap)
  "Return a global symbol holding KEYMAP.
Prefers symbols that look like real keymap globals and avoids common
let-bound locals, especially on dynamic-scope Emacs (e.g., 23)."
  (let (candidates)
    (mapatoms
     (lambda (sym)
       (when (and (boundp sym)
                  (eq (symbol-value sym) keymap)
                  (not (keywordp sym)))
         (let* ((name (symbol-name sym))
                ;; Denylist: common locals we bind in this file and
                ;; generic names often used in dynamic scope.
                (deny '(keymap key key-def binding prompt doc-string
                               on-single-press on-double-press
                               single-key-def double-key-def
                               ev-keys ev-kind ev-data))
                (skip (memq sym deny))
                (score 0))
           (unless skip
             ;; Prefer conventional keymap variable names.
             (when (string-match-p "-\(mode-\)?map$" name)
               (setq score (+ score 2)))
             ;; Prefer documented variables (global-ish).
             (when (documentation-property sym 'variable-documentation)
               (setq score (1+ score)))
             (push (cons score sym) candidates))))))
    (when candidates
      (cdr (car (sort candidates (lambda (a b) (> (car a) (car b)))))))))

;; Provide feature at end of file per packaging conventions.
(provide 'double-press)

;;; double-press.el ends here

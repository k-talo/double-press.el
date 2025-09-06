;; double-press.el --- keyboard operation method corresponding to a mouse double-click. -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2012, 2025 K-talo Miyazaki, all rights reserved.

;; Author: K-talo Miyazaki <Keitaro dot Miyazaki at gmail dot com>
;; Created: Fri Dec 24 02:33:06 2010 JST
;; Keywords: abbrev convenience emulations wp
;; GitHub: http://github.com/k-talo/double-press.el
;; Version: 1.0.0

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

;;; NOTE
;;
;; This library is just tested on Emacs 23.2.1 on Mac OS X 10.6.5,
;; and won't be run with any version of XEmacs.

;;; Commentary:
;;
;; Overview
;; ========
;; This library provides a keyboard operation method corresponding
;; to a mouse double-click.
;;
;;
;; INSTALLING
;; ==========
;; To install this library, save this file to a directory in your
;; `load-path' (you can view the current `load-path' using "C-h v
;; load-path RET" within Emacs), then add the following line to your
;; .emacs startup file:
;;
;;    (require 'double-press)
;;
;;
;; USING
;; =====
;; You can bind commands to keyboard events "single-press" and
;; "double-press" on a key via a function `double-press/define-key'
;; which is provided by this library.
;;
;;   Examples)
;;
;;     ;; Save buffer with key <double-C-s>.
;;     (double-press/define-key global-map "\C-s"
;;                             :on-single-press 'isearch-forward
;;                             :on-double-press 'save-buffer)
;;     
;;     ;; Other window with key <double-C-o>.
;;     (double-press/define-key global-map "\C-o"
;;                             :on-single-press 'open-line
;;                             :on-double-press 'other-window)
;;     
;;     ;; Open RE-Builder with key <double-C-r>.
;;     (double-press/define-key global-map "\C-r"
;;                             :on-single-press 'isearch-backward
;;                             :on-double-press 're-builder)
;;     
;;     ;; Insert current time with key <double-M-t>.
;;     (double-press/define-key esc-map "t"
;;                             :on-single-press 'transpose-words
;;                             :on-double-press (lambda ()
;;                                               (interactive)
;;                                               (insert (current-time-string))))
;;     
;;     ;; Insert mail address by keyboard macro with key <double-@>.
;;     (double-press/define-key global-map "@"
;;                             :on-single-press 'self-insert-command
;;                             :on-double-press "my-name@example.com")
;;     
;;     ;; Use <double-C-w> as prefix key `ctl-x-4-map'.
;;     (double-press/define-key global-map "\C-w"
;;                             :on-single-press 'kill-region
;;                             :on-double-press ctl-x-4-map)
;;
;;   NOTE: You should avoid assigning the "double-press" event to keys
;;         that are often pressed in rapid succession, such as "C-f" or
;;         "C-b". Such assignments can feel sluggish and annoying.
;;
;;
;; KNOWN PROBLEMS
;; ==============
;;
;;
;; WISH LIST
;; =========
;; - Show commands bound to "double-press" event by `describe-key'.

;;; Change Log:

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

(provide 'double-press)

(defconst double-press/version "1.0.0")

(eval-and-compile
  (require 'cl-lib))


;;; ===========================================================================
;;;
;;;  User customizable things.
;;;
;;; ===========================================================================

(defgroup double-press nil
  "\"Double Press\" key event."
  :group 'convenience)

(defcustom double-press/timeout 0.4
  "Interval of a \"double press\" key event in seconds."
  :type  'float
  :group 'double-press)

(defcustom double-press/use-prompt t
  "Non nil to display prompt for prefix keys which are
bound to a key by `double-press/define-key'."
  :type  'boolean
  :group 'double-press)


;;; ===========================================================================
;;;
;;;  Public Functions.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;  (double-press/define-key keymap key &key on-single-press on-double-press)
;;                                                                     => VOID
;; ----------------------------------------------------------------------------
(cl-defun double-press/define-key (keymap key
                                          &key
                                          on-single-press
                                          on-double-press)
  "In KEYMAP, define key sequence KEY as ON-SINGLE-PRESS and
ON-DOUBLE-PRESS.

KEY is a string or a vector of symbols and characters meaning a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.
Using [t] for KEY creates a default definition, which applies to any
event type that has no other definition in this keymap.

ON-SINGLE-PRESS ON-DOUBLE-PRESS are anything that can be a
key's definition:

 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.).

See also `define-key'."
  (let ((fn-name (gensym "double-press/dispatcher-"))
        (single-map (or (lookup-key keymap [single])
                        (define-key keymap [single] (make-sparse-keymap))))
        (double-map (or (lookup-key keymap [double])
                        (define-key keymap [double] (make-sparse-keymap))))
        (doc-string (double-press/doc/.dispatcher-desc on-single-press on-double-press)))
    (put fn-name 'double-press/dispatcher-p t)
    
    ;; Bind a closure, which handles event by the KEY, to a KEY.
    (setf (symbol-function fn-name)
          ;; NOTE: use lexical-let in non lexical-binding environment.
          (let ((on-single-press on-single-press)
                (on-double-press on-double-press))
            #'(lambda ()
                (:documentation doc-string)
                (interactive)
                (funcall 'double-press/.track-event
                         (list :single-press on-single-press
                               :double-press on-double-press)))))
    ;; Hints for `where-is'.
    (define-key single-map key on-single-press)
    (define-key double-map key on-double-press)
    
    (define-key keymap key fn-name)))


;;; ===========================================================================
;;;
;;;  Advices
;;;
;;; ===========================================================================

(defun double-press/.define-key-advice (keymap key _def &optional _remove)
  "Clear hints for `where-is'."
  (let ((key-def (lookup-key keymap key)))
    (when (and (symbolp key-def)
               (get key-def 'double-press/dispatcher-p))
      (let ((single-map (lookup-key keymap [single]))
            (double-map (lookup-key keymap [double])))
        ;; Clear hints for `where-is'.
        (and (keymapp single-map) (define-key single-map key nil))
        (and (keymapp double-map) (define-key double-map key nil))))))
(advice-add 'define-key :before #'double-press/.define-key-advice)


;;; ===========================================================================
;;;
;;;  Private Functions.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;  (double-press/.track-event EV-data) => VOID
;; ----------------------------------------------------------------------------
(defun double-press/.track-event (ev-data)
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
                              (double-press/timeout 'timeout)
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
      
      (double-press/.do-key :ev-keys keys-tracking
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
      
      (double-press/.do-key :ev-keys keys-tracking
                            :ev-kind :single-press
                            :ev-data ev-data)))))

;; ----------------------------------------------------------------------------
;;  (double-press/.do-key &key ev-keys ev-kind ev-data) => VOID
;; ----------------------------------------------------------------------------
(cl-defun double-press/.do-key (&key ev-keys ev-kind ev-data)
  "Run the action bound to the current key event."
  ;; FIXME: Write codes which handles `indirect entry'.
  (let ((binding  (cadr (memq ev-kind ev-data)))
        (key-desc (format "%s%s"
                          (if (eq ev-kind :double-press) "<double>-" "")
                          (key-description ev-keys))))
    ;; When binding is a function, get `symbol-function'
    ;; of a binding.
    (setq binding
          (double-press/.do-key/aux/expand-symbol-function
           binding key-desc))
    
    ;; When binding is a prefix key, read next event
    ;; then find binding from the prefix key.
    (setq binding
          (double-press/.do-key/aux/read-with-prefix-key
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
      (error "Unknown binding type: \"%s\"."
             binding)))))

;; ----------------------------------------------------------------------------
;;  (double-press/.do-key/aux/expand-symbol-function binding key-desc) => OBJECT
;; ----------------------------------------------------------------------------
(defun double-press/.do-key/aux/expand-symbol-function (binding key-desc)
  "Returns symbol function of BINDING if it is a symbol."
  (let (seen)
    (while (and (not (commandp binding))
                (symbolp binding)
                (not (eq (symbol-function binding)
                         binding)))
      (when (memq binding seen)
        (error "Loop in binding of %s." key-desc))
      (cl-pushnew binding seen)
      (setq binding (symbol-function binding)))
    binding))

;; ----------------------------------------------------------------------------
;;  (double-press/.do-key/aux/read-with-prefix-key binding key-desc) => OBJECT
;; ----------------------------------------------------------------------------
(defun double-press/.do-key/aux/read-with-prefix-key (binding key-desc)
  "Read a key and return the binding for that key in BINDING
when BINDING is a prefix key. Handles help events explicitly."
  (when (keymapp binding)
    (let* ((global-map nil)
           (overriding-local-map binding)
           (prompt (and double-press/use-prompt
                        (let ((prompt (format "%s-" key-desc)))
                          (if (fboundp 'help--append-keystrokes-help)
                              (help--append-keystrokes-help prompt)
                            prompt))))
           key key-def)
      (while (and
              (not key-def)
              ;; Use `read-key' to capture single events so help events
              ;; (e.g., <f1>, C-h) can be handled explicitly here.
              (setq key (read-key prompt))
              (cond ((double-press/.help-key-p key)
                     ;; On a help key, display help then exit.
                     (double-press/.display-help binding key-desc)
                     (setq binding nil))
                    ;; Otherwise, continue.
                    (t t)))
        (setq key (if (vectorp key) key (vector key)))
        (setq key-def (and key (lookup-key binding key)))
        (when (not key-def)
          (error (format "%s %s is undefined"
                         key-desc
                         (key-description key))))
        (setq binding key-def))))
  binding)

;; ----------------------------------------------------------------------------
;;  (double-press/.help-key-p key) => boolean
;; ----------------------------------------------------------------------------
(defun double-press/.help-key-p (key)
  "Return non-nil if KEY is a help request per `help-event-list'.
If an element of `help-event-list' is the symbol `help', compare KEY with
the value of `help-char'."
  (let ((needle (if (eq key 'help) help-char key))
        (found nil))
    (dolist (h help-event-list)
      (let ((hv (if (eq h 'help) help-char h)))
        (when (eq needle hv)
          (setq found t)
          (cl-return))))
    found))

;; ----------------------------------------------------------------------------
;;  (double-press/.display-help keymap key-desc) => VOID
;; ----------------------------------------------------------------------------
(defun double-press/.display-help (keymap key-desc)
  "Show help for KEYMAP. Uses `describe-keymap' when available."
  (let ((help-header (format "Bindings in %s:\n\n" key-desc)))
    (cond
     ;; When `describe-keymap' is available
     ((fboundp 'describe-keymap)
      (funcall 'describe-keymap keymap)
      (save-excursion
        (with-current-buffer "*Help*"
          (let ((buffer-read-only nil))
            (goto-char 0)
            (insert help-header)))))
     ;; Fallback: generate the help buffer ourselves.
     (t
      (with-help-window (help-buffer)
        (princ help-header)
        (princ (double-press/doc/.keymap-desc keymap)))))))



;;; ===========================================================================
;;;
;;;  Manipulate doc string.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;  (double-press/doc/.dispatcher-desc single-key-def double-key-def) => string
;; ----------------------------------------------------------------------------
(defun double-press/doc/.dispatcher-desc (single-key-def double-key-def)
  ;; Provide a docstring so `describe-key` can show single/double bindings.
  (let* ((single-key-def-desc (double-press/doc/.key-def-desc single-key-def))
         (double-key-def-desc (double-press/doc/.key-def-desc double-key-def))
         (single-keymap-sym (and (keymapp single-key-def)
                                 (double-press/doc/.find-keymap-var-name single-key-def)))
         (double-keymap-sym (and (keymapp double-key-def)
                                 (double-press/doc/.find-keymap-var-name double-key-def)))
         (single-keymap-desc (cond
                              (single-keymap-sym
                               (format ":\n\\{%s}" single-keymap-sym))
                              ((keymapp single-key-def)
                               (concat "\n" (double-press/doc/.keymap-desc single-key-def)))
                              (t "")))
         (double-keymap-desc (cond
                              (double-keymap-sym
                               (format ":\n\\{%s}" double-keymap-sym))
                              ((keymapp double-key-def)
                               (concat "\n" (double-press/doc/.keymap-desc double-key-def)))
                              (t "")))
         (doc-string (format (concat
                              "A double-press event dispatcher created by `double-press/define-key'.\n"
                              "Dispatches by timing: single on one press; double on two quick presses.\n\n"
                              "[On Single Press]: %s%s\n"
                              "\n[On Double Press]: %s%s")
                             single-key-def-desc single-keymap-desc
                             double-key-def-desc double-keymap-desc)))
    doc-string))

;; ----------------------------------------------------------------------------
;;  (double-press/doc/.key-def-desc key-def) => string
;; ----------------------------------------------------------------------------
(defun double-press/doc/.key-def-desc (key-def)
  (cond ((null key-def) "none")
        ((keymapp key-def)
         (or (and (symbolp key-def) (boundp key-def)
                  (format "bound to the keymap `%s'" key-def))
             (and (double-press/doc/.find-keymap-var-name key-def)
                  (format "bound to the keymap `%s'"
                          (double-press/doc/.find-keymap-var-name key-def)))
             "bound to an unnamed keymap"))
        ((symbolp key-def) (format "bound to `%s'" (symbol-name key-def)))
        ((vectorp key-def) (format "bound to a keyboard macro:\n[%s]" (format-kbd-macro key-def)))
        ((stringp key-def) (format "bound to a keyboard macro:\n\"%s\"" key-def))
        ((functionp key-def) (format "bound to a function:\n%s" key-def))
        (t (format "%S" key-def))))

;; ----------------------------------------------------------------------------
;;  (double-press/doc/.keymap-desc keymap) => string
;; ----------------------------------------------------------------------------
(defun double-press/doc/.keymap-desc (keymap)
  (with-temp-buffer
    (map-keymap
     (lambda (ev key-def)
       (let* ((vec (vector ev))
              (key-desc (condition-case _
                            (key-description vec)
                          (error (format "%S" ev))))
              (key-def-desc (double-press/doc/.key-def-desc key-def)))
         (insert (format "  %s -> %s\n" key-desc key-def-desc))))
     keymap)
    (buffer-string)))

;; ----------------------------------------------------------------------------
;;  (double-press/doc/.find-keymap-var-name keymap) => string
;; ----------------------------------------------------------------------------
(defun double-press/doc/.find-keymap-var-name (keymap)
  (let (keymap-var-name-lst)
    (mapatoms (lambda (sym)
                (when (and (boundp sym)
                           (eq (symbol-value sym) keymap))
                  (setq keymap-var-name-lst (cons sym keymap-var-name-lst)))))
    ;; Pick up first one.
    (car keymap-var-name-lst)))

;;; double-press.el ends here

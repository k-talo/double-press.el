;;; test-double-press.el --- ERT tests for double-press.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "double-press.el" default-directory) nil t)

;;; expand-symbol-function

(ert-deftest double-press/expand-symbol-function-resolves-indirection ()
  (cl-letf* (((symbol-function 'dt-test-c) (lambda () 'cc))
             ((symbol-function 'dt-test-b) 'dt-test-c)
             ((symbol-function 'dt-test-a) 'dt-test-b))
    (let ((fn (double-press/.do-key/aux/expand-symbol-function 'dt-test-a "K")))
      (should (functionp fn))
      (should (eq fn (symbol-function 'dt-test-c))))))

(ert-deftest double-press/expand-symbol-function-detects-loop ()
  (let ((orig-symf (symbol-function 'symbol-function)))
    (cl-letf (((symbol-function 'symbol-function)
               (lambda (sym)
                 (pcase sym
                   ('dt-loop-a 'dt-loop-b)
                   ('dt-loop-b 'dt-loop-a)
                   (_ (funcall orig-symf sym))))))
      (should-error (double-press/.do-key/aux/expand-symbol-function 'dt-loop-a "K")
                    :type 'error))))

;;; read-with-prefix-key

(ert-deftest double-press/read-with-prefix-key-picks-binding ()
  (let ((double-press/use-prompt nil)
        (km (make-sparse-keymap)))
    (define-key km (kbd "x") 'next-line)
    (cl-letf (((symbol-function 'read-key)
               (lambda (&optional _prompt) ?x)))
      (should (eq (double-press/.do-key/aux/read-with-prefix-key km "K") 'next-line)))))

(ert-deftest double-press/read-with-prefix-key-undefined-errors ()
  (let ((double-press/use-prompt nil)
        (km (make-sparse-keymap)))
    (cl-letf (((symbol-function 'read-key)
               (lambda (&optional _prompt) ?x)))
      (should-error (double-press/.do-key/aux/read-with-prefix-key km "K")
                    :type 'error))))

;;; .do-key execution paths

(ert-deftest double-press/do-key-runs-command ()
  (let ((flag nil))
    (fset 'dt-test-cmd (lambda () (interactive) (setq flag :ran)))
    (double-press/.do-key :ev-keys (kbd "C-a")
                          :ev-kind :single-press
                          :ev-data (list :single-press 'dt-test-cmd
                                         :double-press nil))
    (should (eq flag :ran))))

(ert-deftest double-press/do-key-executes-keyboard-macro ()
  (with-temp-buffer
    (let ((initial (buffer-string)))
      (double-press/.do-key :ev-keys (kbd "a")
                            :ev-kind :single-press
                            :ev-data (list :single-press (kbd "a")
                                           :double-press nil))
      (should (string= (buffer-string) (concat initial "a"))))))

(ert-deftest double-press/do-key-with-prefix-map-runs-bound-command ()
  (let ((double-press/use-prompt nil)
        (flag nil)
        (km (make-sparse-keymap)))
    (fset 'dt-test-cmd (lambda () (interactive) (setq flag :prefix)))
    (define-key km (kbd "x") 'dt-test-cmd)
    (cl-letf (((symbol-function 'read-key) (lambda (&optional _prompt) ?x)))
      (double-press/.do-key :ev-keys (kbd "M-x")
                            :ev-kind :double-press
                            :ev-data (list :single-press nil
                                           :double-press km))
      (should (eq flag :prefix)))))

;;; .track-event paths

(ert-deftest double-press/track-event-single-press-on-timeout ()
  (let* ((double-press/timeout 0.01)
         (flag nil))
    (fset 'dt-test-single (lambda () (interactive) (setq flag :single)))
    (cl-letf (((symbol-function 'this-command-keys-vector)
               (lambda () (kbd "a")))
              ;; Simulate waiting longer than timeout so it falls back to single-type
              ((symbol-function 'read-key-sequence-vector)
               (lambda (&optional _prompt)
                 (sleep-for 0.05)
                 (kbd "z"))))
      (double-press/.track-event (list :single-press 'dt-test-single
                                       :double-press nil))
      (should (eq flag :single)))))

(ert-deftest double-press/track-event-double-press-in-kbd-macro ()
  (let ((flag nil)
        (vec (vconcat (kbd "a") (kbd "a") [double])))
    (fset 'dt-test-double (lambda () (interactive) (setq flag :double)))
    (cl-letf* (((symbol-function 'this-command-keys-vector)
                (lambda () (kbd "a")))
               ;; Ensure the timeout path is not consulted in this branch
               ((symbol-function 'read-key-sequence-vector)
                (lambda (&optional _prompt)
                  (error "read-key-sequence-vector should not be called")))
               ((symbol-value 'executing-kbd-macro) vec)
               ((symbol-value 'executing-kbd-macro-index) 1))
      (double-press/.track-event (list :single-press nil
                                       :double-press 'dt-test-double))
      (should (eq flag :double))
      ;; Index should advance by +2 per implementation
      (should (= executing-kbd-macro-index 3)))))

;;; define-key integration and advice

(ert-deftest double-press/define-key-adds-hints-and-advice-clears ()
  (let* ((km (make-sparse-keymap))
         (key (kbd "x")))
    (double-press/define-key km key
                             :on-single-press 'next-line
                             :on-double-press 'other-window)
    (let ((single-map (lookup-key km [single]))
          (double-map (lookup-key km [double])))
      (should (keymapp single-map))
      (should (keymapp double-map))
      (should (eq (lookup-key single-map key) 'next-line))
      (should (eq (lookup-key double-map key) 'other-window))
      ;; Redefine original key; advice should clear hints
      (define-key km key 'self-insert-command)
      (should (null (lookup-key single-map key)))
      (should (null (lookup-key double-map key))))))

;;; prompt behavior when reading from prefix map

(ert-deftest double-press/read-with-prefix-key-shows-prompt ()
  (let* ((double-press/use-prompt t)
         (km (make-sparse-keymap))
         (seen-prompt nil))
    (define-key km (kbd "x") 'ignore)
    (cl-letf (((symbol-function 'help--append-keystrokes-help)
               (lambda (p) p))
              ((symbol-function 'read-key)
               (lambda (&optional prompt)
                 (setq seen-prompt prompt)
                 ?x)))
      (let ((res (double-press/.do-key/aux/read-with-prefix-key km "K")))
        (should (equal seen-prompt "K-"))
        (should (eq res 'ignore))))))

;;; unknown binding type errors

(ert-deftest double-press/do-key-unknown-binding-type-errors ()
  (should-error (double-press/.do-key :ev-keys (kbd "C-a")
                                      :ev-kind :single-press
                                      :ev-data (list :single-press 123
                                                     :double-press nil))
                :type 'error))

;;; test-double-press.el ends here

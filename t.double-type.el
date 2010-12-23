(require 'el-test-more)
(flet ((a () 'aa)
       (b () 'bb)
       (c () 'cc))
  (setf (symbol-function 'b) 'c)
  (setf (symbol-function 'a) 'b)
  
  (symbol-function 'a)
  (etm/eq (double-type/expand-symbol-function 'a nil)
          (symbol-function 'c)))

(flet ((a () 'aa)
       (b () 'bb))
  (setf (symbol-function 'b) 'a)
  (setf (symbol-function 'a) 'b)
  (etm/signals-ok (double-type/expand-symbol-function 'a nil)
                  '(error "Loop in binding of nil.")))

;; (double-type/define-key esc-map "q"
;;                         :on-single-type (lambda () (interactive) (message "%s Single1" (mydate)))
;;                         :on-double-type (lambda () (interactive) (message "%s Double1" (mydate))))

;; (double-type/define-key esc-map "."
;;                         :on-single-type (lambda () (interactive) (message "%s Single2" (mydate)))
;;                         :on-double-type (lambda () (interactive) (message "%s Double2" (mydate))))

;; (defun mydate ()
;;   (format-time-string "[%p%H:%M:%S] " (current-time)))


(defun .test-fn (&rest args)
  (interactive "p")
  (message "ARGS: %s, PREFIX: %S, CMD: %s" args prefix-arg this-command))

(double-type/define-key esc-map  "q"
                        :on-single-type '.test-fn
                        :on-double-type '.test-fn)

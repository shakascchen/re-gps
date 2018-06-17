(in-package :cl-user)

(defpackage re-gps-op2
  (:use :cl :re-gps-util2)
  (:export #:op
           #:convert-op
           #:op-action
           #:op-preconds
           #:op-add-list
           #:op-del-list
           #:make-op))
(in-package :re-gps-op2)

(defstruct op "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &rest rest &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op (apply 'make-op
                     (append `(:action ,action)
                             rest))))

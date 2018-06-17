(in-package :cl-user)

(defpackage re-gps
  (:use :cl)
  (:export #:use-gps
           #:use
           #:gps))
(in-package :re-gps)

(defun use-gps (gps-function-symbol)
  (values (setf (symbol-function 'gps)
                (symbol-function gps-function-symbol))
          (setf (symbol-function 'use)
                (symbol-function (alexandria:ensure-symbol :use
                                                           (symbol-package gps-function-symbol))))))


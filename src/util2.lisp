(in-package :cl-user)

(defpackage re-gps-util2
  (:use :cl)
  (:export #:member-equal
           #:executing-p
           #:action-p
           #:orderings
           #:recursive-symbol->keyword
           #:find-all
           #:eql-symbol-name))
(in-package :re-gps-util2)

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun symbol->keyword (atom)
  (if (symbolp atom)
      (alexandria:make-keyword (symbol-name atom))
      atom))

(defun eql-symbol-name (x y)
  (apply 'eql
         (mapcar 'symbol->keyword
                 (list x y))))

(defun recursive-symbol->keyword (elt)
  (if (consp elt)
      (mapcar 'recursive-symbol->keyword elt)
      (symbol->keyword elt)))

(defun member-equal (item list)
  (member (recursive-symbol->keyword item)
          list
          :test #'equal :key #'recursive-symbol->keyword))

(defun executing-p (x)
  "Is x of the form: (executing ...) ?"
  (starts-with x :executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list)
       (eql-symbol-name (first list) x)))

(defun action-p (x)
  "Is x something that is (start) or (executing ...)?"
  (or (equal (and (listp x)
                  (null (cdr x))
                  `(,(symbol->keyword (first x))))
             '(:start))
      (executing-p x)))

(defun orderings (l) 
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))


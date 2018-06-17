(in-package :cl-user)

(defpackage re-gps-gps2
  (:use :cl :re-gps-op2)
  (:import-from #:re-gps-util2
                #:member-equal
                #:recursive-symbol->keyword
                #:orderings
                #:action-p
                #:find-all)
  (:import-from #:re-gps-debug
                #:dbg-indent)
  (:export #:gps
           #:use))
(in-package :re-gps-gps2)

(defvar *ops* nil "A list of available operators.")

(defun use (oplist)
  "Use oplist as the default list of operators."
  ;; Return something useful, but not too verbose: 
  ;; the number of operators.
  (length (setf *ops* oplist)))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member-equal goal (op-add-list op)))

(defun appropriate-ops (goal state)
  "Return a list of appropriate operators, 
  sorted by the number of unfulfilled preconditions."
  (sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'<
        :key #'(lambda (op) 
                 (count-if #'(lambda (precond)
                               (not (member-equal precond state)))
                           (op-preconds op)))))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op) 
                             (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state
      (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
      (append (remove-if #'(lambda (x) 
                             (member-equal x (op-del-list op)))
                         state2)
              (op-add-list op)))))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, trying several orderings."
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
        (orderings goals)))

(defun achieve-each (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal :key #'recursive-symbol->keyword))
        current-state)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (appropriate-ops goal state))))) 

(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (remove-if-not #'action-p
                 (achieve-all (cons '(start) state) goals nil)))

;; (defun GPS (state goals &optional (*ops* *ops*))
;;   "General Problem Solver: from state, achieve goals using *ops*."
;;   (find-all-if #'action-p
;;                (achieve-all (cons '(start) state) goals nil)))

;; (defun achieve-all (state goals goal-stack)
;;   "Achieve each goal, trying several orderings."
;;   (some #'(lambda (goals) (achieve-each state goals goal-stack))
;;         (orderings goals)))

;; (defun achieve-each (state goals goal-stack)
;;   "Achieve each goal, and make sure they still hold at the end."
;;   (let ((current-state state))
;;     (when (and (setf current-state
;;                      (achieve current-state
;;                               (first goals)
;;                               goal-stack
;;                               (rest goals)))
;;                (subsetp goals current-state :test #'equal))
;;       current-state)))

;; (defun achieve (state goal goal-stack remaining-goals)
;;   "A goal is achieved if it already holds,
;;   or if there is an appropriate op for it that is applicable."
;;   (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
;;   (cond ((or (null goal)
;;              (member-equal goal state))
;;          state)
;;         ((member-equal goal goal-stack) nil)
;;         (t (some #'(lambda (op)
;;                      (achieve-all (apply-op state goal op goal-stack)
;;                                   remaining-goals
;;                                   goal-stack))
;;                  (appropriate-ops goal state))))) 


;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Sequence utilities

(def (function ioe) the-only-element (elements)
  (debug-only (assert (length= 1 elements)))
  (elt elements 0))

(def (function ioe) the-non-nil (value)
  (debug-only (assert value))
  value)

(def (function e) ensure-sequence (thing)
  (if (typep thing 'sequence)
      thing
      (list thing)))

(def (function e) all-eq (list)
  "Returns t if all elements in the LIST are eq."
  (let ((first-element (first list)))
    (if first-element
	(loop for element in list
	      when (not (eq first-element element))
	      do (return nil)
	      finally (return t))
	t)))

(def (function e) collect-if (predicate sequence)
  "Collects elements from SEQUENCE for which the PREDICATE is true."
  (remove-if (complement predicate) sequence))

(def (function e) collect-if-typep (type sequence)
  "Collects elements from SEQUENCE with the given TYPE."
  (collect-if (lambda (element) (typep element type)) sequence))

(def (function e) filter-out (element sequence &key (key #'identity) (test #'eq))
  "Filters out element from SEQUENCE."
  (remove element sequence :key key :test-not test))

(def (function e) filter-out-if (predicate sequence &key (key #'identity))
  "Filters out elements from SEQUENCE for which the PREDICATE is true."
  (remove-if (complement predicate) sequence :key key))

(def (function e) optional-list (&rest elements)
  (remove nil elements))

(def (function e) optional-list* (&rest elements)
  (remove nil (apply #'list* elements)))

(def (macro e) foreach (function first-sequence &rest more-sequences)
  `(map nil ,function ,first-sequence ,@more-sequences))

(def (function e) partition (sequence &rest predicates)
  (iter (with result = (make-array (length predicates) :initial-element nil))
        (for element :in-sequence sequence)
        (iter (for predicate :in predicates)
              (for index :from 0)
              (when (funcall predicate element)
                (push element (aref result index))
                (finish)))
        (finally
         (return
           (iter (for element :in-vector result)
                 (collect (nreverse element)))))))

(def (function e) split-sequence-by-partitioning (sequence &rest predicates)
  (iter outer
        (with length = (length sequence))
        (with position = 0)
        (while (< position length))
        (for element = (elt sequence position))
        (for part = (iter inner
                          (for index :from 0)
                          (for predicate = (elt predicates index))
                          (when (funcall predicate element)
                            (return-from inner (subseq sequence position
                                                       (position-if (lambda (element)
                                                                      (or (iter (for preceding-predicate-index :from 0 :below index)
                                                                                (thereis (funcall (elt predicates preceding-predicate-index) element)))
                                                                          (not (funcall predicate element))))
                                                                    sequence :start position))))))
        (if part
            (progn
              (collect part)
              (incf position (length part)))
            (incf position))))

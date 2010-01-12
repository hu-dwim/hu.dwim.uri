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
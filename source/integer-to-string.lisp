;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Integer to string

(def constant +integer-to-string-cache-size+ 128)

(def (constant :test 'equalp) +cached-integer-names+
    (coerce (iter (for idx :from 0 :below +integer-to-string-cache-size+)
                  (collect (coerce (princ-to-string idx) 'simple-base-string)))
            `(simple-array string (,+integer-to-string-cache-size+))))

(def (function io) %integer-to-string (integer &key minimum-column-count (maximum-digit-count most-positive-fixnum) (divisor 10))
  (declare (type integer integer)
           (type (or null fixnum) minimum-column-count)
           (type fixnum maximum-digit-count))
  (if (< integer +integer-to-string-cache-size+)
      (aref +cached-integer-names+ integer)
      (bind ((remainder integer)
             (digit 0)
             (digits-count 0)
             (digits '())
             (result-index 0)
             (result (make-array 128 :element-type 'base-char)))
        (declare (dynamic-extent digits result)
                 (type fixnum digit digits-count result-index))
        (macrolet ((emit (char)
                     `(progn
                        (setf (aref result result-index) ,char)
                        (incf result-index))))
          (iter (repeat maximum-digit-count)
                (setf (values remainder digit) (truncate remainder divisor))
                (push digit digits)
                (incf digits-count)
                (until (zerop remainder)))
          (when minimum-column-count
            (bind ((padding-length (- minimum-column-count digits-count)))
              (when (plusp padding-length)
                (iter (repeat padding-length)
                      (emit #\0)))))
          (dolist (digit digits)
            (emit (code-char (+ #x30 digit)))))
        (bind ((real-result (make-array result-index :element-type 'base-char)))
          (replace real-result result :end1 result-index)
          real-result))))

(def (function eio) integer-to-string (integer &key minimum-column-count (maximum-digit-count most-positive-fixnum) (divisor 10))
  (check-type integer integer)
  (check-type minimum-column-count (or null fixnum))
  (check-type maximum-digit-count fixnum)
  (etypecase integer
    (fixnum (%integer-to-string integer
                                :minimum-column-count minimum-column-count
                                :maximum-digit-count maximum-digit-count
                                :divisor divisor))
    (integer (%integer-to-string integer
                                 :minimum-column-count minimum-column-count
                                 :maximum-digit-count maximum-digit-count
                                 :divisor divisor))))

(declaim (notinline integer-to-string)) ; make it inlinable, but don't make it inline by default
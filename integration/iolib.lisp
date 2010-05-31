;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(with-muffled-redefinition-warnings
  (def (function o :inline :possible) get-monotonic-time ()
    "Returns such a time measure that constantly grows (it's a number in seconds, and it's unaffected by setting the system clock)."
    (isys:get-monotonic-time)))

(def (function e) posix-process-exists? (pid)
  (ignore-errors
    (isys:kill pid 0)
    #t))

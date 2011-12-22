;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.uri.documentation
  (:use :hu.dwim.asdf
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.presentation
        :hu.dwim.syntax-sugar
        :hu.dwim.uri)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.presentation)))

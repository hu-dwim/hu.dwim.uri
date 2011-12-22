;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.uri.test
  :class hu.dwim.test-system
  :depends-on (:hu.dwim.stefil+hu.dwim.def+swank
               :hu.dwim.uri
               :hu.dwim.util)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "parsing" :depends-on ("suite"))
                             (:file "mutation" :depends-on ("suite"))))))

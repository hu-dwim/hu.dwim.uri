;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2011 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.uri
  :class hu.dwim.system
  :depends-on (:babel
               :hu.dwim.util
               :split-sequence)
  :components ((:module "source"
                :components ((:file "uri")))))

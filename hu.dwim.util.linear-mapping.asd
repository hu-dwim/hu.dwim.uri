;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2011 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.linear-mapping
  :class hu.dwim.system
  :description ""
  :depends-on (:bordeaux-threads
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "linear-mapping")))))

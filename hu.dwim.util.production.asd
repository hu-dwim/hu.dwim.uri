;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.production
  :class hu.dwim.system
  :description "Various utilities used by the dwim.hu team. Contains code for producing standalone executable services."
  :depends-on (:cl-fad
               :command-line-arguments
               :hu.dwim.util
               :iolib.syscalls)
  :components ((:module "source"
                :components ((:file "production")
                             (:file "command-line")))))
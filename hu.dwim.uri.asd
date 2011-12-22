;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2011 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.uri
  :class hu.dwim.system
  :depends-on (:babel
               :cl-ppcre
               :hu.dwim.util
               :iolib.sockets)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "uri" :depends-on ("package"))))))
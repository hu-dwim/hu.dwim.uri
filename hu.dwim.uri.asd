;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2011 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.uri
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:babel
               :cl-ppcre
               :hu.dwim.util
               :iolib.sockets)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "uri" :depends-on ("package"))))))

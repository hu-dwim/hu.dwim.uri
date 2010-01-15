;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.util.soap
  :class hu.dwim.system
  :description "SOAP messages over HTTP."
  :depends-on (:babel
               :babel-streams
               :cxml
               ;; KLUDGE: TODO: move flexml to hu.dwim.util.flexml
               :cl-l10n
               :drakma
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.quasi-quote.xml
               :hu.dwim.util)
  :components ((:module "source"
                :components ((:file "soap")))))

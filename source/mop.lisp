;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (function ioe) find-slot (class-or-name slot-name &key (otherwise :error otherwise?))
  (or (find slot-name
            (the list
              (class-slots (if (symbolp class-or-name)
                               (find-class class-or-name)
                               class-or-name)))
            :key 'slot-definition-name
            :test 'eq)
      (handle-otherwise (error "Cannot find slot ~S in class ~A" slot-name class-or-name))))

(def (function ioe) find-direct-slot (class-or-name slot-name &key (otherwise :error otherwise?))
  (or (find slot-name
            (the list
              (closer-mop:class-direct-slots (if (symbolp class-or-name)
                                                 (find-class class-or-name)
                                                 class-or-name)))
            :key 'closer-mop:slot-definition-name
            :test 'eq)
      (handle-otherwise (error "Cannot find direct slot ~S in class ~A" slot-name class-or-name))))

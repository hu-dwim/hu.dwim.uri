;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (macro e) define-dynamic-context (name direct-slots &key direct-superclasses
                                       export-symbols (class-name name) chain-parents
                                       (create-struct nil) (create-class (not create-struct))
                                       struct-options (defclass-macro-name 'defclass))
  "The purpose of this macro is to provide an easy way to access a group of related special variables. To do so, it generates with-NAME/in-NAME/current-NAME/has-NAME macros to access either a CLOS instance or a defstruct in a special variable. Optionally it can chain the \"parent\" bindings (use :CHAIN-PARENTS T and access with PARENT-CONTEXT-OF)."
  (assert (and (or create-class
                   create-struct
                   (not (or direct-slots direct-superclasses chain-parents)))
               (or (not create-struct)
                   (not direct-superclasses)))
          () "Invalid combination of DIRECT-SLOTS, DIRECT-SUPERCLASSES, CHAIN-PARENTS and CREATE-CLASS/CREATE-STRUCT.")
  (assert (or (not struct-options) create-struct) () "STRUCT-OPTIONS while no CREATE-STRUCT?")
  (assert (not (and create-class create-struct)) () "Only one of CREATE-CLASS and CREATE-STRUCT is allowed.")
  (bind ((special-var-name (symbolicate "*" name "*"))
         (extractor-name (symbolicate '#:current- name))
         (has-checker-name (symbolicate '#:has- name))
         (in-macro-name (symbolicate '#:in- name))
         (with-new-macro-name (symbolicate '#:with-new- name))
         (with-macro-name (symbolicate '#:with- name))
         (ensure-macro-name (symbolicate '#:ensure- name))
         (struct-constructor-name (when create-struct
                                    (or (second (assoc :constructor struct-options))
                                        (symbolicate '#:make- name))))
         (struct-conc-name (when create-struct
                             (or (second (assoc :conc-name struct-options))
                                 (symbolicate class-name "-")))))
    `(progn
       (defvar ,special-var-name)
       (declaim (inline ,has-checker-name ,extractor-name (setf ,extractor-name)))
       ,(when export-symbols
              `(export (list
                        ',special-var-name
                        ',extractor-name
                        ',has-checker-name
                        ',with-new-macro-name
                        ',with-macro-name
                        ',in-macro-name)))
       ;; generate the context class definition
       ,(when create-class
              `(,defclass-macro-name ,class-name ,direct-superclasses
                 ,(if chain-parents
                      (append `((parent-context nil :accessor parent-context-of)) direct-slots) ; accessor is explicitly given to force it to be interned in this package
                      direct-slots)))
       ,(when create-struct
              `(defstruct (,name ,@struct-options)
                 ,@(if chain-parents
                       (append `((parent-context nil :type (or null ,class-name))) direct-slots)
                       direct-slots)))
       ;; generate the with-new-... macro
       (defmacro ,with-new-macro-name ((&rest initargs) &body forms)
         `(,',with-macro-name ,,(if create-struct
                                    ``(,',struct-constructor-name ,@initargs)
                                    ``(make-instance ',',class-name ,@initargs))
            ,@forms))
       ;; generate the ensure-... macro
       (defmacro ,ensure-macro-name ((&rest initargs) &body forms)
         `(,',with-macro-name (or (and (boundp ',',special-var-name)
                                       ,',special-var-name)
                                  ,,(if create-struct
                                        ``(,',struct-constructor-name ,@initargs)
                                        ``(make-instance ',',class-name ,@initargs)))
            ,@forms))
       ;; generate the with-... macro
       (defmacro ,with-macro-name (context &body forms)
         (bind ((context-instance (gensym "CONTEXT-INSTANCE"))
                (parent (gensym "PARENT")))
           (declare (ignorable parent))
           `(bind ((,context-instance ,context)
                   ,@,(when chain-parents
                            ``((,parent (when (,',has-checker-name)
                                          (,',extractor-name)))))
                   (,',special-var-name ,context-instance))
              (declare (special ,',special-var-name)) ; KLUDGE with-call/cc in needs it currently
              ,@,(when chain-parents
                       ``((setf (,',(if create-struct
                                        (symbolicate struct-conc-name '#:parent-context)
                                        'parent-context-of) ,context-instance)
                                ,parent)))
              (unless ,context-instance
                (error ,',(string+ "Called with nil " (string-downcase name))))
              ,@forms)))
       (defun ,has-checker-name ()
         (boundp ',special-var-name))
       ;; KLUDGE: the rest is obsolete and will be dropped eventually
       (defmacro ,in-macro-name (var-name-or-slot-name-list &body forms)
         (bind ((slots (when (listp var-name-or-slot-name-list)
                         var-name-or-slot-name-list)))
           (if slots
               `(with-slots ,slots (,',extractor-name)
                  ,@forms)
               `(bind ((,var-name-or-slot-name-list (,',extractor-name)))
                  (declare (special ,',special-var-name)) ; KLUDGE with-call/cc in needs it currently
                  ,@forms))))
       ;; generate the current-... function
       (defun ,extractor-name ()
         ,special-var-name)
       (defun (setf ,extractor-name) (value)
         (setf ,special-var-name value)))))

(def (macro e) define-dynamic-context* (name direct-slots &rest args &key (defclass-macro-name 'defclass*) &allow-other-keys)
  (remove-from-plistf args :defclass-macro-name)
  `(define-dynamic-context ,name ,direct-slots
     :defclass-macro-name ,defclass-macro-name
     ,@args))

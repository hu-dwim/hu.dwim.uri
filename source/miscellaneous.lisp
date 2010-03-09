;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Misc

(def (function e) enable-standard-hu.dwim-syntaxes ()
  "This function sets up the common readtable modifications we (http://dwim.hu) use in almost all of our projects. Some projects enable more, but this is available almost everywhere."
  (enable-sharp-boolean-syntax)
  (enable-sharp-comment-syntax)
  (enable-readtime-wrapper-syntax)
  (enable-feature-cond-syntax))

(def (macro e) eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(def (function ioe) eval-interpret (form)
  (bind #*((:sbcl ((sb-ext:*evaluator-mode* :interpret))))
    (eval form)))

(def (constant e) +no-error-status-code+ 0)

(def (function e) quit (status-code)
  ;; (log.info "Quiting production image with status-code ~A" status-code)
  #*((:sbcl (sb-ext:quit :recklessly-p #t :unix-status status-code))
     (t #.(warn "~S is not implemented on your platform" 'quit)
        (not-yet-implemented))))

(def (function e) guess-file-type (pathname)
  ;; TODO: KLUDGE: not portable, etc.
  (bind ((type (pathname-type pathname)))
    (switch (type :test #'string=)
      ("asd" :asd)
      ("lisp" :lisp)
      ("txt" :text)
      ("text" :text)
      (t
       ;; TODO this is a bit too heavy-weight, both on dependencies and runtime implications...
       #*((:sbcl
           (bind ((result (with-output-to-string (output)
                            (sb-ext:run-program "/usr/bin/file" (list (namestring pathname)) :output output))))
             (cond ((search "text" result) :text)
                   (t :binary))))
          (t :binary))))))

(def (macro e) with-keyword-package (&body body)
 `(bind ((*package* #.(find-package "KEYWORD")))
    ,@body))

(def (function e) fully-qualified-symbol-name (symbol &key separator)
  (bind ((symbol-name (symbol-name symbol))
         (package (symbol-package symbol))
         (keyword-package (load-time-value (find-package "KEYWORD"))))
    (string+ (unless (eq package keyword-package)
               (package-name package))
             (or separator
                 (if (or (eq package keyword-package)
                         (eq (nth-value 1 (find-symbol symbol-name package)) :external))
                     ":"
                     "::"))
             symbol-name)))

(def (function e) find-fully-qualified-symbol (name)
  (declare (type string name))
  (bind ((position (position #\: name)))
    (when position
      (bind ((package-name (subseq name 0 position))
             (symbol-name (subseq name (or (position #\: name :start (1+ position) :test-not #'char=)
                                           (1+ position)))))
        (if (string= package-name "#")
            (make-symbol symbol-name)
            (awhen (find-package package-name)
              (find-symbol symbol-name it)))))))

(def (definer e :available-flags "ioed") macro/multiple-arguments-variant (singular-macro-name)
  (bind ((plural (intern (format nil "~aS" singular-macro-name))))
    `(def (macro ,@-options-) ,plural (bindings &body body)
       ,(format nil "Multiple binding version of ~(~a~)." singular-macro-name)
       (if bindings
           `(,',singular-macro-name ,(car bindings)
                                    (,',plural ,(cdr bindings)
                                               ,@body))
           `(progn ,@body)))))

(def (with-macro* e) with-profiling ()
  #*((:sbcl
      #.(progn (require :sb-sprof) nil)
      (load-time-value (require :sb-sprof))
      (sb-sprof:with-profiling ()
        (-body-)))
     (t #.(warn "~S is not implemented for your platform, no profiling information will be available." 'with-profiling)
        (-body-))))

(def (function e) if-symbol-exists (package name)
  "Can be used like this: #+#.(hu.dwim.util:if-symbol-exists \"SB-C\" \"SAVE-FP-AND-PC-AROUND-ALIEN-CALL\") to only read the next form when the given symbol exists in the given package."
  (if (and (find-package (string package))
           (find-symbol (string name) (string package)))
      '(:and)
      '(:or)))

(def (macro e) surround-body-when (test surround-with &body body)
  `(surround-body-when* (,test ,surround-with)
     ,@body))

(def (macro e) surround-body-when* ((test surround-with &key (body-name '-body-)) &body body)
  (cond
    ((eq test t)
     `(macrolet ((,body-name ()
                   `(progn
                      ,',@body)))
        (,@surround-with)))
    ((null test)
     `(progn
        ,@body))
    (t `(flet ((,body-name ()
                 ,@body))
          (declare (dynamic-extent #',body-name))
          (if ,test
              (,@surround-with)
              (,body-name))))))

(def (macro e) surround-body-unless (test surround-with &body body)
  `(surround-body-when* ((not ,test) ,surround-with)
     ,@body))

(def (macro e) surround-body-unless* ((test surround-with &key (body-name '-body-)) &body body)
  `(surround-body-when* ((not ,test) ,surround-with :body-name ,body-name)
     ,@body))

;;;;;;
;;; Binding related

(def (macro e) rebind (bindings &body body)
  `(let ,(loop :for symbol-name :in bindings
               :collect (list symbol-name symbol-name))
     ,@body))

;;;;;;
;;; Development marks

(def (function e) not-yet-implemented (&optional (datum "Not yet implemented." datum?) &rest args)
  (when datum?
    (setf datum (concatenate 'string "Not yet implemented: " datum)))
  (apply #'cerror "Ignore and continue" datum args))

(def function not-yet-implemented/crucial-api (name)
  (warn "~S is not implemented on your platform! This may lead to runtime errors later..." name)
  `(error "~S is not implemented on your platform, sorry..." ',name))

(def (function e) operation-not-supported (&optional (datum "Operation not supported." datum?) &rest args)
  (when datum?
    (setf datum (concatenate 'string "Operation not supported: " datum)))
  (apply #'error datum args))

;;;;;;
;;; Thread name

(def (macro e) with-thread-name (name &body body)
  (declare (ignorable name))
  #*((:sbcl
      (with-unique-names (thread previous-name)
        `(let* ((,thread sb-thread:*current-thread*)
                (,previous-name (sb-thread:thread-name ,thread)))
           (setf (sb-thread:thread-name ,thread)
                 (concatenate 'string ,previous-name ,name))
           (unwind-protect
                (progn
                  ,@body)
             (setf (sb-thread:thread-name ,thread) ,previous-name)))))
     (t
      `(progn
         ,@body))))

(def (macro e) with-thread-activity-description ((name) &body body)
  "This is a debugging helper tool. Information provided here may show up in backtraces unless compiled without the debug helpers."
  `(with-thread-name ,(if (stringp name) ;; TODO this should be a compiler-macro in sbcl itself
                          (concatenate 'string " / " name)
                          `(concatenate 'string " / " (string ,name)))
     ,@body))

;;;;;;
;;; Otherwise

(def (function ioe) handle-otherwise/value (otherwise &key default-message)
  (setf default-message (ensure-list default-message))
  (case otherwise
    (:error  (apply #'error (or default-message (list "Otherwise assertion failed"))))
    (:cerror (apply #'cerror "Continue" (or default-message (list "Otherwise assertion failed"))))
    (:warn   (apply #'warn (or default-message (list "Otherwise assertion failed"))))
    (t (cond
         ((and (consp otherwise)
               (member (first otherwise) '(:error error :cerror cerror :warn warn) :test #'eq))
          (assert (not (null (rest otherwise))))
          (ecase (first otherwise)
            ((error :error)   (apply #'error  (rest otherwise)))
            ((cerror :cerror) (apply #'cerror (list* "Continue by returning (VALUES)" (rest otherwise))))
            ((warn :warn)     (apply #'warn   (rest otherwise))))
          (values))
         ((functionp otherwise)
          (funcall otherwise))
         (t
          otherwise)))))

(def (macro e) handle-otherwise (&body default-error-forms)
  `(handle-otherwise* ()
     ,@default-error-forms))

(def (macro e) handle-otherwise* ((&key default-message) &body default-forms)
  ;; we assume two lexically visible local variables at the call site...
  `(if otherwise?
       (handle-otherwise/value otherwise :default-message ,default-message)
       (progn
         ,@default-forms)))

(def (function e) quoted-form? (thing)
  (and (consp thing)
       (eq (car thing) 'quote)
       (progn
         (assert (length= 2 thing) () "Illegal QUOTE form ~S" thing)
         t)))

(def (function e) quoted-symbol? (thing)
  (and (quoted-form? thing)
       (not (null (second thing)))
       (symbolp (second thing))))

(def (function e) tree-substitute (new old list &key from-end (test #'eql) (test-not nil) (end nil) (count nil) (key nil) (start 0))
  "Starting from LIST non-destructively replaces OLD with NEW."
  (if (consp list)
      (bind ((result (iter (for newitem in (ensure-list new))
                           (for olditem in (ensure-list old))
                           (setf list (substitute newitem olditem list :from-end from-end :test test :test-not test-not
                                                  :end end :count count :key key :start start))
                           (finally (return list)))))
        (iter (for node first result then (cdr node))
              (until (null node))
              (for el = (car node))
              (setf (car node) (tree-substitute new old el :from-end from-end :test test :test-not test-not
                                                :end end :count count :key key :start start)))
        result)
      (if (funcall test list old)
          new
          list)))

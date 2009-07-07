;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (special-variable e) *debug-on-error* #f
  "The default, system wide, value for debug-on-error.")

(def special-variable *debug-context*)

(def (generic e) debug-on-error? (context error)
  (:method (context error)
    *debug-on-error*))

(def (generic e) handle-toplevel-error (context error)
  (:method :before (context (condition serious-condition))
    (maybe-invoke-debugger condition))
  (:method :around (context error)
    (with-thread-activity-description ("HANDLE-TOPLEVEL-ERROR")
      (bind ((*debug-context* context))
        (call-next-method)))))

(def (with-macro* e) with-layered-error-handlers (debug-context level-1-error-handler abort-unit-of-work-callback
                                                                &rest args &key
                                                                (log-to-debug-io #t)
                                                                (ignore-condition-callback (constantly #f))
                                                                (level-2-error-handler (if log-to-debug-io
                                                                                           (lambda (error &key message &allow-other-keys)
                                                                                             (format *debug-io* "~A~%" (build-backtrace-string error :message message))
                                                                                             (maybe-invoke-debugger error))
                                                                                           (lambda (error &key &allow-other-keys)
                                                                                             (maybe-invoke-debugger error))))
                                                                (giving-up-callback (if log-to-debug-io
                                                                                        (lambda (&key reason &allow-other-keys)
                                                                                          (format *debug-io* "WITH-LAYERED-ERROR-HANDLERS is giving up due to: ~A~%" reason))
                                                                                        (constantly nil)))
                                                                (out-of-storage-callback (if log-to-debug-io
                                                                                             (lambda (error &key &allow-other-keys)
                                                                                               (declare (ignore error))
                                                                                               (format *debug-io* "WITH-LAYERED-ERROR-HANDLERS is bailing out due to a STORAGE-CONDITION...~%"))
                                                                                             (constantly nil)))
                                                                &allow-other-keys)
  (remove-from-plistf args :ignore-condition-callback :out-of-storage-callback)
  (bind ((level-1-error nil)
         (*debug-context* debug-context))
    (labels ((ignore-error? (error)
               (apply ignore-condition-callback error args))
             (abort-unit-of-work (reason)
               (apply abort-unit-of-work-callback reason args))
             (handle-level-1-error (error)
               ;; first level of error handling, call around participants, give them a chance to render an error page, etc
               (setf level-1-error error)
               (handler-bind ((serious-condition #'handle-level-2-error))
                 (with-thread-activity-description ("HANDLE-LEVEL-1-ERROR")
                   (cond
                     ((typep error 'storage-condition)
                      ;; on SBCL it includes control stack exhaustion, too
                      (apply out-of-storage-callback error args))
                     ((ignore-error? error)
                      nil)
                     (t
                      (funcall level-1-error-handler error)))
                   (abort-unit-of-work "Level 1 error handler returned normally")
                   (error "This code path must not be reached in the level 1 error handler of WITH-LAYERED-ERROR-HANDLERS"))))
             (handle-level-2-error (error)
               ;; second level of error handling quarding against errors while handling the original error
               (handler-bind ((serious-condition #'handle-level-3-error))
                 (with-thread-activity-description ("HANDLE-LEVEL-2-ERROR")
                   (unless (ignore-error? error)
                     ;; reason: when e.g. an error html page is being sent the client socket may get reset
                     (funcall level-2-error-handler error :message (list "Nested error while handling original error: ~A; the nested error is: ~A" level-1-error error)))
                   (abort-unit-of-work error)
                   (error "This code path must not be reached in the level 2 error handler of WITH-LAYERED-ERROR-HANDLERS"))))
             (handle-level-3-error (error)
               ;; if we get here then do as little as feasible wrapped in ignore-errors to bail out and abort processing
               ;; the request as soon as we can.
               (with-thread-activity-description ("HANDLE-LEVEL-3-ERROR")
                 (bind ((error-message (or (ignore-errors
                                             (format nil "Nested error while handling original error: ~A; the second nested error is: ~A"
                                                     level-1-error error))
                                           (ignore-errors
                                             (format nil "Failed to log nested error message (nested print errors?). Condition type of the third nested error is ~S."
                                                     (type-of error)))
                                           "Completely failed to log error, giving up. It's probably due to some nested printer errors or the the whole VM is dying...")))
                   (ignore-errors
                     (apply giving-up-callback :reason error-message args))
                   (abort-unit-of-work error)
                   (error "This code path must not be reached in the level 3 error handler of WITH-LAYERED-ERROR-HANDLERS")))))
      (handler-bind
          ((serious-condition #'handle-level-1-error))
        (-body-)))))

(def (function e) maybe-invoke-debugger (condition)
  (when (debug-on-error? *debug-context* condition)
    (when (fboundp 'invoke-slime-debugger)
      (restart-case
          (funcall 'invoke-slime-debugger condition)
        (continue ()
          :report "Continue processing the error as if the debugger was not available"))))
  (values))


;;;;;;
;;; Error log decorators

(def special-variable *error-log-decorators* ())

(def (definer e :available-flags "eiod") error-log-decorator (name &body body)
  `(def function ,name ()
     ,@body))

(def (macro e) make-error-log-decorator (&body body)
  `(lambda ()
     ,@body))

(def (macro e) make-special-variable-printing-error-log-decorator (&rest variables)
  `(lambda ()
     (bind ((*package* (find-package "COMMON-LISP"))
            (*print-right-margin* 150))
       ,@(iter (for variable :in variables)
               (collect `(format t ,(bind ((*package* (find-package "COMMON-LISP")))
                                      (format nil "~~%~S: ~~A" variable))
                                 (if (boundp ',variable)
                                     ,variable
                                     'unbound)))))))

(def (macro e) with-error-log-decorator (decorator &body body)
  `(bind ((*error-log-decorators* (cons ,(if (symbolp decorator)
                                             `(quote ,decorator)
                                             decorator)
                                        *error-log-decorators*)))
     ,@body))


;;;;;;
;;; Backtrace extraction

(def class* stack-frame ()
  ((description)
   (local-variables)
   (source-location)))

(def function make-stack-frame (description &optional local-variables source-location)
  (make-instance 'stack-frame
                 :description description
                 :local-variables local-variables
                 :source-location source-location))

(def with-macro with-backtrace-bindings ()
  (bind ((bindings `((*print-pretty*           . #t)
                     (*print-level*            . 3)
                     (*print-length*           . 100)
                     (*print-circle*           . #t)
                     (*print-readably*         . #f)
                     (*print-pprint-dispatch*  . ,(copy-pprint-dispatch nil))
                     (*print-gensym*           . #t)
                     (*print-base*             . 10)
                     (*print-radix*            . #f)
                     (*print-array*            . #t)
                     (*print-lines*            . nil)
                     (*print-escape*           . #t)
                     (*print-right-margin*     . ,most-positive-fixnum)))
         (variables (mapcar #'car bindings))
         (values (mapcar #'cdr bindings)))
    (progv variables values
      (-body-))))

(def function current-thread-name-if-available ()
  (awhen (find-package "BORDEAUX-THREADS")
    (funcall (find-symbol "THREAD-NAME" it) (funcall (find-symbol "CURRENT-THREAD" it)))))

(def (function e) build-backtrace-string (error &key message)
  "Message may also be a list, in which case FORMAT is applied on it."
  (with-backtrace-bindings
    (block building
      (handler-bind ((serious-condition
                      (lambda (nested-error)
                        (handler-bind ((serious-condition
                                        (lambda (nested-error2)
                                          (declare (ignore nested-error2))
                                          (return-from building "Failed to log backtrace due to multiple nested errors. Giving up..."))))
                          (return-from building (format nil "Failed to log backtrace due to: ~A. The orignal error is: ~A" nested-error error))))))
        (with-output-to-string (*standard-output*)
          (format t "~%*** At: ~A" (aif (find-package "LOCAL-TIME")
                                        (funcall (find-symbol "FORMAT-RFC3339-TIMESTRING" it) nil (funcall (find-symbol "NOW" it)))
                                        (get-universal-time)))
          (when message
            (format t "~%*** Message:~%")
            (apply #'format t (ensure-list message)))
          (awhen (current-thread-name-if-available)
            (format t "~%*** In thread: ~A" it))
          (format t "~%*** Error:~%~A~%*** Backtrace:~%" error)
          (if (fboundp 'collect-backtrace)
              (bind ((backtrace (funcall 'collect-backtrace))
                     (*print-pretty* #f))
                (iter (for stack-frame :in backtrace)
                      (for index :upfrom 0)
                      (write-string stack-frame)
                      (terpri)))
              (format t "[Backtrace is not available, see COLLECT-BACKTRACE.]"))
          (when *error-log-decorators*
            (format t "~%*** Backtrace decorators:")
            (dolist (decorator *error-log-decorators*)
              (when (symbolp decorator)
                (bind ((*package* (find-package :keyword)))
                  (format t "~%~S:" decorator)))
              (funcall decorator)))
          (format t "~%*** End of error details"))))))


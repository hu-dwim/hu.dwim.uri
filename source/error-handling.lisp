;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def (special-variable e) *debug-on-error* #f
  "The default, system wide, value for debug-on-error.")

(def (generic e) debug-on-error? (context error)
  (:method (context error)
    *debug-on-error*))

(def (function e) print-error-safely (&optional message &rest args)
  (when message
    (bind ((formatted (or (ignore-errors
                            (apply #'format nil message args))
                          (ignore-errors
                            (format nil "Error while formatting error message.~%  Format control: ~A~%  Argument types: ~A" message (mapcar #'type-of args)))
                          "Err, complete meltdown in BEST-EFFORT-LOG-ERROR. Sorry, no more clue is available...")))
      (ignore-errors
        (write-string formatted *error-output*)))))

;; TODO rename? to what? with-invoke-debugger-hook?
(def with-macro with-debugger-hook-for-break (hook)
  "CL:BREAK is specified to ignore CL:*DEBUGGER-HOOK*, so we need a platform dependent way to hook the debugger to be able to catch it."
  #*((:sbcl (bind ((sb-ext:*invoke-debugger-hook* hook))
              (-body-)))
     (t #.(warn "~S is not implemented for your platform. This may interfere with the behavior of CL:BREAK while the debugger is disabled..." 'with-debugger-hook-for-break)
        (-body-))))

(def (with-macro* e) with-layered-error-handlers
    (level-1-error-handler abort-unit-of-work-callback
                           &rest args &key
                           (log-to-debug-io #t)
                           (ignore-condition-callback (constantly #f))
                           (level-2-error-handler (if log-to-debug-io
                                                      (lambda (error &key message &allow-other-keys)
                                                        (format *debug-io* "~A~%" (build-backtrace-string error :message message :timestamp (get-universal-time)))
                                                        (maybe-invoke-debugger error))
                                                      (lambda (error &key &allow-other-keys)
                                                        (maybe-invoke-debugger error))))
                           (giving-up-callback (if log-to-debug-io
                                                   (lambda (&key reason &allow-other-keys)
                                                     (format *debug-io* "WITH-LAYERED-ERROR-HANDLERS is giving up due to: ~A~%" reason))
                                                   (constantly nil)))
                           (out-of-storage-callback (if log-to-debug-io
                                                        (lambda (error &key &allow-other-keys)
                                                          ;; TODO if/when sbcl becomes more failure tolerant with stack overflows, we could try to log this using the logger infrastructure. until then, print something to *debug-io* and bail out...
                                                          (format *debug-io* "WITH-LAYERED-ERROR-HANDLERS is bailing out due to a STORAGE-CONDITION of type ~S~%" (type-of error)))
                                                        (constantly nil)))
                           &allow-other-keys)
  (remove-from-plistf args :log-to-debug-io :ignore-condition-callback :level-2-error-handler :giving-up-callback :out-of-storage-callback)
  (bind ((level-1-error nil))
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
                   (abort-unit-of-work "Level 1 error handler finished normally")
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
        (flet ((with-layered-error-handlers/debugger-hook (condition hook)
                 ;; this is only here because (break) ignores the *debugger-hook* variables, so it needs platform dependent care...
                 (declare (ignore hook))
                 (when log-to-debug-io
                   (format *debug-io* "~&WITH-LAYERED-ERROR-HANDLERS/DEBUGGER-HOOK is invoked, most probably because of CL:BREAK (if not, then that's a big WTF?!)~%"))
                 (maybe-invoke-debugger condition)))
          (with-debugger-hook-for-break #'with-layered-error-handlers/debugger-hook
            (-body-)))))))

(def (function e) maybe-invoke-debugger (condition &key context)
  (when (debug-on-error? context condition)
    (when (fboundp 'invoke-slime-debugger)
      (restart-case
          (funcall 'invoke-slime-debugger condition)
        (continue-error-handling ()
          :report "Continue processing the error as if the debugger was not available"))))
  (values))

(def function disabled-debugger-hook (condition &optional logger)
  (bind ((message (or (ignore-errors
                        (build-backtrace-string condition :message "Unhandled error while debugger is disabled, quitting..." :timestamp (get-universal-time)))
                      "Err, complete meltdown in DISABLED-DEBUGGER-HOOK. Sorry, no more clue is available...")))
    (when message
      (ignore-errors
        (write-string message *error-output*)
        (terpri *error-output*))
      (when logger
        (hu.dwim.logger:handle-log-message logger hu.dwim.logger:+fatal+ message nil))))
  (quit 42))

(def (function e) disable-debugger (&optional logger)
  (declare (ignorable logger))
  #*((:sbcl (flet ((call-disabled-debugger-hook (condition hook)
                     (declare (ignore hook))
                     (disabled-debugger-hook condition logger)))
              (sb-ext:disable-debugger) ; so that we unconditionally disable LDB
              (setf sb-ext:*invoke-debugger-hook* #'call-disabled-debugger-hook)
              (setf *debugger-hook* #'call-disabled-debugger-hook)))
     (t #.(warn "~S is not fully implemented for your implementation which may lead to undesired consequences" 'disable-debugger)))
  (format *debug-io* "Disabled debugger~%"))


;;;;;;
;;; Error log decorators

(def (special-variable :documentation "List of decorators that will be invoked when an error backtrace is logged. A decorator is a function thunk that will be funcall'd and can print to *STANDARD-OUTPUT* (caret will not be on a new line when called).")
    *error-log-decorators* ())

(def (definer e :available-flags "eiod") error-log-decorator (name &body body)
  `(def function ,name ()
     ,@body))

(def (macro e) make-error-log-decorator (&body body)
  `(named-lambda make-error-log-decorator/body ()
     ,@body))

(def (macro e) make-special-variable-printing-error-log-decorator (&rest variables)
  `(named-lambda make-special-variable-printing-error-log-decorator/body ()
     (bind ((*package* (find-package "COMMON-LISP"))
            (*print-right-margin* 150))
       ,@(iter (for variable :in variables)
               (collect `(format t ,(bind ((*package* (find-package "COMMON-LISP")))
                                      (format nil "~~%~S ~~A" variable))
                                 (if (boundp ',variable)
                                     ,variable
                                     'unbound)))))))

(def (macro e) with-error-log-decorator (decorator &body body)
  `(bind ((*error-log-decorators* (cons ,decorator *error-log-decorators*)))
     ,@body))

(def (macro/multiple-arguments-variant e) with-error-log-decorator)

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
  ;; TODO err... it's kinda...
  (awhen (find-package "BORDEAUX-THREADS")
    (funcall (find-symbol "THREAD-NAME" it) (funcall (find-symbol "CURRENT-THREAD" it)))))

(def (function e) build-backtrace-string (error &key message timestamp)
  "Message may also be a list, in which case FORMAT is applied on it."
  (with-backtrace-bindings
    (block building
      (with-layered-error-handlers ((lambda (nested-error)
                                      (return-from building (format nil "Failed to build backtrace due to: ~A. The orignal error was: ~A" nested-error error)))
                                    (lambda (reason args)
                                      (declare (ignore reason args))
                                      (error "This should be impossible to reach in ~S" 'build-backtrace-string))
                                    :level-2-error-handler (lambda (nested-error2)
                                                             (declare (ignore nested-error2))
                                                             (return-from building "Failed to build backtrace due to multiple nested errors. Giving up...")))
        (with-output-to-string (*standard-output*)
          (when timestamp
            (format t "~%*** At: ~A" timestamp))
          (when message
            (format t "~&*** Message:~%")
            (apply #'format t (ensure-list message)))
          (awhen (current-thread-name-if-available)
            (format t "~&*** In thread: ~A" it))
          (format t "~&*** Error of type ~S:~%~A~&*** Backtrace:~%" (type-of error) error)
          (if (fboundp 'collect-backtrace)
              (bind ((backtrace (funcall 'collect-backtrace :start 6))
                     (*print-pretty* #f))
                (iter (for stack-frame :in backtrace)
                      (for index :upfrom 0)
                      (write-string stack-frame)
                      (terpri)))
              (format t "[Backtrace is not available, see COLLECT-BACKTRACE.]"))
          (when *error-log-decorators*
            (format t "~&*** Backtrace decorators:")
            (dolist (decorator *error-log-decorators*)
              (when (symbolp decorator)
                (bind ((*package* (find-package :keyword)))
                  (format t "~&~S:" decorator)))
              (funcall decorator)))
          (format t "~&*** End of error details"))))))

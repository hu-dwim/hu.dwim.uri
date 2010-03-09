;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Production support

(def (function e) ensure-utf-8-external-format ()
  #+sbcl
  (unless (eq (sb-impl::default-external-format) :utf-8)
    (cerror "Ignore" "The default external format is ~S, but UTF-8 is strongly advised! Check your $LANG env variable..."
            (sb-impl::default-external-format))))

(def (function e) load-and-eval-config-file (system-name)
  (bind ((pathname (merge-pathnames (string+ (string-downcase system-name) ".lisp") "config/"))
         (config-file-name (system-relative-pathname system-name pathname)))
    (if (cl-fad:file-exists-p config-file-name)
        (with-local-readtable
          (bind ((*package* (find-package :hu.dwim.common)))
            ;; load using the special in-package symbol that sets up the readtable based on what the package says
            (load config-file-name))
          config-file-name)
        nil)))

(def (with-macro* e) with-pid-file (pathname)
  (check-type pathname (or pathname string))
  (bind ((pid-file-has-been-created? #f))
    (unwind-protect
         (progn
           (format *debug-io* "Writing pid file ~S~%" pathname)
           (when (cl-fad:file-exists-p pathname)
             (bind ((pid (parse-integer (read-file-into-string pathname))))
               (if (ignore-errors
                     (isys:%sys-kill pid 0)
                     #t)
                   (error "PID file ~S already exists and points to a running process ~S" pathname pid)
                   (progn
                     (format *debug-io* "Deleting stale PID file ~S pointing to non-existent PID ~S~%" pathname pid)
                     (delete-file pathname)))))
           (with-open-file (pid-stream pathname :direction :output
                                       :element-type 'character
                                       :if-exists :error)
             (princ (isys:%sys-getpid) pid-stream))
           (setf pid-file-has-been-created? #t)
           (format *debug-io* "PID file is ~S, PID is ~A~%" pathname (isys:%sys-getpid))
           (-with-macro/body-))
      (when pid-file-has-been-created?
        (unless (ignore-errors
                  (delete-file pathname)
                  #t)
          (print-error-safely "Failed to remove pid file ~S~%" pathname))))))

(def (with-macro e) with-save-core-and-die-restart ()
  (restart-case
      (-body-)
    #+sbcl
    (save-core-and-die ()
      :report "Save image to /tmp/sbcl.core and die"
      (mapcar
       (lambda (thread)
         (unless (eq thread sb-thread:*current-thread*)
           (sb-thread:terminate-thread thread)))
       (sb-thread:list-all-threads))
      (sb-ext:save-lisp-and-die "/tmp/sbcl.core"))))

(def (with-macro e) with-standard-toplevel-restarts ()
  (restart-case
      (with-save-core-and-die-restart
        (-body-))
    (abort nil
      :report (lambda (stream)
                (format stream "Give up starting the image and quit the VM process with exit code 2"))
      (quit 2))))

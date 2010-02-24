;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

(def special-variable *temporary-file-random-state* (make-random-state t))
(def global-variable *temporary-file-unique-counter* (make-atomic-counter))

(def special-variable *directory-for-temporary-files* nil
  "Holds the runtime value of the temporary directory, which includes the PID of the process.")

(def (function e) directory-for-temporary-files ()
  (or *directory-for-temporary-files*
      (setf *directory-for-temporary-files*
            (ensure-directories-exist
             (string+ (iolib.pathnames:file-path-namestring iolib.os:*temporary-directory*)
                      "/hu.dwim-"
                      (integer-to-string (isys:%sys-getpid))
                      "/")))))

(def (function e) delete-directory-for-temporary-files ()
  (when *directory-for-temporary-files*
    (iolib.os:delete-files *directory-for-temporary-files* :recursive #t)
    (setf *directory-for-temporary-files* nil)))

(def (function e) filename-for-temporary-file (&optional name-prefix)
  (string+ (directory-for-temporary-files)
           name-prefix
           (when name-prefix
             "-")
           (integer-to-string (atomic-counter/increment *temporary-file-unique-counter*))
           "-"
           (integer-to-string (random 100000 *temporary-file-random-state*))))

(def (function e) shadow-temporary-filename (root-directory relative-path temp-subdirectory-name)
  "Returns a filename 'relocated' to the temp directory under TEMP-SUBDIRECTORY-NAME."
  (merge-pathnames relative-path (make-pathname :directory (append (pathname-directory (directory-for-temporary-files))
                                                                   (list temp-subdirectory-name)
                                                                   (rest (pathname-directory root-directory)))
                                                :defaults root-directory)))

(def (function e) open-temporary-file (&rest args &key
                                             (element-type '(unsigned-byte 8))
                                             (direction :output)
                                             name-prefix)
  (remove-from-plistf args :name-prefix)
  (iter
    (for file-name = (filename-for-temporary-file name-prefix))
    (for file = (apply #'open
                       file-name
                       :if-exists nil
                       :direction direction
                       :element-type element-type
                       args))
    (until file)
    (finally (return (values file file-name)))))

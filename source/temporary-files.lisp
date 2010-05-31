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

(def constant +temporary-directory-name-prefix+ "hu.dwim-")

(def (function e) directory-name-for-temporary-files (&key (pid (isys:getpid)))
  ;; NOTE: unexpressed abstraction: this file name structure is assumed in CLEANUP-TEMPORARY-DIRECTORIES
  (string+ (iolib.pathnames:file-path-namestring iolib.os:*temporary-directory*)
           "/"
           +temporary-directory-name-prefix+
           (integer-to-string pid)
           "/"))

(def (function e) directory-for-temporary-files ()
  (or *directory-for-temporary-files*
      (setf *directory-for-temporary-files*
            (ensure-directories-exist (directory-name-for-temporary-files)))))

(def (function e) delete-directory-for-temporary-files ()
  (when *directory-for-temporary-files*
    (iolib.os:delete-files *directory-for-temporary-files* :recursive #t)
    (setf *directory-for-temporary-files* nil))
  (values))

(def (function e) cleanup-temporary-directories ()
  "Tries to delete all temporary directories that have been created by this library by a process not running anymore."
  (bind ((deleted ()))
    (flet ((temporary-directory-of-dead-process? (pathname kind)
             (bind ((name (iolib.pathnames:file-path-namestring pathname)))
               (when (and (eq kind :directory)
                          (starts-with-subseq +temporary-directory-name-prefix+ name))
                 (bind (((:values pid position) (ignore-errors (parse-integer name :start (length +temporary-directory-name-prefix+)))))
                   (and pid
                        (= position (length name))
                        (not (posix-process-exists? pid)))))))
           (delete-temporary-directory (pathname kind parent depth)
             (declare (ignore kind parent depth))
             (ignore-errors
               ;; we may be lacking the permission, etc...
               (iolib.os:delete-files pathname :recursive #t)
               (push pathname deleted))))
      (iolib.os:walk-directory iolib.os:*temporary-directory*
                               #'delete-temporary-directory
                               :maxdepth 1
                               :test #'temporary-directory-of-dead-process?))
    deleted))

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

(def (function e) substitute-illegal-characters-in-file-name (name &key (replacement #\_))
  (substitute-all "/?*\"" replacement name))

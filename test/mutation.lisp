;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.uri.test)

(def suite* (test/uri/mutation :in test))

(def test test/uri/mutation/path ()
  (is (string= (print-uri-to-string (append-path (parse-uri "http://example.com/foo") "/bar/baz/"))
               "http://example.com/foo/bar/baz"))
  (is (string= (print-uri-to-string (prepend-path (parse-uri "http://example.com/foo/") "/bar/baz/"))
               "http://example.com/bar/baz/foo/")))

(def test test/uri/mutation/query ()
  (bind ((uri-string "http://example.com/foo?arg1=val1&arg2=val2&arg3=val3"))
    (is (string= (print-uri-to-string (delete-query-parameters (parse-uri uri-string) "arg1"))
                 "http://example.com/foo?arg2=val2&arg3=val3"))
    (is (string= (print-uri-to-string (delete-query-parameters (parse-uri uri-string) "arg1" "arg3"))
                 "http://example.com/foo?arg2=val2"))
    (is (string= (print-uri-to-string (delete-all-query-parameters (parse-uri uri-string)))
                 "http://example.com/foo")))
  (bind ((uri (parse-uri "http://example.com/foo?arg1=val1&arg2=val2")))
    (is (string= (query-parameter-value uri "arg2") "val2"))
    (setf (query-parameter-value uri "arg2") "val41")
    (setf (query-parameter-value uri "arg2") "val42")
    (setf (query-parameter-value uri "newsetf") "newsetfval")
    (add-query-parameter uri "newadd" "newaddval")
    (is (string= (print-uri-to-string uri)
                 "http://example.com/foo?newsetf=newsetfval&arg1=val1&arg2=val42&newadd=newaddval"))))

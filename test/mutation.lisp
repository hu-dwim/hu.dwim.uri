;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.uri.test)

(def suite* (test/uri/mutation :in test))

(def test test/uri/mutation/path ()
  (is (string= (uri/print-to-string (uri/append-path (parse-uri "http://example.com/foo") "/bar/baz/"))
               "http://example.com/foo/bar/baz"))
  (is (string= (uri/print-to-string (uri/prepend-path (parse-uri "http://example.com/foo/") "/bar/baz/"))
               "http://example.com/bar/baz/foo")))

(def test test/uri/mutation/query ()
  (bind ((uri-string "http://example.com/foo?arg1=val1&arg2=val2&arg3=val3"))
    (is (string= (uri/print-to-string (uri/delete-query-parameters (parse-uri uri-string) "arg1"))
                 "http://example.com/foo?arg2=val2&arg3=val3"))
    (is (string= (uri/print-to-string (uri/delete-query-parameters (parse-uri uri-string) "arg1" "arg3"))
                 "http://example.com/foo?arg2=val2"))
    (is (string= (uri/print-to-string (uri/delete-all-query-parameters (parse-uri uri-string)))
                 "http://example.com/foo")))
  (bind ((uri (parse-uri "http://example.com/foo?arg1=val1&arg2=val2")))
    (is (string= (uri/query-parameter-value uri "arg2") "val2"))
    (setf (uri/query-parameter-value uri "arg2") "val41")
    (setf (uri/query-parameter-value uri "arg2") "val42")
    (setf (uri/query-parameter-value uri "newsetf") "newsetfval")
    (uri/add-query-parameter uri "newadd" "newaddval")
    (is (string= (uri/print-to-string uri)
                 "http://example.com/foo?newsetf=newsetfval&arg1=val1&arg2=val42&newadd=newaddval"))))

;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.uri.test)

(def suite* (test/uri :in test))

(def test parse-and-compare (uri-string &key scheme host port path fragment)
  (bind ((uri (finishes (parse-uri uri-string))))
    (is (equalp scheme (scheme-of uri)))
    (is (equalp host (host-of uri)))
    (is (equalp port (port-of uri)))
    (is (equalp path (path-of uri)))
    (is (equalp fragment (fragment-of uri)))
    uri))

(def test print-and-compare (expected-uri-string &key scheme host port path fragment)
  (is (string= expected-uri-string
               (finishes (print-uri-to-string (make-uri :scheme scheme :host host :port port :path path :fragment fragment))))))

(def test test/uri/simple ()
  (parse-and-compare "foo/bar"
                     :path '("foo" "bar"))
  (parse-and-compare "//example.com/"
                     :host "example.com")
  (parse-and-compare "http://example.com/"
                     :scheme "http" :host "example.com" :path '())
  (parse-and-compare "http://example.com:8080/foo/bar/?arg1=value1&arg2=value2"
                     :scheme "http" :host "example.com" :port 8080 :path '("foo" "bar")))

(def test test/uri/printing ()
  (print-and-compare "http:/foo/bar"
                     :scheme "http" :path '("foo" "bar"))
  (print-and-compare "http://example.com:42/foo/bar#asdf"
                     :scheme "http" :host "example.com" :port 42 :path '("foo" "bar") :fragment "asdf"))

(def test test/uri/idna ()
  (flet ((entry (uri-string &rest components)
           (is (string= (print-uri-to-string (apply 'parse-and-compare uri-string components))
                        uri-string))))
    (entry "http://r%C3%A4ksm%C3%B6rg%C3%A5s.josefsson.org"
           :scheme "http" :host "räksmörgås.josefsson.org")
    (entry "http://%E7%B4%8D%E8%B1%86.w3.mag.keio.ac.jp"
           :scheme "http" :host "納豆.w3.mag.keio.ac.jp")
    (entry "http://www.%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.w3.mag.keio.ac.jp/"
           :scheme "http" :host "www.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.w3.mag.keio.ac.jp")
    (entry "http://%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.%E3%81%BB%E3%82%93%E3%81%A8%E3%81%86%E3%81%AB%E3%81%AA%E3%81%8C%E3%81%84%E3%82%8F%E3%81%91%E3%81%AE%E3%82%8F%E3%81%8B%E3%82%89%E3%81%AA%E3%81%84%E3%81%A9%E3%82%81%E3%81%84%E3%82%93%E3%82%81%E3%81%84%E3%81%AE%E3%82%89%E3%81%B9%E3%82%8B%E3%81%BE%E3%81%A0%E3%81%AA%E3%81%8C%E3%81%8F%E3%81%97%E3%81%AA%E3%81%84%E3%81%A8%E3%81%9F%E3%82%8A%E3%81%AA%E3%81%84.w3.mag.keio.ac.jp/"
           :scheme "http" :host "ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.w3.mag.keio.ac.jp")))

(def test test/uri/escaping ()
  (parse-and-compare "http://www.w%33.org"
                     :scheme "http" :host "www.w3.org")
  (parse-and-compare "//ex%3Aample.com/foo/bara%2Fb%2Fc"
                     :host "ex:ample.com" :path '("foo" "bara/b/c")))

(def test illegal-uri (uri-string)
  (signals error (parse-uri uri-string)))

(def test test/uri/illegal ()
  (illegal-uri "htt!p://example.com/")
  ;; unicode stuff and other illegal chars must be percent-encoded
  (illegal-uri "http://example.com/áéaдвлбы"))

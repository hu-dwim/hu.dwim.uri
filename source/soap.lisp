;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; SOAP

(def special-variable *soap-stream*)

(eval-always
  (def (function e) with-quasi-quoted-soap-xml-to-string-emitting-form-syntax ()
    (hu.dwim.quasi-quote.xml:with-quasi-quoted-xml-to-string-emitting-form-syntax '*soap-stream*)))

(def macro emit-soap-request-to-string (&body forms)
  `(with-output-to-string (*soap-stream*)
     (hu.dwim.quasi-quote:emit ,@forms)))

(def (constant e) +xml-namespace-uri/soap+ "http://www.w3.org/2003/05/soap-envelope")

(def function make-soap-envelope (body)
  {with-quasi-quoted-soap-xml-to-string-emitting-form-syntax
    (hu.dwim.quasi-quote::as-delayed-emitting
      (hu.dwim.quasi-quote.xml:emit-xml-prologue :encoding :utf-8 :stream *soap-stream* :version "1.0")
      (hu.dwim.quasi-quote:emit <soap:Envelope (xmlns:xsi  "http://www.w3.org/2001/XMLSchema-instance"
                                                xmlns:xsd  "http://www.w3.org/2001/XMLSchema"
                                                ;; NOTE: contrary to google results, it doesn't work with this soap namespace: "http://schemas.xmlsoap.org/soap/envelope/"
                                                xmlns:soap #.+xml-namespace-uri/soap+)
                                  <soap:Body ,body>>))})

(def (function e) send-soap-request (host service-url body &key proxy)
  (check-type host string)
  (check-type service-url string)
  (bind ((request (emit-soap-request-to-string (make-soap-envelope body)))
         (url (format nil "http://~A/~A" host service-url)))
    (hu.dwim.logger:standard-logger.debug "Sending soap request to ~A" url)
    (bind ((response (multiple-value-list
                      (sb-ext:with-timeout 15
                        ;; TODO add a timeout for the socket stuff without sb-ext:with-timeout
                        (drakma:http-request url
                                             :proxy proxy
                                             :method :post
                                             :content-type "application/soap+xml"
                                             :content request)))))
      (hu.dwim.logger:standard-logger.debug "Received soap response from ~A: ~S" url response)
      (when (typep (first response) '(vector (unsigned-byte 8)))
        (setf (first response) (babel:octets-to-string (coerce (first response) '(vector (unsigned-byte 8))) :encoding :utf-8)))
      (values-list response))))

(def function parse-soap-envelope/flexml (string)
  (cxml:parse string (flexml:make-flexml-builder :default-package *package*
                                                 :default-node-class 'flexml:flexml-node)))

(def function soap-envelope-body/flexml (envelope)
  (first-elt (flexml:children-of (first-elt (flexml::children-of envelope)))))

(def function parse-soap-envelope/dom (string)
  (labels ((dummy-entity-resolver (public-id system-id)
             (declare (ignore public-id system-id))
             (babel-streams:make-in-memory-input-stream #())))
    (cxml:parse string (cxml-dom:make-dom-builder) :entity-resolver #'dummy-entity-resolver)))

(def macro flexml-node-name-eswitch (value &body forms)
  (once-only (value)
    `(when (typep ,value 'flexml:flexml-node)
       (bind ((-children- (flexml:children-of node))
              (-content- (when (and (length= 1 -children-)
                                    (stringp (first-elt -children-)))
                           (flexml:string-content-of node))))
         (declare (ignorable -children- -content-))
         (flet ((recurse (function)
                  (map 'list function -children-)))
           (declare (ignorable #'recurse))
           (eswitch ((flexml:local-name-of ,value) :test #'string=)
             ,@forms))))))
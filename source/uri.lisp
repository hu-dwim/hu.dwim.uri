;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;; http://tools.ietf.org/html/rfc2396

;; encoding:
;; http://tools.ietf.org/html/rfc3490
;; http://tools.ietf.org/html/rfc3492

;;;;;;
;;; constants and tables

(def (constant :test 'equalp) +uri/alphanumeric-characters+
  (flet ((series (low high)
           (iter (for i :from (char-code low) :to (char-code high))
                 (collect (code-char i)))))
    (coerce (append (series #\a #\z)
                    (series #\A #\Z)
                    (series #\0 #\9))
            'simple-string)))

(eval-always
  (def function to-character-ok-table (string &key (initial-element #f))
    (check-type initial-element boolean)
    (bind ((result (make-array 256
                               :element-type 'boolean
                               :initial-element initial-element)))
      (loop
        :for char :across string
        :do (setf (aref result (char-code char)) (not initial-element)))
      (coerce result '(simple-array boolean (256))))))

(def function is-string-ok? (string ok-table)
  (iter (for char :in-vector string)
        (unless (aref ok-table (char-code char))
          (return #f))
        (finally (return string))))

(def (constant :test 'equalp) +uri/delimiter-characters+ ";/?:@&=+$,")
(def (constant :test 'equalp) +uri/unreserved-characters+ (string+ +uri/alphanumeric-characters+ "-_.!~*'()"))
(def (constant :test 'equalp) +uri/allowed-characters/scheme+ (string+ +uri/alphanumeric-characters+ "+-."))

;; The list of characters which don't need to be escaped when writing URIs.
;; This list is inherently a heuristic, because different uri components may have
;; different escaping needs, but it should work fine for http.
(def (constant :test 'equalp) +uri/character-ok-table+ (to-character-ok-table +uri/unreserved-characters+))
(def (constant :test 'equalp) +uri/character-ok-table/scheme+ (to-character-ok-table +uri/allowed-characters/scheme+))

(def (function io) uri/split-path (path-string)
  (split-sequence:split-sequence #\/ path-string :remove-empty-subseqs #t))

;;;;;;
;;; conditions

(def (condition e) uri-parse-error (simple-parse-error)
  ())

(def function uri-parse-error (message &rest args)
  (error 'uri-parse-error
         :format-control message
         :format-arguments args))

;;;;;;
;;; uri object

(def (class* eas) uri ()
  ((scheme
    nil
    :type (or null string))
   (host
    nil
    :type (or null string))
   (port
    nil
    :type (or null string))
   (path
    nil
    :type (or null string))
   (query
    nil
    :type (or null string))
   (query-parameters
    :unbound
    :documentation "A cache for URI/PARSE-QUERY-PARAMETERS."
    :type list)
   (fragment nil)))

(def method make-load-form ((self uri) &optional env)
  (bind ((slot-names (load-time-value
                      (remove 'query-parameters
                              (mapcar 'slot-definition-name (class-slots (ensure-finalized (find-class 'uri))))))))
    (make-load-form-saving-slots self :environment env :slot-names slot-names)))

(def print-object (uri :identity nil)
  (uri/write -self- *standard-output* :escape #f))

;;;;;;
;;; API stuff

(def (function ie) make-uri (&key scheme host port path query fragment)
  (check-type path (or string list))
  (check-type port (or null non-negative-integer))
  (check-type scheme   (or null string))
  (check-type host     (or null string))
  (check-type query    (or null string))
  (check-type fragment (or null string))
  (make-instance 'uri :scheme scheme :host host :port port :path (ensure-list path)
                 :query query :fragment fragment))

(def (function ie) clone-uri (uri &key (scheme nil scheme-provided?) (host nil host-provided?) (port nil port-provided?)
                                  (path nil path-provided?) (query nil query-provided?) (fragment nil fragment-provided?))
  "Clone URI with any provided components fully overriding its components (e.g. no path merging)."
  (bind ((result #.`(make-instance 'uri
                                   ,@(iter (for name :in '(scheme host port path query fragment))
                                           (collect (intern (string name) :keyword))
                                           (collect `(if ,(symbolicate name '#:-provided?)
                                                         ,name
                                                         (,(symbolicate name '#:-of) uri)))))))
    (when (and (not query-provided?)
               (slot-boundp uri 'query-parameters))
      (setf (query-parameters-of result) (copy-alist (query-parameters-of uri))))
    result))

(def method query-parameters-of :before ((self uri))
  (unless (slot-boundp self 'query-parameters)
    (setf (query-parameters-of self) (awhen (query-of self)
                                       (uri/parse-query-parameters it)))))

(def (function e) uri/query-parameter-value (uri name)
  (assoc-value (query-parameters-of uri) name :test #'string=))

(def (function e) (setf uri/query-parameter-value) (value uri name)
  (if value
      (setf (assoc-value (query-parameters-of uri) name :test #'string=) value)
      (removef (query-parameters-of uri) name :test #'string= :key #'car))
  value)

(def (function e) uri/add-query-parameter (uri name value)
  (nconcf (query-parameters-of uri) (list (cons name value)))
  uri)

(def (function e) uri/delete-query-parameters (uri &rest names)
  (setf (query-parameters-of uri)
        (delete-if (lambda (el)
                     (member (car el) names :test #'string=))
                   (query-parameters-of uri)))
  uri)

(def (function e) uri/delete-all-query-parameters (uri)
  (setf (query-parameters-of uri) '())
  uri)

(def (function e) uri/copy-query-parameters (from to &rest parameter-names)
  (dolist (name parameter-names)
    (setf (uri/query-parameter-value to name)
          (uri/query-parameter-value from name))))

(def (function e) uri/copy-all-query-parameters (from to)
  (setf (query-parameters-of to)
        (copy-alist (query-parameters-of from))))

(def (function e) uri/append-path (uri path)
  (bind ((path (if (stringp path) (uri/split-path path) path)))
    (setf (path-of uri) (append (path-of uri) path)))
  uri)

(def (function e) uri/prepend-path (uri path)
  (bind ((path (if (stringp path) (uri/split-path path) path)))
    (setf (path-of uri) (append path (path-of uri))))
  uri)

;;;;;;
;;; printing and parsing

(def (function o) uri/write/sans-query (uri stream &key (escape #t))
  "Write URI to STREAM, only write scheme, host and path."
  (bind ((scheme (scheme-of uri))
         (host (host-of uri))
         (port (port-of uri))
         (path (path-of uri)))
    (flet ((out (string)
             (funcall (if escape
                          #'write-in-percent-encoding
                          #'write-string)
                      string stream)))
      (when scheme
        (out scheme)
        (write-char #\: stream))
      (when host
        (write-string "//" stream)
        ;; don't percent-escape host
        (etypecase host
          (iolib:ipv6-address
           (write-char #\[ stream)
           (write-string (iolib:address-to-string host) stream)
           (write-char #\] stream))
          (iolib:ipv4-address
           (write-string (iolib:address-to-string host) stream))
          (string
           ;; NOTE idna escaping wouldn't be appropriate here
           (write-string host stream))))
      (when port
        (write-char #\: stream)
        (princ port stream))
      (iter (for el :in path)
            (write-char #\/ stream)
            (out el)))))

(def (function o) uri/write (uri stream &key (escape t) (extra-parameters '()))
  (uri/write/sans-query uri stream :escape escape)
  (labels ((out (string)
             (funcall (if escape
                          #'write-in-percent-encoding
                          #'write-string)
                      string stream)))
    (bind ((parameters (query-parameters-of uri)))
      (when extra-parameters
        (setf parameters (append extra-parameters parameters)))
      (write-query-parameters parameters stream :escape escape))
    (awhen (fragment-of uri)
      (write-char #\# stream)
      (out it))))

(def (function e) uri/print-to-string (uri &key (escape #t) (extra-parameters '()))
  (bind ((*print-pretty* #f)
         (*print-circle* #f))
    (with-output-to-string (string)
      (uri/write uri string :escape escape :extra-parameters extra-parameters))))

(def function uri/print-to-string/sans-query (uri &key (escape #t))
  (bind ((*print-pretty* #f)
         (*print-circle* #f))
    (with-output-to-string (string)
      (uri/write/sans-query uri string :escape escape))))

(def (function o) write-in-percent-encoding (string stream)
  (check-type string string)
  (check-type stream stream)
  (loop
    :for char-code :of-type (unsigned-byte 8) :across (the (simple-array (unsigned-byte 8) (*))
                                                        (babel:string-to-octets string :encoding :utf-8 :use-bom #f))
    :do (if (aref #.+uri/character-ok-table+ char-code)
            (write-char (code-char char-code) stream)
            (progn
              ;; this would be much slower... (format stream "%~2,'0X" char-code)
              (write-char #\% stream)
              (write-string (integer-to-string char-code :base 16) stream)))))

(def (function eo) uri/percent-encoding/encode (string)
  "Escapes all non alphanumeric characters in STRING following the URI convention. Returns a fresh string."
  (bind ((*print-pretty* #f)
         (*print-circle* #f))
    (with-output-to-string (escaped nil :element-type 'base-char)
      (write-in-percent-encoding string escaped))))

(def (function eo) uri/percent-encoding/decode (input)
  "URI unescape based on http://www.ietf.org/rfc/rfc2396.txt"
  (etypecase input
    (simple-base-string
     (let ((input-length (length input)))
       (when (zerop input-length)
         (return-from uri/percent-encoding/decode ""))
       (bind ((seen-escaped? #f)
              (seen-escaped-non-ascii? #f)
              (input-index 0)
              (output (make-array input-length :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
         (declare (type array-index input-length input-index))
         (labels ((read-next-char (must-exists-p)
                    (when (>= input-index input-length)
                      (if must-exists-p
                          (uri-parse-error "Unexpected end of input on ~S" input)
                          (return-from uri/percent-encoding/decode (if seen-escaped?
                                                                   (if seen-escaped-non-ascii?
                                                                       (babel:octets-to-string output :encoding :utf-8)
                                                                       (babel:octets-to-string output :encoding :us-ascii))
                                                                   input))))
                    (prog1
                        (aref input input-index)
                      (incf input-index)))
                  (write-next-byte (byte)
                    (declare (type (unsigned-byte 8) byte))
                    (when (> byte 127)
                      (setf seen-escaped-non-ascii? #t))
                    (vector-push-extend byte output)
                    (values))
                  (char-to-int (char)
                    (let ((result (digit-char-p char 16)))
                      (unless result
                        (uri-parse-error "Expecting a digit and found ~S in ~S at around position ~S" char input input-index))
                      result))
                  (parse ()
                    (let ((next-char (read-next-char nil)))
                      (case next-char
                        (#\% (char%))
                        (#\+ (char+))
                        (t (write-next-byte (char-code next-char))))
                      (parse)))
                  (char% ()
                    (setf seen-escaped? #t)
                    (write-next-byte (+ (ash (char-to-int (read-next-char t)) 4)
                                        (char-to-int (read-next-char t))))
                    (values))
                  (char+ ()
                    (setf seen-escaped? #t)
                    (write-next-byte #.(char-code #\Space))))
           (parse)))))
    (string
     (uri/percent-encoding/decode (coerce input 'simple-base-string)))))

(def (function eo) parse-uri (uri)
  ;; can't use :sharedp, because we expect the returned pieces to be simple-base-string's and :sharedp would return displaced arrays
  (etypecase uri
    (simple-base-string
     (bind ((pieces (nth-value 1 (cl-ppcre:scan-to-strings "^(([^:/?#]+):)?(//([^:/?#]*)(:([0-9]+)?)?)?([^?#]*)(\\?([^#]*))?(#(.*))?"
                                                           uri :sharedp #f))))
       (flet ((process (index)
                (bind ((piece (aref pieces index)))
                  (values (if (and piece
                                   (not (zerop (length piece))))
                              (uri/percent-encoding/decode piece)
                              nil)))))
         (declare (inline process)
                  (dynamic-extent #'process))
         ;; call uri/percent-encoding/decode on each piece separately, so some of them may remain simple-base-string even if other pieces contain unicode
         (make-uri :scheme   (bind ((scheme (aref pieces 1)))
                               (when (and scheme
                                          (not (is-string-ok? scheme +uri/character-ok-table/scheme+)))
                                 (uri-parse-error "Scheme ~S contains illegal characters" scheme))
                               scheme)
                   :host     (awhen (aref pieces 3)
                               (uri/percent-encoding/decode it))
                   :port     (bind ((port-string (aref pieces 5)))
                               (when port-string
                                 (bind (((:values port position) (parse-integer port-string :junk-allowed #t)))
                                   (when (or (< port 0)
                                             (not (eql position (length port-string))))
                                     (uri-parse-error "Port ~S is not a non-negative integer" port-string))
                                   port)))
                   :path     (mapcar 'uri/percent-encoding/decode (uri/split-path (aref pieces 6)))
                   :query    (aref pieces 8) ; see URI/PARSE-QUERY-PARAMETERS
                   :fragment (process 10)))))
    (string
     (parse-uri (coerce uri 'simple-base-string)))))

;;;;;;
;;; query parameters

(def (function o) write-query-parameters (parameters stream &key (escape t))
  (labels ((out (string)
             (funcall (if escape
                          #'write-in-percent-encoding
                          #'write-string)
                      string stream)
             (values))
           (write-query-part (name value)
             (if (consp value)
                 (iter (for el :in value)
                       (unless (first-time-p)
                         (write-char #\& stream))
                       (out name)
                       (write-char #\= stream)
                       (write-query-value el))
                 (progn
                   (out name)
                   (write-char #\= stream)
                   (write-query-value value))))
           (write-query-value (value)
             (out (typecase value
                    (integer (integer-to-string value))
                    (number (princ-to-string value))
                    (null "")
                    (t (string value))))))
    (iter (for (name . value) :in parameters)
          (write-char (if (first-time-p) #\? #\&) stream)
          (write-query-part name value))))

(def macro record-query-parameter (param params)
  (declare (type cons param))
  (once-only (param)
    `(bind ((entry (assoc (car ,param) ,params :test #'string=)))
       (if entry
           (progn
             (unless (consp (cdr entry))
               (setf (cdr entry) (list (cdr entry))))
             (nconcf (cdr entry) (list (cdr ,param))))
           (push ,param ,params))
       ,params)))

(def (function eo) uri/parse-query-parameters (param-string &key initial-parameters (sideffect-initial-parameters #f))
  "Parse PARAM-STRING into an alist. The value part will be a list if the given parameter was found multiple times."
  (declare (type simple-base-string param-string))
  (labels ((make-displaced-array (array &optional (start 0) (end (length array)))
             (make-array (- end start)
                         :element-type (array-element-type array)
                         :displaced-to array
                         :displaced-index-offset start))
           (grab-param (start separator-position end)
             (declare (type array-index start end)
                      (type (or null array-index) separator-position))
             (bind ((key-start start)
                    (key-end (or separator-position end))
                    (key (make-displaced-array param-string key-start key-end))
                    (value-start (if separator-position
                                     (1+ separator-position)
                                     end))
                    (value-end end)
                    (value (if (zerop (- value-end value-start))
                               ""
                               (make-displaced-array param-string value-start value-end)))
                    (unescaped-key (uri/percent-encoding/decode key))
                    (unescaped-value (uri/percent-encoding/decode value)))
               (cons unescaped-key unescaped-value))))
    (when (and param-string
               (< 0 (length param-string)))
      (iter
        (with start = 0)
        (with separator-position = nil)
        (with result = (if sideffect-initial-parameters
                           initial-parameters
                           (copy-alist initial-parameters)))
        (for char :in-vector param-string)
        (for index :upfrom 0)
        (switch (char :test #'char=)
          (#\& ;; end of the current param
           (setf result (record-query-parameter (grab-param start separator-position index) result))
           (setf start (1+ index))
           (setf separator-position nil))
          (#\= ;; end of name
           (setf separator-position index)))
        (finally
         (return (nreverse (record-query-parameter (grab-param start separator-position (1+ index)) result))))))))

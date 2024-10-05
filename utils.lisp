(uiop:define-package :lem-rooms-client/utils
  (:use #:cl)
  (:export #:hash
           #:json
           #:pretty-json
           #:pretty-json-to-string
           #:print-json))
(in-package :lem-rooms-client/utils)

(defun hash (&rest plist)
  (let ((hash (make-hash-table :test 'equal)))
    (loop :for (key value) :on plist :by #'cddr
          :do (setf (gethash (string-downcase key)
                             hash)
                    value))
    hash))

(defun json (&rest plist)
  (with-output-to-string (out)
    (yason:encode (apply #'hash plist) out)))

(defun pretty-json (object &optional (stream *standard-output*))
  (yason:with-output (stream :indent t)
    (yason:encode object)))

(defun pretty-json-to-string (object)
  (with-output-to-string (out)
    (pretty-json object out)))

(defun print-json (json &optional (stream *standard-output*))
  (yason:with-output (stream :indent t)
    (yason:encode (yason:parse json))))

(uiop:define-package :lem-rooms-client/config
  (:use #:cl
	#:lem)
  (:export #:base-url
           #:access-token))
(in-package :lem-rooms-client/config)

(defparameter *base-url* "http://localhost")

(defun base-url ()
  "http://localhost")

(defun (setf base-url) (base-url)
  (setf *base-url* base-url))

(defun access-token ()
  (config :rooms.access-token))

(defun (setf access-token) (access-token)
  (setf (config :rooms.access-token) access-token))

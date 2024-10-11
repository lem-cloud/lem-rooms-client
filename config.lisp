(uiop:define-package :lem-rooms-client/config
  (:use #:cl
	#:lem)
  (:export #:access-token
           #:hostname))
(in-package :lem-rooms-client/config)

(defun access-token ()
  (config :rooms.access-token))

(defun (setf access-token) (access-token)
  (setf (config :rooms.access-token) access-token))

(defun hostname ()
  "localhost")

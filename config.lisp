(uiop:define-package :lem-rooms-client/config
  (:use #:cl
	#:lem)
  (:export #:*rooms-url*
           #:*editor-server-url*
           #:access-token))
(in-package :lem-rooms-client/config)

(defparameter *rooms-url* "http://localhost:5000")
(defparameter *editor-server-url* "http://localhost:51000")

(defun access-token ()
  (config :rooms.access-token))

(defun (setf access-token) (access-token)
  (setf (config :rooms.access-token) access-token))

(uiop:define-package :lem-rooms-client/config
  (:use #:cl
	#:lem)
  (:export #:*rooms-url*
           #:*editor-server-url*
           #:access-token))
(in-package :lem-rooms-client/config)

(defparameter *rooms-url* "https://lunoa.taild35c18.ts.net")
(defparameter *editor-server-url* "https://arcalk.taild35c18.ts.net/")

(defun access-token ()
  (config :rooms.access-token))

(defun (setf access-token) (access-token)
  (setf (config :rooms.access-token) access-token))

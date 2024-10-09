(uiop:define-package :lem-rooms-client/sign-in
  (:use #:cl
	#:lem
	#:alexandria
	#:lem-rooms-client/utils
	#:lem-rooms-client/config)
  (:local-nicknames (:http :lem-rooms-client/http))
  (:export #:rooms-sign-in))
(in-package :lem-rooms-client/sign-in)

(defun authorize-url ()
  (let ((json (http:get "/editor-server/github/authorize-url")))
    (gethash "url" json)))

(defun authenticate (code)
  (http:get (format nil "/editor-server/github/authenticate?code=~A" code)))

(define-command rooms-sign-in () ()
  (lem:open-external-file (authorize-url))
  (let ((code (prompt-for-string "code: ")))
    (setf (access-token)
          (gethash "access_token" (authenticate code)))
    (message "sign in")))

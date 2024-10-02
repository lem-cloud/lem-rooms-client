(defpackage :lem-rooms-client
  (:use :cl :lem)
  (:export))
(in-package :lem-rooms-client)

(defun authorize-url ()
  (let ((json (yason:parse (dex:get "http://localhost:5000/editor-server/github/authorize-url"))))
    (gethash "url" json)))

(defun authenticate (code)
  (yason:parse (dex:get (format nil "http://localhost:5000/editor-server/github/authenticate?code=~A" code))))

(define-command rooms-sign-in () ()
  (lem:open-external-file (authorize-url))
  (let ((code (prompt-for-string "code: ")))
    (setf (config :rooms.access-token)
          (gethash "access_token" (authenticate code)))
    (message "sign in")))

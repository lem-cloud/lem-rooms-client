(uiop:define-package :lem-rooms-client/http
  (:use #:cl
        #:lem-rooms-client/config)
  (:shadow #:get)
  (:export #:get
           #:post))
(in-package :lem-rooms-client/http)

(defun url (path)
  (quri:make-uri :defaults "http://localhost:5000/"
                 :path path))

(defun headers (authorization)
  `(("content-type" . "application/json")
    ,@(when authorization
        `(("Authorization" . ,(format nil "Bearer ~A" (access-token)))))))

(defun get (path &key authorization)
  (yason:parse (dex:get (url path)
                        :headers (headers authorization))))

(defun post (path content)
  (yason:parse (dex:post (url path)
                         :headers (headers t)
                         :content content)))

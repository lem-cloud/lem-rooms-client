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

(defun prompt-code-with-browser-frontend (authorize-url)
  (js-eval (current-window)
           (format nil "let x = document.createElement('a'); x.href = '~A'; x.target = '_blank'; document.body.appendChild(x); x.click(); document.body.removeChild(x);" authorize-url))
  ;; (js-eval (current-window) (format nil "window.open('~A', '_blank')" authorize-url))
  (let ((code (js-eval (current-window) "prompt('code: ')" :wait t)))
    code))

(defun prompt-code (authorize-url)
  (if (and (find-package :lem-server)
           (typep (lem:implementation) (find-symbol "JSONRPC" :lem-server)))
      (prompt-code-with-browser-frontend authorize-url)
      (prompt-for-string "code: ")))

(define-command rooms-sign-in () ()
  (let ((authorize-url (authorize-url)))
    (ignore-errors (open-external-file authorize-url))
    (when-let ((code (prompt-code authorize-url)))
      (setf (access-token)
            (gethash "access_token" (authenticate code)))
      (message "sign in"))))

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

(defun open-authorize-url-with-browser-frontend (authorize-url)
  (js-eval (current-window)
           (format nil "let x = document.createElement('a');~@
                        x.href = '~A';~@
                        x.target = '_blank';~@
                        document.body.appendChild(x);~@
                        x.click();~@
                        document.body.removeChild(x);"
                   authorize-url)))

(defun prompt-code-with-browser-frontend ()
  (js-eval (current-window) "prompt('code: ')" :wait t))

(defun prompt-code ()
  (if (browser-frontend-p)
      (prompt-code-with-browser-frontend)
      (prompt-for-string "code: ")))

(define-command rooms-sign-in () ()
  (let ((authorize-url (authorize-url)))
    (handler-case (open-external-file authorize-url)
      (error ()
        (when (browser-frontend-p)
          (open-authorize-url-with-browser-frontend authorize-url))))
    (when-let ((code (prompt-code)))
      (setf (access-token)
            (gethash "access_token" (authenticate code)))
      (message "Sign-in Successful"))))

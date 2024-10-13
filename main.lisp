(uiop:define-package :lem-rooms-client
  (:use #:cl
        #:lem
        #:alexandria
        #:lem-rooms-client/utils
        #:lem-rooms-client/config)
  (:local-nicknames (:http :lem-rooms-client/http)
                    (:woot :crdt/woot))
  (:export))
(in-package :lem-rooms-client)

(defvar *inhibit-did-change* nil)

(defun rooms-home ()
  (merge-pathnames "Rooms/" (user-homedir-pathname)))

(defun buffer-file-id (buffer)
  (buffer-value buffer 'file-id))

(defun (setf buffer-file-id) (file-id buffer)
  (setf (buffer-value buffer 'file-id) file-id))

(defun buffer-file-path (buffer)
  (buffer-value buffer 'file-path))

(defun (setf buffer-file-path) (file-path buffer)
  (setf (buffer-value buffer 'file-path) file-path))

(defun buffer-document (buffer)
  (buffer-value buffer 'document))

(defun (setf buffer-document) (document buffer)
  (setf (buffer-value buffer 'document) document))

(defun find-buffer-by-file-id (file-id)
  (dolist (buffer (buffer-list))
    (when (equal file-id (buffer-file-id buffer))
      (return buffer))))

(defun position-of (point)
  (1- (position-at-point point)))

(defun get-rooms ()
  (http:get "rooms-v2"))

(defun create-room (room-name)
  (http:post "rooms-v2"
             (json :name room-name
                   :scope "public")))

(defun fetch-room (room-id)
  (http:get (format nil "rooms-v2/~A" room-id)
            :authorization t))

(defvar *client* nil)

(defun connect ()
  (setf *client* (jsonrpc:make-client))
  (jsonrpc:expose *client* "woot/insert" 'on-insert)
  (jsonrpc:expose *client* "woot/delete" 'on-delete)
  (jsonrpc:client-connect *client*
                          :mode :websocket
                          :host (hostname)
                          :port 51000))

(defun ensure-connection ()
  (unless *client*
    (connect)))

(defun jsonrpc-call (method params)
  (ensure-connection)
  (log:info "jsonrpc-call: ~A ~A" method (pretty-json-to-string params))
  (let ((response (jsonrpc:call *client* method params)))
    (log:info "jsonrpc-call response: ~A ~A" method (pretty-json-to-string response))
    response))

(defun jsonrpc-notify (method params)
  (ensure-connection)
  (log:info "jsonrpc-notify: ~A ~A" method (pretty-json-to-string params))
  (jsonrpc:notify *client* method params))

(defun fetch-file-text (file-id)
  (jsonrpc-call "fetch-file-text"
                (hash :access-token (access-token)
                      :file-id file-id)))

(defun enter-room (room-id)
  (let ((response
          (jsonrpc-call "enter-room"
                        (hash :access-token (access-token)
                              :room-id room-id)))
        (room-directory
          (uiop:ensure-directory-pathname
           (merge-pathnames room-id (rooms-home)))))
    (defparameter $response response)
    (ensure-directories-exist room-directory)

    (dolist (file (gethash "files" response))
      (let ((text (fetch-file-text (gethash "id" file))))
        (write-string-into-file text
                                (merge-pathnames (gethash "path" file)
                                                 room-directory)
                                :if-exists :supersede)))

    (add-hook *find-file-hook* 'on-find-file)
    (add-hook (variable-value 'before-change-functions :global t) 'on-before-change)
    (find-file room-directory)))

(defvar *room-path-regex* nil)

(defun room-path-p (filename)
  (unless *room-path-regex*
    (setf *room-path-regex*
          (ppcre:create-scanner `(:SEQUENCE
                                  :START-ANCHOR
                                  ,(namestring (rooms-home))
                                  (:GREEDY-REPETITION 0 NIL :EVERYTHING)
                                  "/"))))
  (ppcre:scan *room-path-regex* (namestring filename)))

(defun file-room-id (filename)
  (ppcre:register-groups-bind (room-id)
      (`(:SEQUENCE
         ,(namestring (rooms-home))
         (:REGISTER (:GREEDY-REPETITION 0 NIL :EVERYTHING)) "/")
       filename)
    room-id))

(defun file-to-room-path (room-id filename)
  (enough-namestring filename
                     (uiop:ensure-directory-pathname
                      (merge-pathnames room-id (rooms-home)))))

(defun set-file-info (buffer file)
  (setf (buffer-file-id buffer)
        (gethash "id" file)
        (buffer-file-path buffer)
        (gethash "path" file)))

(defun set-buffer-text (buffer text)
  (let ((*inhibit-did-change* t))
    (delete-between-points (buffer-start-point buffer)
                           (buffer-end-point buffer))
    (insert-string (buffer-point buffer) text)
    (when (buffer-enable-undo-p buffer)
      (buffer-disable-undo buffer)
      (buffer-enable-undo buffer))
    (buffer-start (buffer-point buffer))))

(defun make-document-from-text (text)
  (let ((document (woot:make-document (frugal-uuid:make-v4-string))))
    (loop :for c :across text
          :for pos :from 0
          :do (woot:generate-insert document pos (string c)))
    document))

(defun on-find-file (buffer)
  (when-let ((filename (buffer-filename buffer)))
    (when (room-path-p filename)
      (setf (buffer-document buffer)
            (make-document-from-text (buffer-text buffer)))
      (setf (revert-buffer-function buffer) (lambda (buffer) (declare (ignore buffer))))
      (let* ((room-id (file-room-id filename))
             (response
               (jsonrpc-call "open-file"
                             (hash :access-token (access-token)
                                   :room-id room-id
                                   :path (file-to-room-path room-id filename)
                                   :characters (woot::document-sequence
                                                (buffer-document buffer))))))
        (defparameter $open-file-response response)
        (cond ((null (gethash "error" response))
               (set-file-info buffer response))
              ((equal "already-file-exists" (gethash "error" response))
               (let ((file (gethash "file" response))
                     (woot-sequence (gethash "woot-sequence" response)))
                 (woot:replace-with-woot-sequence (buffer-document buffer)
                                                  (map 'list
                                                       #'woot:make-character-from-hash
                                                       woot-sequence))
                 (set-buffer-text buffer (woot:get-string (buffer-document buffer)))
                 (set-file-info buffer file)))
              (t
               (with-output-to-string (*standard-output*)
                 (editor-error "Rooms error: open-file: ~A"
                               (pretty-json-to-string response)))))))))

(defun on-before-change (point arg)
  (unless *inhibit-did-change*
    (let* ((buffer (point-buffer point))
           (filename (buffer-filename buffer)))
      (when (and filename (room-path-p filename) (buffer-document buffer))
        (let ((file-id (buffer-file-id buffer)))
          (etypecase arg
            (string
             (loop :for c :across arg
                   :for pos :from (position-of point)
                   :do (let ((woot-char (woot:generate-insert (buffer-document buffer)
                                                              pos
                                                              (string c))))
                         (jsonrpc-notify "woot/insert"
                                         (hash :access-token (access-token)
                                               :file-id file-id
                                               :character woot-char)))))
            (integer
             (with-point ((end point))
               (character-offset end arg)
               (loop :repeat arg
                     :do (let ((woot-char
                                 (woot:generate-delete (buffer-document buffer)
                                                       (position-of point))))
                           (jsonrpc-notify "woot/delete"
                                           (hash :access-token (access-token)
                                                 :file-id file-id
                                                 :character woot-char))))))))))))

(defun on-insert (params)
  (let ((*inhibit-did-change* t))
    (when-let ((buffer (find-buffer-by-file-id (gethash "file-id" params))))
      (let ((character (woot:make-character-from-hash (gethash "character" params))))
        (woot:insert-char (buffer-document buffer) character)
        (let ((pos (woot:char-position (buffer-document buffer) character)))
          (with-point ((point (buffer-point buffer)))
            (move-to-position point pos)
            (insert-string point (woot:woot-char-value character))
            (redraw-display)))))))

(defun on-delete (params)
  (let ((*inhibit-did-change* t))
    (when-let ((buffer (find-buffer-by-file-id (gethash "file-id" params))))
      (let* ((character (woot:make-character-from-hash (gethash "character" params)))
             (pos (woot:char-position (buffer-document buffer) character)))
        (message "pos: ~A" pos)
        (with-point ((point (buffer-point buffer)))
          (move-to-position point (1+ pos))
          (delete-character point 1)
          (woot:delete-char (buffer-document buffer) character)
          (redraw-display))))))

(define-command rooms-list () ()
  (lem/multi-column-list:display
   (make-instance 'lem/multi-column-list:multi-column-list
                  :columns '("Room" "Scope" "Owner")
                  :column-function (lambda (component room)
                                     (declare (ignore component))
                                     (list (format nil "~A  " (gethash "name" room))
                                           (format nil "~A  " (gethash "scope" room))
                                           (format nil
                                                   "~A  "
                                                   (gethash "github_login"
                                                            (gethash "owner" room)))))
                  :items (get-rooms)
                  :select-callback (lambda (component room)
                                     (lem/multi-column-list:quit component)
                                     (enter-room (gethash "id" room))))))

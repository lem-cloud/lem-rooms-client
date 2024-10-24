(uiop:define-package :lem-rooms-client
  (:use #:cl
        #:lem
        #:alexandria
        #:lem-rooms-client/utils
        #:lem-rooms-client/config)
  (:local-nicknames (:http :lem-rooms-client/http)
                    (:woot :crdt/woot)
                    (:cursor :lem-rooms-client/cursor))
  (:export))
(in-package :lem-rooms-client)

(defvar *inhibit-change-notification* nil)

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
  (http:get "/rooms-v2"))

(defun create-room (room-name)
  (http:post "/rooms-v2"
             (json :name room-name
                   :scope "public")))

(defun fetch-room (room-id)
  (http:get (format nil "rooms-v2/~A" room-id)
            :authorization t))

(defvar *client* nil)

(defun connect ()
  (setf *client* (jsonrpc:make-client))
  (jsonrpc:expose *client* "woot/edit" 'on-edit)
  (jsonrpc:expose *client* "focus" 'on-focus)
  (let ((url (quri:uri *editor-server-url*)))
    (jsonrpc:client-connect *client*
                            :mode :websocket
                            :securep (equal "https" (quri:uri-scheme url))
                            :host (quri:uri-host url)
                            :port (quri:uri-port url))))

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


(defvar *edit-queue-map* (make-hash-table :test 'equal))

(defun empty-queue-p (file-id)
  (if-let (queue (gethash file-id *edit-queue-map*))
    (lem/common/queue:empty-p queue)
    t))

(defun enqueue (file-id edit)
  (unless (gethash file-id *edit-queue-map*)
    (setf (gethash file-id *edit-queue-map*)
          (lem/common/queue:make-queue)))
  (lem/common/queue:enqueue (gethash file-id *edit-queue-map*) edit))

(defun bulk-notify ()
  (maphash (lambda (file-id queue)
             (jsonrpc-notify "woot/edit"
                             (hash :access-token (access-token)
                                   :file-id file-id
                                   :ops (loop :until (lem/common/queue:empty-p queue)
                                              :append (lem/common/queue:dequeue queue)))))
           *edit-queue-map*)
  (notify-current-cursor))

(defvar *notification-timer*
  (make-idle-timer 'bulk-notify :name "lem-rooms-client/bulk-notify"))

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

    (start-timer *notification-timer* 100 :repeat t)

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
  (let ((*inhibit-change-notification* t))
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

(defun notify-current-cursor ()
  (when-let (file-id (buffer-file-id (current-buffer)))
    (jsonrpc-notify "focus"
                    (hash :access-token (access-token)
                          :file-id file-id
                          :position (position-of (current-point))))))

(defun on-before-change (point arg)
  (unless *inhibit-change-notification*
    (let* ((buffer (point-buffer point))
           (filename (buffer-filename buffer)))
      (when (and filename (room-path-p filename) (buffer-document buffer))
        (let ((file-id (buffer-file-id buffer)))
          (etypecase arg
            (string
             (enqueue file-id
                      (loop :for c :across arg
                            :for pos :from (position-of point)
                            :collect (let ((woot-char
                                             (woot:copy-woot-char
                                              (woot:generate-insert (buffer-document buffer)
                                                                    pos
                                                                    (string c)))))
                                       (hash :operate "insert"
                                             :character woot-char)))))
            (integer
             (with-point ((end point))
               (unless (character-offset end arg)
                 (buffer-end end))
               (enqueue file-id
                        (loop :with pos := (position-of point)
                              :repeat (count-characters point end)
                              :collect (let ((woot-char
                                               (woot:copy-woot-char
                                                (woot:generate-delete (buffer-document buffer)
                                                                      pos))))
                                         (hash :operate "delete"
                                               :character woot-char))))))))))))

(defun on-edit (params)
  (send-event
   (lambda ()
     (let ((*inhibit-change-notification* t))
       (with-inhibit-undo ()
         (when-let ((buffer (find-buffer-by-file-id (gethash "file-id" params))))
           (map ()
                (lambda (op)
                  (let ((operate (gethash "operate" op))
                        (character (woot:make-character-from-hash (gethash "character" op))))
                    (eswitch (operate :test #'string=)
                      ("insert"
                       (woot:insert-char (buffer-document buffer) character)
                       (let ((pos (woot:char-position (buffer-document buffer) character)))
                         (assert pos () "op: insert, pos is nil but expect integer")
                         (with-point ((point (buffer-point buffer)))
                           (move-to-position point (1+ pos))
                           (lem/buffer/internal::recompute-undo-position-offset
                            buffer
                            (lem/buffer/internal::make-edit :insert-string
                                                            (position-at-point point)
                                                            (woot:woot-char-value character)))
                           (insert-string point (woot:woot-char-value character)))))
                      ("delete"
                       (let ((pos (woot:char-position (buffer-document buffer) character)))
                         (assert pos () "op: delete, pos is nil but expect integer")
                         (when-let ((string (woot:delete-char (buffer-document buffer) character)))
                           (with-point ((point (buffer-point buffer)))
                             (move-to-position point (1+ pos))
                             (lem/buffer/internal::recompute-undo-position-offset
                              buffer
                              (lem/buffer/internal::make-edit :delete-string
                                                              (position-at-point point)
                                                              (woot:woot-char-value character)))
                             (delete-character point 1))))))))
                (gethash "ops" params)))))
     (redraw-display))))

(defun destructuring-focus-parameters (params)
  (let* ((position (gethash "position" params))
         (file-id (gethash "file-id" params))
         (color (gethash "color" params))
         (user (gethash "user" params))
         (user-id (gethash "id" user))
         (user-name (gethash "name" user)))
    (list :position position
          :file-id file-id
          :user-id user-id
          :user-name user-name
          :color color)))

(defun on-focus (params)
  (destructuring-bind (&key position file-id user-id user-name color)
      (destructuring-focus-parameters params)
    (lem:send-event (lambda ()
                      (when-let (buffer (find-buffer-by-file-id file-id))
                        (cursor:set-cursor buffer
                                           user-id
                                           user-name
                                           color
                                           (1+ position)))))))

(define-command rooms-list () ()
  (unless (access-token)
    (lem-rooms-client/sign-in:rooms-sign-in))
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

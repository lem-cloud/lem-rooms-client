(defpackage :lem-rooms-client/management-pane
  (:use #:cl
        #:lem
        #:lem-rooms-client/editor
        #:lem-rooms-client/utils)
  (:local-nicknames (#:api-client #:lem-rooms-client/api-client)
                    (#:room #:lem-rooms-client/room)
                    (#:agent-api #:lem-rooms-client/agent-api))
  (:export #:convert-comments
           #:make-management-pane
           #:update))
(in-package :lem-rooms-client/management-pane)

(define-major-mode rooms-mode nil
    (:name "Rooms"
     :keymap *rooms-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t
        (not-switchable-buffer-p (current-buffer)) t))

(define-key *rooms-mode-keymap* "c" 'rooms-comment)

(defstruct comment
  user-name
  user-color
  text
  date)

(defun convert-comments (comments)
  (defparameter $comments comments)
  (map 'list
       (lambda (comment)
         (make-comment :user-name (gethash "name" (gethash "user" comment))
                       :user-color (gethash "color" (gethash "user" comment))
                       :text (gethash "text" comment)
                       :date (local-time:parse-timestring (gethash "date" comment))))
       comments))

(defclass management-pane ()
  ((room-id :initarg :room-id
            :reader management-pane-room-id)
   (buffer :initarg :buffer
           :reader management-pane-buffer)
   (status-buffer :initarg :status-buffer
                  :reader management-pane-status-buffer)
   (users-buffer :initarg :users-buffer
                 :reader management-pane-users-buffer)
   (comment-buffer :initarg :comment-buffer
                   :reader management-pane-comment-buffer)))

(defun make-management-pane (&key room-id)
  (let ((buffer (make-buffer "*Rooms right-side-pane*" :temporary t :enable-undo-p nil)))
    (change-buffer-mode buffer 'rooms-mode)
    (let ((pane (make-instance 'management-pane
                               :room-id room-id
                               :buffer buffer
                               :status-buffer (make-buffer "*Rooms status*"
                                                           :temporary t
                                                           :enable-undo-p nil)
                               :users-buffer (make-buffer "*Rooms users*"
                                                          :temporary t
                                                          :enable-undo-p nil)
                               :comment-buffer (make-buffer "*Rooms comments*"
                                                            :temporary t
                                                            :enable-undo-p nil))))
      (setf (buffer-value buffer 'management-pane) pane)
      pane)))

(defun insert-color-text (point string color)
  (insert-string point
                 string
                 :attribute (make-attribute :foreground (best-foreground-color color)
                                            :background color)))

(defun update (pane &key (client nil client-p) (users nil users-p) adding-comments)
  (with-save-cursor (current-buffer)
    (let ((buffer (management-pane-buffer pane)))
      (with-buffer-read-only buffer nil
        (erase-buffer buffer)
        (with-point ((point (buffer-point buffer) :left-inserting))
          (insert-string point
                         "Rooms"
                         :attribute (make-attribute
                                     :bold t
                                     :foreground (best-foreground-color (background-color))))
          (insert-character point #\newline)
          (insert-character point #\newline)
          (insert-string point "Status:" :attribute (make-attribute :bold t))
          (insert-character point #\newline)
          (let ((status-buffer (management-pane-status-buffer pane)))
            (when client-p
              (erase-buffer status-buffer)
              (with-point ((point (buffer-point status-buffer) :left-inserting))
                (ecase (api-client:client-connection-status client)
                  (:connecting
                   (insert-string point
                                  "Connecting..."
                                  :attribute (make-attribute :foreground "orange")))
                  (:connected
                   (insert-string point
                                  "Connected"
                                  :attribute (make-attribute :foreground "green")))
                  (:disconnected
                   (insert-string point
                                  "Disconnected"
                                  :attribute (make-attribute :foreground "red"))))))
            (insert-buffer point status-buffer))
          (insert-character point #\newline)
          (insert-string point "Users:" :attribute (make-attribute :bold t))
          (insert-character point #\newline)
          (let ((users-buffer (management-pane-users-buffer pane)))
            (when users-p
              (erase-buffer users-buffer)
              (with-point ((point (buffer-point users-buffer) :left-inserting))
                (do-sequence (user users)
                  (let ((name (gethash "name" user))
                        (color (gethash "color" user)))
                    (insert-color-text point (format nil " ~A " name) color)
                    (insert-character point #\newline)))))
            (insert-buffer point users-buffer))
          (insert-string point "Comments:" :attribute (make-attribute :bold t))
          (insert-character point #\newline)
          (insert-string (buffer-point buffer) "press 'c' to comment")
          (insert-character point #\newline)
          (insert-character point #\newline)
          (let ((comment-buffer (management-pane-comment-buffer pane)))
            (with-point ((point (buffer-point comment-buffer) :left-inserting))
              (dolist (comment adding-comments)
                (buffer-start point)
                (insert-string point
                               (format nil
                                       "[~2,'0D:~2,'0D:~2,'0D]"
                                       (local-time:timestamp-hour (comment-date comment))
                                       (local-time:timestamp-minute (comment-date comment))
                                       (local-time:timestamp-second (comment-date comment))))
                (insert-color-text point
                                   (format nil " ~A " (comment-user-name comment))
                                   (comment-user-color comment))
                (insert-string point (format nil ": ~A~%" (comment-text comment)))))
            (insert-buffer point comment-buffer)))
        (buffer-start (buffer-point buffer))))))

(define-command rooms-comment () ()
  (with-save-cursor (current-buffer)
    (let ((room (if (mode-active-p (current-buffer) 'rooms-mode)
                    (room:find-room-by-id (management-pane-room-id (buffer-value (current-buffer) 'management-pane)))
                    (room:find-room-by-file (buffer-directory (current-buffer))))))
      (with-current-buffer (lem-rooms-client/management-pane::management-pane-buffer
                            (room:room-management-pane room))
        (with-current-window (frame-rightside-window (current-frame))
          (buffer-end (current-point))
          (let ((text (prompt-for-string "Comment: "
                                         :test-function (lambda (s) (plusp (length s)))
                                         :gravity :cursor
                                         :use-border nil)))
            (agent-api:comment :room-id (room:room-id room)
                               :text text)))))))

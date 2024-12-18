(uiop:define-package #:lem-rooms-client/room
  (:use #:cl
        #:alexandria)
  (:shadow #:room)
  (:export #:room-management-pane
           #:room-client-id
           #:room-id
           #:room-directory
           #:room-invitation
           #:room-owner-p
           #:register-room
           #:find-room-by-id
           #:find-room-by-file
           #:update-room-users))
(in-package #:lem-rooms-client/room)

(defvar *rooms* '())

(defstruct room
  id
  client-id
  directory
  management-pane
  invitation
  owner-p
  users)

(defun register-room (&key room-id client-id directory management-pane owner-p)
  (let ((room (make-room :id room-id
                         :client-id client-id
                         :directory directory
                         :management-pane management-pane
                         :owner-p owner-p)))
    (push room *rooms*)
    room))

(defun find-room-by-id (room-id)
  (find room-id *rooms* :key #'room-id :test #'equal))

(defun find-room-by-file (file)
  (dolist (room *rooms*)
    (let ((root-path (namestring (room-directory room)))
          (sub-path (namestring file)))
      (when (starts-with-subseq root-path sub-path)
        (return room)))))

(defun update-room-users (room users)
  (setf (room-users room) users))

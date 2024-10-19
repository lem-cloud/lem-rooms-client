(uiop:define-package #:lem-rooms-client/cursor
  (:use #:cl)
  (:export #:set-cursor))
(in-package #:lem-rooms-client/cursor)

(defclass cursor-overlay (lem-core::cursor-overlay)
  ((user-id :initarg :user-id)
   (user-name :initarg :user-name)
   (popup-message :initarg :popup-message
                  :initform nil
                  :accessor cursor-overlay-popup-message)))

(defun make-cursor-overlay (&key point attribute user-id user-name)
  (make-instance 'cursor-overlay
                 :user-id user-id
                 :user-name user-name
                 :start point
                 :end point
                 :buffer (lem:point-buffer point)
                 :temporary nil
                 :fake t
                 :attribute attribute))

(defun delete-cursor-overlay (cursor-overlay)
  (alexandria:when-let (popup-message (cursor-overlay-popup-message cursor-overlay))
    (lem:delete-popup-message popup-message))
  (lem:delete-overlay cursor-overlay))

(defun buffer-cursors (buffer)
  (or (lem:buffer-value buffer 'cursors)
      (setf (lem:buffer-value buffer 'cursors)
            (make-hash-table :test 'equal))))

(defun get-cursor (buffer id)
  (gethash id (buffer-cursors buffer)))

(defun (setf get-cursor) (cursor buffer id)
  (setf (gethash id (buffer-cursors buffer)) cursor))

(defun set-cursor (buffer id name color position)
  (let ((cursor (get-cursor buffer id)))
    (cond ((null cursor)
           (assert color)
           (let ((attribute (lem:make-attribute :foreground "white" :background color)))
             (lem:with-point ((point (lem:buffer-point buffer)))
               (lem:move-to-position point position)
               (setf (get-cursor buffer id)
                     (make-cursor-overlay :point point
                                          :attribute attribute
                                          :user-id id
                                          :user-name name)))))
          (t
           (lem:move-to-position (lem:overlay-start cursor)
                                 position)
           (lem:move-to-position (lem:overlay-end cursor)
                                 position))))
  (let ((cursor (get-cursor buffer id)))
    (alexandria:when-let (popup-message (cursor-overlay-popup-message cursor))
      (lem:delete-popup-message popup-message))
    (lem:save-excursion
      (setf (lem:current-buffer) buffer)
      (lem:move-to-position (lem:current-point) position)
      (setf (cursor-overlay-popup-message cursor)
            (lem:display-popup-message (format nil " ~A " name)
                                       :timeout 1
                                       :style `(:gravity :cursor
                                                :use-border nil
                                                :background-color ,color
                                                :offset-y -1
                                                :cursor-invisible t)))))
  (values))

(uiop:define-package :lem-rooms-client/woot
  (:use #:cl
        #:alexandria)
  (:export #:char-id-site-id
           #:char-id-local-id
           #:woot-char-id
           #:woot-char-visible
           #:woot-char-value
           #:woot-char-next
           #:woot-char-previous
           #:generate-insert
           #:generate-delete))
(in-package :lem-rooms-client/woot)

(defvar *local-id* 0)

(defstruct char-id
  (site-id (error "site-id is required"))
  (local-id (incf *local-id*)))

(defun char-id-equal (id1 id2)
  (and (= (char-id-site-id id1)
          (char-id-site-id id2))
       (= (char-id-local-id id1)
          (char-id-local-id id2))))

(defun char-id< (id1 id2)
  (and (< (char-id-site-id id1)
          (char-id-site-id id2))
       (or (= (char-id-site-id id1)
              (char-id-site-id id2))
           (< (char-id-local-id id1)
              (char-id-local-id id2)))))

(defclass woot-char ()
  ((id :initarg :id
       :type char-id
       :accessor woot-char-id)
   (visible :initarg :visible
            :type boolean
            :accessor woot-char-visible)
   (value :initarg :value
          :type (or character string)
          :accessor woot-char-value)
   (next :initarg :next
         :type char-id
         :accessor woot-char-next)
   (previous :initarg :previous
             :type char-id
             :accessor woot-char-previous)))

(defmethod print-object ((object woot-char) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (when (slot-boundp object 'id)
      (format stream
              "~A:~A:~A"
              (char-id-site-id (woot-char-id object))
              (char-id-local-id (woot-char-id object))
              (woot-char-value object)))))

(defun woot-char-equal (char1 char2)
  (char-id-equal (woot-char-id char1)
                 (woot-char-id char2)))

(defclass document ()
  ((seq :initarg :sequence
        :accessor document-sequence)
   (start :initarg :start
          :accessor document-start)
   (end :initarg :end
        :accessor document-end)
   (site-id :initarg :site-id
            :accessor document-site-id)))

(defun make-document (site-id)
  (let ((start (make-instance 'woot-char
                              :id (make-char-id :site-id site-id)
                              :value "start"
                              :visible nil))
        (end (make-instance 'woot-char
                            :id (make-char-id :site-id site-id)
                            :value "end"
                            :visible nil)))
    (setf (woot-char-next start) (woot-char-id end)
          (woot-char-previous end) (woot-char-id start))
    (make-instance 'document
                   :sequence (list start end)
                   :start start
                   :end end
                   :site-id site-id)))

(defun nth-visible (sequence position)
  (if (minusp position)
      nil
      (let ((visible-sequence (remove-if-not #'woot-char-visible sequence)))
        (nth position visible-sequence))))

(defun position-by-char-id (sequence char-id)
  (position char-id sequence :key #'woot-char-id :test #'char-id-equal))

(defun sub-sequence (sequence prev next)
  (let ((start-pos (position-by-char-id sequence (woot-char-id prev)))
        (end-pos (position-by-char-id sequence (woot-char-id next))))
    (subseq sequence (1+ start-pos) end-pos)))

(defun local-insert (sequence char position)
  (assert (< 0 position (length sequence)))
  (setf sequence (append (subseq sequence 0 position)
                         (list char)
                         (subseq sequence position)))
  (setf (woot-char-next (elt sequence (1- position))) (woot-char-id char)
        (woot-char-previous (elt sequence (1+ position))) (woot-char-id char))
  sequence)

(defun integrate-insert (sequence char prev next)
  (let ((sub-sequence (sub-sequence sequence prev next))
        (pos (position-by-char-id sequence (woot-char-id next))))
    (cond ((length= sub-sequence 0)
           (local-insert sequence char pos))
          ((length= sub-sequence 1)
           (local-insert sequence char (1- pos)))
          (t
           (let ((i 1))
             (loop :while (and (< i (1- (length sub-sequence)))
                               (char-id< (elt sub-sequence i)
                                         (woot-char-id char)))
                   :do (incf i))
             (integrate-insert sequence
                               char
                               (elt sub-sequence (1- i))
                               (elt sub-sequence i)))))))

(defun insert-char (sequence char)
  (let ((prev (find (woot-char-previous char) sequence :key #'woot-char-id :test #'char-id-equal))
        (next (find (woot-char-next char) sequence :key #'woot-char-id :test #'char-id-equal)))
    (integrate-insert sequence char prev next)))

(defun generate-insert (document position value)
  (let* ((prev (or (nth-visible (document-sequence document) (1- position))
                   (document-start document)))
         (next (or (nth-visible (document-sequence document) position)
                   (document-end document)))
         (char (make-instance 'woot-char
                              :id (make-char-id :site-id (document-site-id document))
                              :visible t
                              :value value
                              :next (woot-char-id next)
                              :previous (woot-char-id prev))))
    (setf (document-sequence document)
          (insert-char (document-sequence document) char))
    char))

(defun delete-char (document char)
  (let* ((sequence (document-sequence document))
         (pos (position-by-char-id sequence (woot-char-id char))))
    (when pos
      (setf (woot-char-visible (elt sequence pos)) nil)))
  (values))

(defun generate-delete (document position)
  (when-let ((char (nth-visible (document-sequence document) position)))
    (setf (woot-char-visible char) nil)
    char))

(defun test-woot ()
  (let ((doc (make-document 1)))
    (let ((char1 (generate-insert doc 0 "a"))
          (char2 (generate-insert doc 1 "b"))
          (char3 (generate-insert doc 2 "c")))
      (assert (equal (mapcar #'woot-char-value (document-sequence doc))
                     '("start" "a" "b" "c" "end")))
      (assert (equal (mapcar #'woot-char-visible (document-sequence doc))
                     '(nil t t t nil)))
      (assert (char-id-equal (woot-char-next (document-start doc)) (woot-char-id char1)))
      (assert (char-id-equal (woot-char-next char1) (woot-char-id char2)))
      (assert (char-id-equal (woot-char-next char2) (woot-char-id char3)))
      (assert (char-id-equal (woot-char-next char3) (woot-char-id (document-end doc))))
      (assert (char-id-equal (woot-char-previous (document-end doc)) (woot-char-id char3)))
      (assert (char-id-equal (woot-char-previous char3) (woot-char-id char2)))
      (assert (char-id-equal (woot-char-previous char2) (woot-char-id char1)))
      (assert (char-id-equal (woot-char-previous char1) (woot-char-id (document-start doc))))
      (let ((char4 (generate-insert doc 1 "d")))
        (assert (equal (mapcar #'woot-char-value (document-sequence doc))
                       '("start" "a" "d" "b" "c" "end")))
        (assert (equal (mapcar #'woot-char-visible (document-sequence doc))
                       '(nil t t t t nil)))
        (assert (char-id-equal (woot-char-next (document-start doc)) (woot-char-id char1)))
        (assert (char-id-equal (woot-char-next char1) (woot-char-id char4)))
        (assert (char-id-equal (woot-char-next char4) (woot-char-id char2)))
        (assert (char-id-equal (woot-char-next char2) (woot-char-id char3)))
        (assert (char-id-equal (woot-char-next char3) (woot-char-id (document-end doc))))))))

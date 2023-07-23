(in-package #:turn-the-rail)

;;; notes

(defun note-file (x)
  (probe-file
   (data-path (format nil "notes/note-~a.ogg" x))))

(defparameter *notes* (make-hash-table))

(defun note (x)
  (gethash x *notes*))

(defun (setf note) (note x)
  (setf (gethash x *notes*) note))

(defun create-note (x)
  (setf (note x) (h:create (note-file x) :mixer :effect :volume 0.2)))

(defun create-notes (&aux (min -21) (max 27))
  (loop for x from min to max
        do (create-note x)))

(defparameter *sfx-mute* NIL)

(defun sfx (&rest notes)
  (unless *sfx-mute*
    (dolist (x notes)
      (a:when-let ((note (note x)))
        (h:play note :reset T)))))

;; soundtrack

(defparameter *soundtrack* NIL)

(defun create-soundtrack ()
  (setf *soundtrack*
        (make-instance 'h:environment :sets `((:normal ,(probe-file (data-path "soundtrack.ogg")))))))

(defparameter *soundtrack-mute* T)

(defun toggle-soundtrack ()
  (if *soundtrack-mute*
      (play-soundtrack)
      (mute-soundtrack)))

(defun play-soundtrack ()
  (when *soundtrack-mute*
    (h:transition *soundtrack* :normal)
    (setf *soundtrack-mute* NIL)))

(defun mute-soundtrack ()
  (unless *soundtrack-mute*
    (h:transition *soundtrack* NIL)
    (setf *soundtrack-mute* T)))

(defun music-init ()
  (unless h:*server*
    (h:maybe-start-simple-server :mixers '((:music m:basic-mixer) (:effect m:basic-mixer))
                                 :name "Turn The Rail")
    (create-notes)
    (create-soundtrack)))

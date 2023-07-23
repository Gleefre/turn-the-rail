(in-package #:turn-the-rail)

;; Camera is 300 x 400

(defparameter +camera-width+ 400)
(defparameter +camera-height+ 300)

(defclass game ()
  ((mode :initform :idle :type (member :idle :game)
         :accessor mode)
   (rocks :initform (sp:queue) :accessor rocks)
   (coins :initform (sp:queue) :accessor coins)
   (life-orbs :initform (sp:queue) :accessor life-orbs)
   (cx :initform   0 :accessor cx)  ;; top left corner
   (cy :initform   0 :accessor cy)
   (x  :initform 100 :accessor x)
   (y  :initform 150 :accessor y)
   (α  :initform   0 :accessor α)
   (score :initform 0 :accessor score)
   (next-score-line :initform 0 :accessor score-line)
   (frame-clock :initarg :frame-clock :accessor frame-clock)))

(defun make-game (clock)
  (make-instance 'game :frame-clock (sc:make-clock :time-source clock)))

(defun update-game ()
  (generate-rocks-and-rolls)
  (move-train)
  (setf (α *game*) (cycle-pos (frame-clock *game*) -30 30 :multiplier 0 :offset (α *game*)))
  (setf (α *game*)
        (a:clamp (α *game*) -30 30)))

;; generating stuff

(defun generate-rocks-and-rolls ()
  (generate-rocks)
  (generate-coins)
  (generate-life-orbs))

(defun lost (item)
  (> (x *game*) (+ 200 (car item))))

(defun clear-lost (queue)
  (loop until (sp:queue-empty-p queue)
        while (lost (sp:front queue))
        do (sp:deq queue)))

(defun lastx (queue)
  (if (sp:queue-empty-p queue)
      (x *game*)
      (car (sp:qback queue))))

(defun generate-rocks (&aux (rocks (rocks *game*)))
  (clear-lost rocks)
  (loop for x from (+ 100 (lastx rocks)) to (+ 500 (x *game*)) by 100
        do (sp:enq (list x (random 300)) rocks)))

(defun generate-coins (&aux (coins (coins *game*)))
  (clear-lost coins)
  (loop for x from (+ 100 (lastx coins)) to (+ 500 (x *game*)) by 100
        do (sp:enq (list x (random 600)) coins)))

(defun generate-life-orbs (&aux (life-orbs (life-orbs *game*)))
  (clear-lost life-orbs)
  (loop for x from (+ 100 (lastx life-orbs)) to (+ 500 (x *game*)) by 100
        do (sp:enq (list x (random 900)) life-orbs)))

;; move the train

(defparameter +train-speed+ 10)

(defun move-train (&aux (dt (pop-time (frame-clock *game*))))
  (s:text (format nil "~,4F" dt) 0 0)
  (incf (x *game*) (* +train-speed+ dt))
  (incf (y *game*) (* +train-speed+ dt
                      (tan (s:radians (α *game*)))))
  (setf (cx *game*) (- (x *game*) 100)
        (cy *game*) 0))

;; controls

(defun pause-game ()
  (sc:pause *game-clock*))

(defun unpause-game ()
  (sc:run *game-clock*))

(defun toggle-game ()
  (sc:toggle *game-clock*))

;; controls

(defun click-on-pause ())

(defun click-on-mute-soundtrack ()
  (toggle-soundtrack))

(defun click-on-mute-sfx ()
  (toggle-sfx))

(defun press-space ()
  (case (mode *game*)
    (:idle (start-game))
    (:game)))

(defun release-space ())

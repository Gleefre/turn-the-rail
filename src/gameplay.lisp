(in-package #:turn-the-rail)

;; Camera is 300 x 400

(defparameter +camera-width+ 400)
(defparameter +camera-height+ 300)

(defparameter +game-speed+ 2)
(defparameter +time-stop-speed+ 1/10)

(defparameter +train-speed+ 100)

;; rock is 40x40
;; coin is 20x20
;; life is 20x20
;; train is 40x30
;; map is 400x300

(defparameter +rock-offset+ 0)
(defparameter +rock-interval+ 90)

(defparameter +coin-offset+ 50)
(defparameter +coin-interval+ 180)

(defparameter +life-offset+ 70)
(defparameter +life-interval+ 270)

;; +1 to score happens every 50 squares
(defparameter +d-score-line+ 50)

(defclass game ()
  ((clock :initarg :clock :accessor game-clock)
   (mode :initform :idle :type (member :idle :game)
         :accessor mode)
   (rocks :initform (sp:queue (list +rock-offset+ -100)) :accessor rocks)
   (coins :initform (sp:queue (list +coin-offset+ -100)) :accessor coins)
   (life-orbs :initform (sp:queue (list +life-offset+ -100)) :accessor life-orbs)
   (cx :initform   0 :accessor cx) ;; top left corner
   (cy :initform   0 :accessor cy)
   (x  :initform 100 :accessor x)  ;; center of train
   (y  :initform 165 :accessor y)  ;; bottom of train, center of the rail
   (α  :initform   0 :accessor α)
   (score :initform 0 :accessor score)
   (next-score-line :initform 0 :accessor score-line)
   (frame-clock :initarg :frame-clock :accessor frame-clock)
   (rotate-clock :initarg :rotate-clock :accessor rotate-clock)
   (animation :initform nil :accessor animation)))

(defun make-game (&aux (clock (sc:make-clock :speed +game-speed+)))
  (make-instance 'game
                 :clock clock
                 :frame-clock (sc:make-clock :time-source clock)
                 :rotate-clock (sc:make-clock :time-source clock :paused t)))

(defun update-game ()
  (generate-rocks-and-rolls)
  (rotate-rails)
  (move-train))

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

;; moving the train

(defun rotate-rails ()
  (setf (α *game*) (cycle-pos (rotate-clock *game*) -20 20 :multiplier 100)))

(defun move-train (&aux (dt (pop-time (frame-clock *game*))))
  (s:text (format nil "~,4F" dt) 0 0)
  (incf (x *game*) (* +train-speed+ dt))
  (incf (y *game*) (* +train-speed+ dt
                      (tan (s:radians (α *game*)))))
  (update-camera)
  (check-collisions))

(defun update-camera ()
  (setf (cx *game*) (- (x *game*) 100)
        (cy *game*) 0))

(defun check-collisions ())

;; controls

(defun pause-game ()
  (sc:pause *game-clock*))

(defun unpause-game ()
  (sc:run *game-clock*))

(defun toggle-game ()
  (sc:toggle *game-clock*))

;; controls

(defun click-on-pause ()
  (sc:toggle *game-clock*))

(defun click-on-mute-soundtrack ()
  (toggle-soundtrack))

(defun click-on-mute-sfx ()
  (toggle-sfx))

(defun start-game ()
  (setf (mode *game*) :game)
  (setf (score-line *game*)
        (+ (x *game*) +d-score-line+)))

(defun press-space ()
  (case (mode *game*)
    (:idle (start-game))
    (:game (setf (sc:speed *game-clock*) +time-stop-speed+
                 (sc:paused (rotate-clock *game*)) nil))))

(defun release-space ()
  (case (mode *game*)
    (:game (setf (sc:speed *game-clock*) +game-speed+
                 (sc:paused (rotate-clock *game*)) t))))

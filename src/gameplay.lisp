(in-package #:turn-the-rail)

;; Camera is 300 x 400

(defparameter +camera-width+ 400)
(defparameter +camera-height+ 300)

(defparameter +game-speed+ 2)
(defparameter +time-stop-speed+ 1/10)

(defparameter +game-rotate-speed+ 50)
(defparameter +max-rotate+ 45)

(defparameter +train-speed+ 50)

;; rock is 40x40
;; coin is 20x20
;; life is 20x20
;; train is 40x30
;; map is 400x300

(defparameter +rock-offset+ 0)
(defparameter +rock-interval+ 90)

(defparameter +coin-offset+ 50)
(defparameter +coin-interval+ 180)
(defparameter +coin-chance+ 1/2)

(defparameter +life-offset+ 70)
(defparameter +life-interval+ 270)
(defparameter +life-chance+ 1/10)

;; +1 to score happens every 50 squares
(defparameter +d-score-line+ 50)

(defclass game ()
  ((main-clock :initarg :main-clock :accessor main-clock)
   (clock :initarg :clock :accessor game-clock)
   (mode :initform :idle :type (member :idle :game :end)
         :accessor mode)
   (rocks :initform (sp:queue (list +rock-offset+ -200)) :accessor rocks)
   (coins :initform (sp:queue (list +coin-offset+ -200)) :accessor coins)
   (life-orbs :initform (sp:queue (list +life-offset+ -200)) :accessor life-orbs)
   (cx :initform   0 :accessor cx) ;; top left corner
   (cy :initform   0 :accessor cy)
   (x  :initform 100 :accessor x)  ;; center of train
   (y  :initform 165 :accessor y)  ;; bottom of train, center of the rail
   (α  :initform   0 :accessor α)
   (score :initform 0 :accessor score)
   (lifes :initform 3 :accessor lifes)
   (next-score-line :initform 0 :accessor score-line)
   (frame-clock :initarg :frame-clock :accessor frame-clock)
   (rotate-clock :initarg :rotate-clock :accessor rotate-clock)
   (animation :initform nil :accessor animation)
   (directions-animation-clock :initform nil :accessor dac)))

(defun copy-into-game (new-game)
  (dolist (slot '(mode rocks coins life-orbs cx cy x y α score lifes next-score-line
                  clock rotate-clock frame-clock directions-animation-clock))
    (setf (slot-value *game* slot)
          (slot-value new-game slot))))

(defun make-game (&aux (mc (sc:make-clock :speed 1))
                       (clock (sc:make-clock :speed +game-speed+ :time-source mc)))
  (make-instance 'game
                 :main-clock mc
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

(defun rock-mode ()
  (cond
    ((eq (mode *game*) :idle) :stand-by)
    ((< (score *game*) 250) :easy)
    ((< (score *game*) 500) :medium)
    ((< (score *game*) 750) :hard)
    ((< (score *game*) 1000) :hell)
    (t :impossible)))

(defun generate-rocks (&aux (rocks (rocks *game*)))
  (clear-lost rocks)
  (loop for x from (+ 100 (lastx rocks)) to (+ 500 (x *game*)) by 100
        do (case (rock-mode)
             (:stand-by
              (sp:enq (list x 80) rocks)
              (sp:enq (list x 240) rocks))
             (:easy
              (sp:enq (list x (if (random-chance 1/2)
                                  (random 300)
                                  -200))
                      rocks))
             (:medium
              (sp:enq (list x (if (random-chance 2/3)
                                  (+ 50 (random 200))
                                  (if (random-chance 1/3)
                                      (a:random-elt (list (random 50)
                                                          (+ 250 (random 50))))
                                      -200)))
                      rocks))
             (:hard (sp:enq (list x (random 300)) rocks))
             (:hell
              (sp:enq (list (- x (random +rock-interval+)) (random 300)) rocks)
              (sp:enq (list x (random 300)) rocks))
             (:impossible
              (sp:enq (list (- x (random +rock-interval+)) (random 300)) rocks)
              (sp:enq (list (- x (random +rock-interval+)) (random 300)) rocks)
              (sp:enq (list x (random 300)) rocks)
              (sp:enq (list x (random 300)) rocks)))))

(defun generate-coins (&aux (coins (coins *game*)))
  (clear-lost coins)
  (loop for x from (+ 100 (lastx coins)) to (+ 500 (x *game*)) by 100
        do (sp:enq (list x (if (and (not (eq (mode *game*) :idle))
                                    (random-chance +coin-chance+))
                               (random 300)
                               -200))
                   coins)))

(defun generate-life-orbs (&aux (life-orbs (life-orbs *game*)))
  (clear-lost life-orbs)
  (loop for x from (+ 100 (lastx life-orbs)) to (+ 500 (x *game*)) by 100
        do (sp:enq (list x (if (and (not (eq (mode *game*) :idle))
                                    (random-chance +life-chance+))
                               (random 300)
                               -200))
                   life-orbs)))

;; moving the train

(defun rotate-rails ()
  (setf (α *game*)
        (cycle-pos (rotate-clock *game*)
                   (- +max-rotate+)
                   +max-rotate+
                   :multiplier (/ +game-rotate-speed+
                                  +time-stop-speed+))))

(defun move-train (&aux (dt (pop-time (frame-clock *game*))))
  (incf (x *game*) (* +train-speed+ dt))
  (incf (y *game*) (* +train-speed+ dt
                      (tan (s:radians (α *game*)))))
  (update-score-line)
  (update-camera)
  (check-collisions))

(defun update-score-line ()
  (unless (eq (mode *game*) :idle)
    (loop while (>= (x *game*) (score-line *game*))
          do (incf (score-line *game*) +d-score-line+)
             (incf (score *game*))
             (sc:accelerate (main-clock *game*) 21/20))))

(defun update-camera ()
  (setf (cx *game*) (- (x *game*) 100)
        (cy *game*) 0))

;; collisions

(defun end-of-world ()
  (start-end-animation))

(defun collides (type x y)
  (case type
    (:rock
     (multiple-value-bind (dx dy) (distance x (+ y 20) (x *game*) (y *game*) (α *game*))
       (and (<= dx 20)
            (<= dy 15))))
    ((:heart :coin)
     (multiple-value-bind (dx dy) (distance x y (x *game*) (y *game*) (α *game*))
       (< (max dx dy) 20)))))

(defun rock-gonna-collide (x y)
  (multiple-value-bind (dx dy) (distance x (+ y 20) (x *game*) (y *game*) (α *game*))
    (declare (ignore dx))
    (<= dy 15)))

(defun check-collisions ()
  (unless (<= 15 (y *game*) 300)
    (end-of-world))
  (mapcar #'collide-with-rock
          (loop for rock in (sp:qlist (rocks *game*))
                when (apply #'collides :rock rock)
                collect rock))
  (mapcar #'collect-life-orb
          (loop for heart in (sp:qlist (life-orbs *game*))
                when (apply #'collides :heart heart)
                collect heart))
  (mapcar #'collect-coin
          (loop for coin in (sp:qlist (coins *game*))
                when (apply #'collides :coin coin)
                collect coin)))

;; collisions results

(defun collect-coin (coin)
  (incf (score *game*) 10)
  (setf (cadr coin) -200)
  (with-shift (a:random-elt '(-5 -2 0 2 5))
    (sfx 0 7)))

(defun collect-life-orb (life-orb)
  (if (>= (lifes *game*) 3)
      (incf (score *game*) 5)
      (incf (lifes *game*)))
  (setf (cadr life-orb) -200)
  (with-shift (a:random-elt '(-5 -2 0 2 5))
    (sfx 0 5)))

(defun collide-with-rock (rock)
  (decf (lifes *game*))
  (if (<= (lifes *game*) 0)
      (end-of-world)
      (setf (cadr rock) -200))
  (with-shift (a:random-elt '(-5 -2 0 2 5))
    (sfx 0 1)))

;; controls

(defun click-on-pause ()
  (sc:toggle *game-clock*))

(defun click-on-mute-soundtrack ()
  (toggle-soundtrack))

(defun click-on-mute-sfx ()
  (toggle-sfx))

(defun start-game ()
  (setf (mode *game*) :game)
  (setf (dac *game*) (sc:make-clock :time-source *game-clock*))
  (setf (score-line *game*)
        (+ (x *game*) +d-score-line+)))

(defun press-space ()
  (case (mode *game*)
    (:idle (start-game))
    (:game
     (setf (sc:speed *game-clock*) +time-stop-speed+
           (sc:paused (rotate-clock *game*)) nil))))

(defun release-space ()
  (case (mode *game*)
    (:game
     (setf (sc:speed *game-clock*) +game-speed+
           (sc:paused (rotate-clock *game*)) t))))

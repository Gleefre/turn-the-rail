(in-package #:turn-the-rail)

(defun draw-game (w h)
  (s:background (s:hex-to-color "#29ADFF"))
  (s+:with-fit (400 300 w h)
    (if (animation *game*)
        (funcall (animation *game*))
        (progn
          (update-game)
          (s+:with-scissor (0 0 400 300)
            (draw-background)
            (draw-world 400 300))))
    (s+:with-color ((s:hex-to-color "#29ADFF"))
      (draw-train-outline 400 300))
    (draw-header 400 300)))

;; World

(defun draw-world (w h)
  (s+:with-fit (400 300 w h)
    (s+:with-scale (1 1 200 150)
      (draw-rail)
      (draw-rocks :up)
      (draw-coins)
      (draw-life-orbs)
      (draw-train)
      (draw-rocks :down))))

(defun draw-background ()
  (s:background (s:hex-to-color "#008751")))

(defparameter +rail-length+ 25)

(defun draw-rail ()
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (s+:with-translate ((x *game*) (y *game*))
      (s+:with-rotate ((α *game*))
        (let ((stage (mod (* +train-speed+ (sc:time *game-clock*)) +rail-length+)))
          (s+:with-color ((s:hex-to-color "#AB5236"))
            (s+:with-translate ((- stage) 0)
              (loop for x from -100 to 100
                    do (s+:with-translate ((* +rail-length+ x) 0)
                         (s:rect -2 -5 4 10))))))
        (s+:with-color ((s:hex-to-color "#C2C3C7"))
          (s:rect -200 -2 800 4))))))

(defun draw-rocks (side)
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (loop for (x y) in (sp:qlist (rocks *game*))
          when (eq side (point-side x (+ y 20) (x *game*) (y *game*) (α *game*)))
          do (s+:with-translate (x y)
               (s+:with-color (s:+black+)
                 (s+:with-color ((s+:filter-alpha (s:hex-to-color "#C2C3C7") 0.5))
                   (with-shear (-20 20)
                     (draw-rock-shape 40 40)))
                 (s+:with-color ((s:hex-to-color "#000000"))
                   (draw-rock-shape 40 40))
                 (s+:with-color ((s:hex-to-color (if (rock-gonna-collide x y)
                                                     "#FF004D"
                                                     "#C2C3C7")))
                   (draw-rock-sign 40 40)))))))

(defun draw-rock-shape (w h)
  (s+:with-fit (5 5 w h 5/2 5/2)
    (s:polygon 0 5 1 3 1 1 3 0 3 2 4 1 5 2 5 5)))

(defun draw-rock-sign (w h)
  (s+:with-fit (10 10 w h 5 5)
    (flet ((draw-!-sign ()
             (s:polygon 0 0 2 0 1 4)
             (s:circle 1 4 1)))
      (s+:with-translate (3 2)
        (draw-!-sign))
      (s+:with-translate (7 4)
        (draw-!-sign)))))

(defun draw-coins ()
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (loop for (x y) in (sp:qlist (coins *game*))
          do (s+:with-translate (x y)
               (s+:with-color ((s+:filter-alpha (s:hex-to-color "#C2C3C7") .5))
                 (draw-coin-shape 15 15))
               (s+:with-translate (0 (- (+ 3/2 (/ (sin (cycle-pos *game-clock* -30 30 :multiplier 5)) 2))))
                 (s+:with-color ((s:hex-to-color "#FFEC27"))
                   (draw-coin-shape 15 15))
                 (s+:with-color ((s:hex-to-color "#FFA300"))
                   (draw-train-shape 10 10)))))))

(defun draw-life-orbs ()
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (loop for (x y) in (sp:qlist (life-orbs *game*))
          do (s+:with-translate (x y)
               (s+:with-color ((s+:filter-alpha (s:hex-to-color "#C2C3C7") .5))
                 (with-shear (0 10)
                   (draw-heart-shape 20 20)))
               (s+:with-color ((s:hex-to-color "#FF004D"))
                 (draw-heart-shape 20 20))
               (s+:with-color ((s:hex-to-color "#FF77A8"))
                 (s+:with-translate (0 -10/12)
                   (draw-train-shape 40/3 10)))))))

(defun draw-train ()
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (s+:with-translate ((x *game*) (y *game*))
      (s+:with-rotate ((α *game*))
        (s+:with-translate (0 -15)
          (s+:with-scale (1 (+ 1 (if (eq :end (mode *game*))
                                     0
                                     (/ (sin (cycle-pos *game-clock* -30 30 :multiplier 2))
                                        10)))
                          0 15)
            (s+:with-color ((s+:filter-alpha (s:hex-to-color "#C2C3C7") .5))
              (with-shear (-20 15)
                (draw-train-shape 40 30)))
            (s+:with-color ((s:hex-to-color "#7E2553"))
              (draw-train-shape 40 30))))))))

(defun draw-train-shape (w h)
  (s+:with-fit (16 12 w h 8 6)
    (s:polygon 0 2 2 0 6 0 8 2 8 5 12 5 11 2 15 2 14 5 15 5 15 6 16 6 16 10 15 10 15 11
               14 12 13 12 12 11 11 12 10 12 9 11 6 11 5 12 4 12 3 11 2 12 1 12 0 11)))

(defun draw-heart-shape (w h)
  (s+:with-fit (6 6 w h 3 3)
    (s:polygon 0 1 1 0 2 0 3 1 4 0 5 0 6 1 6 3 3 6 0 3)))

(defun draw-coin-shape (w h)
  (s+:with-fit (4 4 w h 2 2)
    (s:polygon 0 1 1 0 3 0 4 1 4 3 3 4 1 4 0 3)))

(defun draw-train-outline (w h)
  (s+:with-fit (16 12 w h)
    (s+:with-scissor (0 0 16 12)
      (s:polygon -1 -1 -1 3 2 0 6 0 8 2 8 5 12 5 11 2 15 2 14 5 15 5 15 6 17 6 17 -1)
      (s:polygon -1 13 -1 10 1 12 2 12 3 11 4 12 5 12 6 11 9 11 10
                 12 11 12 12 11 13 12 14 12 15 11 15 10 17 10 17 13))))


(defmacro animating ((var &rest clock-args &key &allow-other-keys) &body body)
  `(let ((animating-clock (sc:make-clock ,@clock-args)))
     (setf (animation *game*)
           (lambda (&aux (,var (sc:time animating-clock)))
             ,@body))))

(defmacro animating-stop ()
  `(setf (animation *game*) nil))

(defun start-end-animation ()
  (sc:stop (main-clock *game*))
  (sc:stop *game-clock*)
  (setf (mode *game*) :end)
  (let* ((new-game (make-game))
         (cx (cx *game*))
         (cy (cy *game*)))
    (animating (v :speed 1/3)
      (setf (cx *game*) (a:lerp (ease:out-circ v) cx (- (x *game*) 200))
            (cy *game*) (a:lerp (ease:out-circ v) cy (- (y *game*) 300)))
      (sp:deq (rocks *game*))
      (when (> v 1)
        (when (> v 1.1)
          (animating (v :speed 1/2)
            (when (> v 1)
              (progn
                (copy-into-game new-game)
                (animating-stop)))
            (s+:with-scissor (0 0 400 300)
              (let ((*game* new-game)
                    (*game-clock* (game-clock new-game)))
                (update-game)
                (draw-background)
                (draw-world 400 300)
                (draw-directions))
              (s+:with-color ((s+:filter-alpha (s:hex-to-color "#7E2553")
                                               (- 1 v)))
                (s:rect 0 0 400 300))))))
      (s+:with-scissor (0 0 400 300)
        (s+:with-rotate ((* (ease:out-quint v) (- (α *game*))) 200 300)
          (draw-background)
          (s+:with-scale ((a:lerp (ease:in-quad v) 1 10) (a:lerp (ease:in-quad v) 1 10) 200 300)
            (draw-world 400 300)))))))

;; Header

(defun draw-header (w h)
  (s+:with-fit (800 600 w h)
    (draw-score)
    (draw-lifes)
    (if (eq (mode *game*) :idle)
        (draw-directions)
        (animate-directions))
    (draw-buttons)))

(defun draw-score ()
  (s:with-font (s:make-font :size 50 :align :right :color (s:hex-to-color "#1D2B53"))
    (s:text (format nil "Score: ~3,'0D" (score *game*))
            800 0)))

(defun draw-lifes ()
  (s+:with-fit (16 12 800 600)
    (loop for x in `(,(+ 11 2/3) 13 ,(- 15 2/3))
          for i from 1
          do (s:with-pen (if (<= i (lifes *game*))
                             (s:make-pen :fill (s:hex-to-color "#FF77A8"))
                             (s:make-pen :stroke (s:hex-to-color "#FF77A8")))
               (s+:with-translate (x 3/2)
                 (draw-heart-shape 1 1))))))

(defun draw-directions ()
  (s:with-font (s:make-font :size 40 :align :center :color (s:hex-to-color "#1D2B53"))
    (s:text "Press SPACE to start.
Use SPACE to turn the rain." 400 (cycle-pos *game-clock* 325 335 :multiplier 5))))

(defun animate-directions ()
  (when (dac *game*)
    (let ((v (/ (sc:time (dac *game*)) 3)))
      (if (> v 1)
          (setf (dac *game*) nil)
          (s+:with-scale ((cos (* 1/2 pi v)) (cos (* 1/2 pi v)) 400 350)
            (s:with-font (s:make-font :size 40 :align :center :color (s:hex-to-color "#1D2B53"))
              (s:text "Press SPACE to start.
Use SPACE to turn the rail." 400 (cycle-pos *game-clock* 325 335 :multiplier 5))))))))

(defmacro named-button ((x y w h name &optional (dy 1/3)) (&optional (when :press)) &body press-body)
  `(progn
     (s:rect ,x ,y ,w ,h)
     (s:text ,name (+ ,x (/ ,w 2)) (+ ,y (* ,h ,dy)))
     (sb:binds (sb:brect ,x ,y ,w ,h)
       ,when (lambda (b)
               (declare (ignorable b))
               ,@press-body))))

(defun draw-buttons ()
  (let ((w 200) (h 200))
    (s+:with-fit (w h 150 150 0 0 400 100)
      (s+:with-color ((s:hex-to-color "#5F574F"))
        (s:with-font (s:make-font :size 25 :align :center :color (s:hex-to-color "#1D2B53"))
          (s+:with-split (w h :vertical)
            (1 (named-button (10 10 (- w 20) (- h 20) "Quit [ESC]" 1/4) ()
                 (kit.sdl2:close-window *game-window*)))
            (1 (named-button (30 10 (- w 20) (- h 20) "Mute [M/F]" 1/4) ()
                 (click-on-mute-sfx)
                 (click-on-mute-soundtrack)))
            (1 (named-button (50 10 (- w 20) (- h 20) "Pause [P]" 1/4) ()
                 (click-on-pause)))))))))

(defun draw-pause (w h)
  (s:with-font (s:make-font :size 30 :align :center :color (s:hex-to-color "#1D2B53"))
    (s:text "||" (/ w 2) 0 40 40)))

(defun draw-menu (w h))

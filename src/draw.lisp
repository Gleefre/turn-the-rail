(in-package #:turn-the-rail)

(defun draw-game (w h)
  (update-game)
  (s+:with-split (w h :vertical)
    (1 (draw-header w h))
    (6 (draw-world w h))))

;; World

(defun draw-world (w h)
  (s+:with-color ((s:hex-to-color "#29ADFF"))
    (s:rect 0 0 2 h))
  (s+:with-fit (400 300 w h)
    (s+:with-scissor (0 0 400 300)
      (s+:with-scale (1 1 200 150)
        (draw-background)
        (draw-rail)
        (draw-rocks)
        (draw-coins)
        (draw-life-orbs)
        (draw-train))
      (s+:with-color ((s:hex-to-color "#29ADFF"))
        (draw-train-outline 400 300)))))

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
                         (s:rect -2 10 4 10))))))
        (s+:with-color ((s:hex-to-color "#C2C3C7"))
          (s:rect -200 13 800 4))
        #+nil
        (s:line -500 0 500 0)))))

(defun draw-rocks ()
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (loop for (x y) in (sp:qlist (rocks *game*))
          do (s+:with-translate (x y)
               (s+:with-color (s:+black+)
                 (s:rect 0 0 10 10))))))

(defun draw-coins ()
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (loop for (x y) in (sp:qlist (coins *game*))
          do (s+:with-translate (x y)
               (s+:with-color ((s+:filter-alpha (s:hex-to-color "#C2C3C7") .5))
                 (draw-coin-shape 15 15))
               (s+:with-translate (0 (- (1+ (/ (sin (cycle-pos *game-clock* -30 30 :multiplier 5)) 2))))
                 (s+:with-color ((s:hex-to-color "#FFEC27"))
                   (draw-coin-shape 15 15))
                 (s+:with-color ((s:hex-to-color "#FFA300"))
                   (draw-train-shape 10 10)))))))

(defun draw-life-orbs ()
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (loop for (x y) in (sp:qlist (life-orbs *game*))
          do (s+:with-translate (x y)
               (s+:with-color ((s+:filter-alpha (s:hex-to-color "#C2C3C7") .5))
                 (s+:with-translate (0 10)
                   (s::set-matrix* #.(sb-cga::matrix 1f0 -0.5f0 0f0 0f0
                                                     0f0 .75f0 0f0 0f0
                                                     0f0 0f0 1f0 0f0
                                                     0f0 0f0 0f0 1f0))
                   (s+:with-translate (0 -10)
                     (draw-heart-shape 20 20))))
               (s+:with-color ((s:hex-to-color "#FF004D"))
                 (draw-heart-shape 20 20))
               (s+:with-color ((s:hex-to-color "#FF77A8"))
                 (s+:with-translate (0 -10/12)
                   (draw-train-shape 40/3 10)))))))

(defun draw-train ()
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (s+:with-translate ((x *game*) (y *game*))
      (s+:with-rotate ((α *game*))
        (s+:with-scale (1 (+ 1 (/ (sin (cycle-pos *game-clock* -30 30 :multiplier 2))
                                  10))
                        0 15)
          (s+:with-color ((s+:filter-alpha (s:hex-to-color "#C2C3C7") .5))
            (s+:with-translate (-20 15)
              (s::set-matrix* #.(sb-cga::matrix 1f0 -0.75f0 0f0 0f0
                                                0f0 0.75f0 0f0 0f0
                                                0f0 0f0 1f0 0f0
                                                0f0 0f0 0f0 1f0))
              (s+:with-translate (20 -15)
                (draw-train-shape 40 30))))
          (s+:with-color ((s:hex-to-color "#7E2553"))
            (draw-train-shape 40 30)))))))

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

;; Header

(defun draw-header (w h)
  (s+:with-color ((s:hex-to-color "#FFF1E8"))
    (s:rect 0 0 w h))
  (s+:with-split (w h :horizontal)
    (2 (draw-score w h))
    (1 (draw-pause w h))
    (2 (draw-menu w h))))

(defun draw-score (w h)
  (s:with-font (s:make-font :size 30 :align :left :color (s:hex-to-color "#1D2B53"))
    (s:text (format nil "Score: ~A" (score *game*)) 0 0)))

(defun draw-pause (w h)
  (s:with-font (s:make-font :size 30 :align :center :color (s:hex-to-color "#1D2B53"))
    (s:text "||" (/ w 2) 0 40 40)))

(defun draw-debug (w h)
  (s:text (format nil "~,0F ~,0F -- ~,0F ~,0F~%angle: ~$"
                  (x *game*) (y *game*)
                  (cx *game*) (cy *game*)
                  (α *game*))
          0 0))

(defun draw-menu (w h))

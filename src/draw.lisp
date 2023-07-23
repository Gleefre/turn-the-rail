(in-package #:turn-the-rail)

(defun draw-game (w h)
  (update-game)
  (s+:with-split (w h :vertical)
    (1 (draw-header w h))
    (6 (draw-world w h))))

;; World

(defun draw-world (w h)
  (s+:with-fit (400 300 w h)
    (s+:with-scissor (0 0 400 300)
      (draw-background)
      (draw-rail)
      (draw-rocks)
      (draw-coins)
      (draw-life-orbs)
      (draw-train))))

(defun draw-background ()
  (s:background (s:hex-to-color "#AB5236")))

(defun draw-rail ()
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (s+:with-translate ((x *game*) (y *game*))
      (s+:with-rotate ((α *game*))
        (s:line -500 0 500 0)
        (sc:with-freeze *game-clock*
          (loop for x from -100 to 100
                do (s+:with-translate ((* 60 x) 0)
                     (s+:with-translate ((- (mod (* +train-speed+ (sc:time *game-clock*)) 60)) 0)
                       (s+:with-color (s:+magenta+ :stroke)
                         (s:line 0 0 20 0))))))))))

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
               (s+:with-color (s:+yellow+)
                 (s:rect 0 0 10 10))))))

(defun draw-life-orbs ()
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (loop for (x y) in (sp:qlist (life-orbs *game*))
          do (s+:with-translate (x y)
               (s+:with-color (s:+red+)
                 (s:rect 0 0 10 10))))))

(defun draw-train ()
  (s+:with-translate ((- (cx *game*)) (- (cy *game*)))
    (s+:with-translate ((x *game*) (y *game*))
      (s+:with-rotate ((α *game*))
        (s+:with-scale (1 (+ 1 (/ (sin (cycle-pos *game-clock* -30 30 :multiplier 2))
                                  10)))
          (s:circle -15 0 5)
          (s:circle 15 0 5)
          (s:rect -20 -25 40 20))))))

;; Header

(defun draw-header (w h)
  (s+:with-split (w h :horizontal)
    (2 (draw-score w h))
    (1 (draw-pause w h))
    (2 (draw-menu w h))))

(defun draw-score (w h))
(defun draw-pause (w h)
  (s:text (format nil "~,0F ~,0F -- ~,0F ~,0F~%angle: ~$"
                  (x *game*) (y *game*)
                  (cx *game*) (cy *game*)
                  (α *game*))
          0 0))
(defun draw-menu (w h))

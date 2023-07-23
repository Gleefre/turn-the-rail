(in-package #:turn-the-rail)

(defun draw-game (w h)
  (s+:with-fit (200 200 w h)
    (s:rect 10 10 180 180)
    (s:with-font (s:make-font :size 50 :align :center)
      (s:text "TTR" 100 50))))

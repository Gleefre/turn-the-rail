(in-package #:turn-the-rail)

(s:defsketch game-window ((s:title "Turn the rail")
                          (clock (sc:make-clock))
                          (game (make-game clock)))
  (let ((*game* game)
        (*game-clock* clock)
        (*game-window* s::*sketch*))
    (draw-game s:width s:height)))

(s:define-start-function (start) game-window (:resizable t :width 800 :height 600)
  (:start (music-init))
  (:setup (_) (play-soundtrack))
  (:on-close (_) (mute-soundtrack)))

(defmethod kit.sdl2:mousebutton-event :around ((window game-window) state ts button x y)
  (let ((*game* (game-window-game window))
        (*game-clock* (game-window-clock window))
        (*game-window* window))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event :around ((window game-window) state ts rep? keysym)
  (let ((*game* (game-window-game window))
        (*game-clock* (game-window-clock window))
        (*game-window* window))
    (call-next-method)))

(defmethod kit.sdl2:keyboard-event ((window game-window) state ts rep? keysym)
  (unless rep?
    (case (sdl2:scancode keysym)
      (:scancode-space
       (case state
         (:keyup (release-space))
         (:keydown (press-space))))
      (:scancode-m
       (when (eq state :keydown)
         (click-on-mute-soundtrack)))
      (:scancode-f
       (when (eq state :keydown)
         (click-on-mute-sfx))))))

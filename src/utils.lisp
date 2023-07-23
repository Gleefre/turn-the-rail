(in-package #:turn-the-rail)

;; data-path to get resource's path
(defparameter *data-location* "res/")

(let ((data-folder nil))
  (defun data-path (relative-path)
    (setf data-folder
          (or data-folder
              (if (member :deploy *features*)
                  (let ((deploy:*data-location* *data-location*))
                    (deploy:data-directory))
                  (asdf:system-relative-pathname "turn-the-rail" *data-location*))))
    (format nil "~a" (merge-pathnames relative-path data-folder))))

(defun pic (name)
  (uiop:native-namestring (merge-pathnames name (data-path "icons/"))))

(let ((font))
  (defun s::make-default-font ()
    (setf font (or font
                   (s:make-font :face (s:load-resource (data-path "font/PromptFont.ttf"))
                                :color s:+black+
                                :size 18)))))

(let ((font))
  (defun s::make-error-font ()
    (setf font (or font
                   (s:make-font :face (s:load-resource (data-path "font/PromptFont.ttf"))
                                :color s:+black+
                                :size 16)))))

(defun pop-time (clock)
  (sc:with-freeze clock
    (prog1 (sc:time clock)
      (setf (sc:time clock) 0))))

(defun cycle-pos (clock start end &key (multiplier 1)
                                       (offset 0)
                  &aux (dt (- end start)))
  (+ start
     (- dt
        (abs (- (mod (+ (- offset start) (* multiplier (sc:time clock))) (* 2 dt))
                dt)))))

(defmacro with-shear ((cx cy) &body body
                      &aux ($cx (gensym "CX"))
                           ($cy (gensym "CY")))
  `(let ((,$cx ,cx)
         (,$cy ,cy))
     (s+:with-translate (,$cx ,$cy)
       (s::set-matrix* #.(sb-cga::matrix 1f0 -0.5f0 0f0 0f0
                                         0f0 .75f0 0f0 0f0
                                         0f0 0f0 1f0 0f0
                                         0f0 0f0 0f0 1f0))
       (s+:with-translate ((- ,$cx) (- ,$cy))
         ,@body))))

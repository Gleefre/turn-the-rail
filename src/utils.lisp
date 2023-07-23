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

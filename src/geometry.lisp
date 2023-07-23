(in-package #:turn-the-rail)

(defun point-side (point-x point-y line-x line-y line-α)
  (if (< (- point-y line-y)
         (* (tan (s:radians line-α))
            (- point-x line-x)))
      :up
      :down))

(defun distance (point-x point-y line-x line-y line-α)
  (values (abs (- point-x line-x))
          (abs (- point-y
                  (+ line-y
                     (* (tan (s:radians line-α))
                        (- point-x line-x)))))))

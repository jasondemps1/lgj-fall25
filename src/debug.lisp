(in-package :lgj-fall25)

(defparameter *frame-times* nil)
(defparameter *frame-count* 0)

(defun get-fps ()
  "Calculate current FPS"
  (if (null *frame-times*)
      0
      (let ((avg-frame-time (/ (reduce #'+ *frame-times*)
                               (length *frame-times*))))
        (if (zerop avg-frame-time)
            0
            (floor (/ 1.0 avg-frame-time))))))

(defun update-fps (delta-time)
  "Update FPS Calculation"
  (push delta-time *frame-times*)
  (when (> (length *frame-times*) 60)
    (setf *frame-times* (subseq *frame-times* 0 60))))

(defun render-debug-info (renderer)
  (when *debug-font*
    ;; FPS Counter
    (render-text renderer (format nil "FPS: ~A" (get-fps)) *debug-font* 10 10 :r 255 :g 255 :b 0)))



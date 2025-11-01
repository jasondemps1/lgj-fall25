(in-package :lgj-fall25)

(defcomponent size
  "Size Component"
  (width 0 :type integer)
  (height 0 :type integer))

(defcomponent image
  "Image Component"
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer))

(defsystem render-images
  (:components-ro (position image)))

(defcomponent character
  "Character Component"
  (speed 0.0 :type single-float)
  (target-x single-float-nan :type single-float)
  (target-y single-float-nan :type single-float))

(defcomponent player
  "Player - Character - Tag"
  (player 1 :type bit :index player-entity :unique t))

(defun load-player ()
  (make-object `((:position :x 64.0 :y 64.0)
                 (:size :width 32 :height 32)
                 (:character speed 100.0)
                 (:player))))

(defsystem control-player
  (:components-ro (player position size)
   :components-rw (character)))

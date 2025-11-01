(in-package :lgj-fall25)

(defcomponent position
  "Location Information"
  (x 0.0 :type single-float :documentation "X Coordinate")
  (y 0.0 :type single-float :documentation "Y Coordinate"))

(defcomponent velocity
  "Velocity Information"
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))

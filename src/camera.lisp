(in-package :lgj-fall25)

(defparameter *camera-x* 0.0)
(defparameter *camera-y* 0.0)

(defun update-camera ()
  "Center on player"
  (when *player*
    (setf *camera-x* (- (entity-x *player*) (/ *screen-width* 2))
          *camera-y* (- (entity-x *player*) (/ *screen-width* 2))))

  ;; Lets clamp to map bounds for now
  (when *tilemap*
    (let ((map-width (* (tilemap-map-width *tilemap*)
                        (tilemap-tile-width *tilemap*)))
          (map-height (* (tilemap-map-height *tilemap*)
                         (tilemap-tile-height *tilemap*))))
      (setf *camera-x* (max 0 (min *camera-x* (- map-width *screen-width*)))) 
      (setf *camera-y* (max 0 (min *camera-y* (- map-height *screen-height*))))))) 

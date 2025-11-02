(in-package :lgj-fall25)

;; TODO - Move texture functions to their own file probably
(defun load-texture (renderer filepath)
  "Load a texture from a file and cache it"
  (or (gethash filepath *textures*)
      (handler-case
          (let* ((surface (sdl2-image:load-image filepath))
                 (texture (sdl2:create-texture-from-surface renderer surface)))
            (sdl2:free-surface surface)
            (setf (gethash filepath *textures*) texture)
            texture)
        (error (e)
          (format t "Failed to load texture ~A: ~A~%" filepath e)
          nil))))

(defun free-textures ()
  "Free all cached textures"
  (maphash (lambda (key texture)
             (declare (ignore key))
             (sdl2:destroy-texture texture))
           *textures*)
  (clrhash *textures*))

(defclass entity ()
  ((x :initarg :x :initform 0.0 :accessor entity-x)
   (y :initarg :y :initform 0.0 :accessor entity-y)
   (width :initarg :width :initform 32 :accessor entity-width)
   (height :initarg :height :initform 32 :accessor entity-height)))

(defclass player (entity)
  ((velocity-x :initform 0.0 :accessor player-vx)
   (velocity-y :initform 0.0 :accessor player-vy)
   (speed :initarg :speed :initform 2 :accessor player-speed)
   (texture :initarg :texture :initform nil :accessor player-texture)))

;; Generics
(defgeneric update (entity delta-time keys-pressed))
(defgeneric render (entity renderer))

;; Player update - handle input and movement
(defmethod update ((p player) delta-time keys-pressed)
  (setf (player-vx p) 0.0
        (player-vy p) 0.0)

  ;; Check keys and set velocity
  (when (gethash (sdl2:scancode-key-to-value :scancode-w) keys-pressed)
    (format t "Player sees W pressed~%")
    (decf (player-vy p) (player-speed p)))
  (when (gethash (sdl2:scancode-key-to-value :scancode-s) keys-pressed)
    (format t "Player sees S pressed~%")
    (incf (player-vy p) (player-speed p)))
  (when (gethash (sdl2:scancode-key-to-value :scancode-a) keys-pressed)
    (format t "Player sees A pressed~%")
    (decf (player-vx p) (player-speed p)))
  (when (gethash (sdl2:scancode-key-to-value :scancode-d) keys-pressed)
    (format t "Player sees D pressed~%")
    (incf (player-vx p) (player-speed p)))

  ;; Apply velocity to position
  (incf (entity-x p) (* (player-vx p) delta-time))
  (incf (entity-y p) (* (player-vy p) delta-time)))

;; Player Render
(defmethod render ((p player) renderer)
  (if (player-texture p)
      ;; Render texture if available
      (let ((dest-rect (sdl2:make-rect (floor (entity-x p))
                                       (floor (entity-y p))
                                       (entity-width p)
                                       (entity-height p))))
        (sdl2:render-copy renderer (player-texture p) :dest-rect dest-rect))
      ;; Fallback to colored rectangle
      (progn
        (sdl2:set-render-draw-color renderer 0 255 0 255)
        (sdl2:render-fill-rect renderer
                               (sdl2:make-rect (floor (entity-x p))
                                               (floor (entity-y p))
                                               (entity-width p)
                                               (entity-height p))))))

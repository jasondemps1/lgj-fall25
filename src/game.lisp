(in-package :lgj-fall25)

;;(defparameter *world* nil)
(defparameter *entities* nil)
(defparameter *player* nil)
(defparameter *keys-pressed* nil)
(defparameter *textures* (make-hash-table :test 'equal))

(defclass entity ()
  ((x :initarg :x :initform 0.0 :accessor entity-x)
   (y :initarg :y :initform 0.0 :accessor entity-y)
   (width :initarg :width :initform 32 :accessor entity-width)
   (height :initarg :height :initform 32 :accessor entity-height)))

(defclass player (entity)
  ((velocity-x :initform 0.0 :accessor player-vx)
   (velocity-y :initform 0.0 :accessor player-vy)
   (speed :initarg :speed :initform 100 :accessor player-speed)
   (texture :initarg :texture :initform nil :accessor player-texture)))

;; Generics
(defgeneric update (entity delta-time keys-pressed))
(defgeneric render (entity renderer))

;; Player update - handle input and movement
(defmethod update ((p player) delta-time keys-pressed)
  (setf (player-vx p) 0.0
        (player-vy p) 0.0)

  ;; Check keys and set velocity
  (when (gethash :scancode-w keys-pressed)
    (format t "Player sees W pressed")
    (decf (player-vy p) (player-speed p)))
  (when (gethash :scancode-s keys-pressed)
    (incf (player-vy p) (player-speed p)))
  (when (gethash :scancode-a keys-pressed)
    (decf (player-vx p) (player-speed p)))
  (when (gethash :scancode-d keys-pressed)
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


(defun init-game (renderer)
  (setf *entities* nil)
  (setf *keys-pressed* (make-hash-table))

  ;; Create Player
  (let ((player-texture (load-texture renderer "assets/player.png")))
    (setf *player* (make-instance 'player
                                  :x 400.0
                                  :y 300.0
                                  :width 32
                                  :height 32
                                  :speed 200.0
                                  :texture player-texture))
    (push *player* *entities*))

  (format t "Game initialized with ~A entities~%" (length *entities*)))

(defun update-game (delta-time)
  (dolist (entity *entities*)
    (update entity delta-time *keys-pressed*)))

(defun render-game (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)

  (dolist (entity *entities*)
    (render entity renderer))

  (sdl2:render-present renderer))


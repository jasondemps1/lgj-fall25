(in-package :lgj-fall25)

(defparameter *debug-font* nil)
(defparameter *entities* nil)
(defparameter *player* nil)
(defparameter *keys-pressed* nil)
(defparameter *textures* (make-hash-table :test 'equal))

(defparameter *tilemap* nil)

(defun init-game (renderer)
  (setf *entities* nil)
  (setf *keys-pressed* (make-hash-table))

  ;; Load tilemap
  (setf *tilemap* (load-tilemap renderer "assets/level1.tmx"))

  ;; Load our font
  (setf *debug-font* (load-font "assets/test-font.ttf" 9))

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
    (update entity delta-time *keys-pressed*))
  (update-camera))

(defun render-game (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)

  ;; Tilemap layers (for now, we're just hardcoding some layers, smarter later)
  (when *tilemap*
    ;; bkg
    (render-tilemap-layer *tilemap* renderer "background" :camera-x *camera-x* :camera-y *camera-y*)
    ;; ground
    (render-tilemap-layer *tilemap* renderer "ground" :camera-x *camera-x* :camera-y *camera-y*)
    ;; spawners
    (render-tilemap-layer *tilemap* renderer "spawners" :camera-x *camera-x* :camera-y *camera-y*))

  ;; Entities
  (dolist (entity *entities*)
    (render entity renderer))

  ;; Any debug info we need (toggle this somehow)
  (render-debug-info renderer)

  (sdl2:render-present renderer))


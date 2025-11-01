(in-package :lgj-fall25)

;; COMPONENTS

(defcomponent player
  "Player - Character - Tag"
  (player 1 :type bit :index player-entity :unique t))

(defun load-player ()
  (make-object ((:size :width 32.0 :height 32.0)
                (:position :x 64.0 :y 64.0)
                (:character :speed 100.0)
                (:rect-shape)
                (:player))))
;;(make-object `(;;(:shape )
                 ;;;;(:image :surface ,(sdl2-image:load-image ))
;;(:position :x 64.0 :y 64.0)
;;(:size :width 32 :height 32)
;;(:character speed 100.0)
;;(:player))

;; SYSTEMS

(defsystem player-input-system
  (:components-ro (player position)
   :components-rw (character))
  (when (gethash :scancode-w *keys-pressed*)
    (setf position-y (decf position-y 10)))
  (when (gethash :scancode-s *keys-pressed*)
    (setf position-y (incf position-y 10))))

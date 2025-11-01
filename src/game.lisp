(in-package :lgj-fall25)

;;(defparameter *world* nil)
(defparameter *player-entity* nil)
(defparameter *keys-pressed* nil)

(defun init-game ()
  "Init the ECS world and create entities"
  (make-storage)
  (setf *keys-pressed* (make-hash-table))

  (setf *player-entity* (load-player)))
;;(setf *world*))

(defun update-game ()
  (run-systems))

(defun render-game (renderer)
  )

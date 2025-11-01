(in-package :lgj-fall25)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *is-running* t)
(defparameter *game-thread* nil)

;; Setup a basic ECS test thing here - move it eventually

(defsystem move
  (:components-ro (velocity)
   :components-rw (position))
  "Moves objects according to their velocity"
  (incf position-x velocity-x)
  (incf position-y velocity-y))

(defsystem print
  (:components-ro (position))
  (format t "entity ~a: (~a, ~a)~%" entity position-x position-y))

(defun start ()
  (when (and *game-thread* (bt2:thread-alive-p *game-thread*))
    (format t "Game already running!~%")
    (return-from start nil))

  (setf *game-thread*
        (bt2:make-thread #'game-main :name "game-main"))
  (format t "Game started in thread ~A~%" *game-thread*)
  *game-thread*)

(defun stop ()
  "Stop the game and clean up any threads"
  (format t "Stopping game...~%")

  ;; Wait for thread to finish (with timeout of course)
  (stop-thread *game-thread*)
  (stop-thread-by-name "SDL2 Main Thread")
  (setf *game-thread* nil)
  (format t "Game stopped.~%")
  t)

(defun init-ecs ()
  (make-storage)
  (load-player))

(defun game-main ()
  "Main entry point"
  (init-ecs)
  (handler-case
      (sdl2:with-init (:video)
        (sdl2:with-window (window :title "LGJ Fall25"
                                  :w *screen-width*
                                  :h *screen-height*
                                  :flags '(:shown))
          (sdl2:with-renderer (renderer window :flags '(:accelerated))
            (game-loop renderer))))
    (error (e)
      (format t "Error in game thread: ~A~%" e)
      (stop))))

(defun calculate-delta-time (last-time)
  (let* ((current-time (/ (sdl2:get-ticks) 1000.0))
         (delta-time (- current-time last-time)))
    (values last-time delta-time)))

(defun game-loop (renderer)
  (let ((last-frame-time (/ (sdl2:get-ticks) 1000.0)))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
             (format t "Quit event received~%")
             t)
      (:keydown (:keysym keysym)
                (let ((scancode (sdl2:scancode-value keysym)))
                  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                    (format t "Escape pressed, stopping game~%"))
                  ;; Track key pressed
                  (setf (gethash scancode *keys-pressed*) t)))
      (:keyup (:keysym keysym)
              ;; Track key released
              (setf (gethash (sdl2:scancode-value keysym) *keys-pressed*) nil))
      (:idle ()
             (multiple-value-bind (last-frame-time delta-time)
                 (calculate-delta-time last-frame-time)
               (update delta-time)
               (render renderer)
               (sdl2:delay 16)))))
  (format t "Left the game-loop")
  (stop))

(defun update (dt)
  "Update game state stuff"
  (run-systems :dt (float dt 0.0)))

(defun render (renderer)
  "Render the game"
  ;; Clear screen to black
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)

  ;; Draw dat rect
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 100 100 50 50))

  ;; Present the frame
  (sdl2:render-present renderer))

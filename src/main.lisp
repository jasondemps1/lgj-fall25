(in-package :lgj-fall25)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *is-running* t)
(defparameter *game-thread* nil)

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
  (setf *game-thread* nil)
  (format t "Game stopped.~%")
  t)

(defun game-main ()
  "Main entry point"
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title "LGJ Fall25"
                              :w *screen-width*
                              :h *screen-height*
                              :flags '(:shown))
      (sdl2:with-renderer (renderer window :flags '(:accelerated))
        (game-loop renderer)))))

(defun game-loop (renderer)
  (sdl2:with-event-loop (:method :poll)
    (:quit ()
           (format t "Quit event received~%")
           t)
    (:keydown (:keysym keysym)
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                (format t "Escape pressed, stopping game~%")))
    (:idle ()
           (update)
           (render renderer)
           (sdl2:delay 16)))
  (format t "Left the game-loop")
  (stop))

(defun update ()
  "Update game state stuff"
  nil)

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

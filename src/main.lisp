(defpackage :lgj-fall25
  (:use :cl)
  (:export #:main))

(in-package :lgj-fall25)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *is-running* t)

(defun main ()
  "Main entry point"
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title "LGJ Fall25"
                              :w *screen-width*
                              :h *screen-height*
                              :flags '(:shown))
      (sdl2:with-renderer (renderer window :flags '(:accelerated))
        (game-loop renderer)))))

(defun game-loop (renderer)
  "Game Loop"
  (setf *is-running* t)
  (loop while *running*
        do (handle-events)
           (update)
           (render renderer)
           (sdl2:delay 16)))

(defun handle-events ())

(defun update ())

(defun render (renderer))

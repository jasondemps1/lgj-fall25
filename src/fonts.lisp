(in-package :lgj-fall25)

(defparameter *fonts* (make-hash-table :test 'equal))

(defun load-font (filepath size)
  "Load a font from a file and cache it"
  (let ((key (format nil "~A-~A" filepath size)))
    (or (gethash key *fonts*)
        (let ((font (sdl2-ttf:open-font filepath size)))
          (setf (gethash key *fonts*) font)
          font))))

(defun free-fonts ()
  "Free all cached fonts"
  (maphash (lambda (key font)
             (declare (ignore key))
             (sdl2-ttf:close-font font))
           *fonts*)
  (clrhash *fonts*))

(defun render-text (renderer text font x y &key (r 255) (g 255) (b 255) (a 255))
  "Render text at a given position"
  (let* ((surface (sdl2-ttf:render-text-blended font text r g b a))
         (texture (sdl2:create-texture-from-surface renderer surface))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface)))
    (sdl2:render-copy renderer texture :dest-rect (sdl2:make-rect x y width height))
    (sdl2:destroy-texture texture)
    (sdl2:free-surface surface)))

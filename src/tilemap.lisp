(in-package :lgj-fall25)

(defparameter *tilemap-textures* (make-hash-table))

(defclass tilemap ()
  ((tiled-map :initarg :tiled-map :accessor tilemap-tiled-map)
   (tileset-textures :initform (make-hash-table) :accessor tilemap-textures)
   (tile-width :initarg :tile-width :accessor tilemap-tile-width)
   (tile-height :initarg :tile-height :accessor tilemap-tile-height)
   (map-width :initarg :map-width :accessor tilemap-map-width)
   (map-height :initarg :map-height :accessor tilemap-map-height)))

(defun load-tilemap (renderer tmx-filepath)
  "Load a Tiled TMX map and create textures for tilesets"
  (let* ((tiled-map (cl-tiled:load-map tmx-filepath))
         (tm (make-instance 'tilemap
                            :tiled-map tiled-map
                            :tile-width (cl-tiled:map-tile-width tiled-map)
                            :tile-height (cl-tiled:map-tile-height tiled-map)
                            :map-width (cl-tiled:map-width tiled-map)
                            :map-height (cl-tiled:map-height tiled-map))))
    ;; Load textures for each tileset
    (dolist (tileset (cl-tiled:map-tilesets tiled-map))
      (let* ((image-source (cl-tiled:tileset-image tileset))
             ;; Resolve relative path from TMX file location
             (image-path (merge-pathnames image-source (uiop:pathname-directory-pathname tmx-filepath)))
             (texture (load-texture renderer (namestring image-path))))
        (when texture
          (setf (gethash (cl-tiled:tileset-name tileset) (tilemap-textures tm))
                texture)
          (format t "Loaded Tileset: ~A from ~A~%" (cl-tiled:tileset-name tileset) image-path))))
    tm))

(defun get-tile-at (tilemap tile-x tile-y layer-name)
  "Get the tile ID at a specific tile position in a layer"
  (let ((layer (find layer-name 
                     (cl-tiled:map-layers (tilemap-tiled-map tilemap))
                     :key #'cl-tiled:layer-name
                     :test #'string=)))
    (when layer
      (let ((index (+ tile-x (* tile-y (tilemap-map-width tilemap)))))
        (when (< index (length (cl-tiled:layer-cells layer)))
          (cl-tiled:cell-tile (aref (cl-tiled:layer-cells layer) index)))))))

(defun render-tilemap-layer (tilemap renderer layer-name &key (camera-x 0) (camera-y 0))
  "Render a specific layer of the tilemap with camera offset"
  (let* ((tiled-map (tilemap-tiled-map tilemap))
         (layer (find layer-name 
                      (cl-tiled:map-layers tiled-map)
                      :key #'cl-tiled:layer-name
                      :test #'string=)))
    
    (when layer
      (let ((tile-width (tilemap-tile-width tilemap))
            (tile-height (tilemap-tile-height tilemap))
            (map-width (tilemap-map-width tilemap)))
        
        ;; Iterate through all tiles in the layer
        (loop for y from 0 below (tilemap-map-height tilemap)
              do (loop for x from 0 below map-width
                       for index = (+ x (* y map-width))
                       for cell = (aref (cl-tiled:layer-cells layer) index)
                       for tile = (cl-tiled:cell-tile cell)
                       when tile
                         do (render-tile tilemap renderer tile 
                                         (- (* x tile-width) camera-x)
                                         (- (* y tile-height) camera-y)
                                         tile-width
                                         tile-height)))))))

(defun render-tile (tilemap renderer tile dest-x dest-y dest-width dest-height)
  "Render a single tile at the given screen position"
  (let* ((tileset (cl-tiled:tile-tileset tile))
         (texture (gethash (cl-tiled:tileset-name tileset) 
                           (tilemap-textures tilemap))))
    
    (when texture
      (let* ((tile-id (cl-tiled:tile-id tile))
             (local-id (- tile-id (cl-tiled:tileset-first-gid tileset)))
             (tileset-columns (cl-tiled:tileset-columns tileset))
             (tileset-tile-width (cl-tiled:tileset-tile-width tileset))
             (tileset-tile-height (cl-tiled:tileset-tile-height tileset))
             
             ;; Calculate source rectangle in tileset
             (src-x (* (mod local-id tileset-columns) tileset-tile-width))
             (src-y (* (floor local-id tileset-columns) tileset-tile-height))
             
             (src-rect (sdl2:make-rect src-x src-y 
                                       tileset-tile-width 
                                       tileset-tile-height))
             (dest-rect (sdl2:make-rect (floor dest-x) 
                                        (floor dest-y)
                                        dest-width 
                                        dest-height)))
        
        (sdl2:render-copy renderer texture
                          :source-rect src-rect
                          :dest-rect dest-rect)))))

(defun world-to-tile (tilemap world-x world-y)
  "Convert world coordinates to tile coordinates"
  (values (floor world-x (tilemap-tile-width tilemap))
          (floor world-y (tilemap-tile-height tilemap))))

(defun tile-to-world (tilemap tile-x tile-y)
  "Convert tile coordinates to world coordinates"
  (values (* tile-x (tilemap-tile-width tilemap))
          (* tile-y (tilemap-tile-height tilemap))))

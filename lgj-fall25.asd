(defsystem "lgj-fall25"
  :description "Lisp Game Jam Entry for Autum 2025"
  :author "Jason Dempsey"
  :license "MIT"
  :depends-on (:sdl2 :sdl2-image :sdl2-ttf :bordeaux-threads :float-features :alexandria :cl-tiled)
  :serial t
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "utils")
                 (:file "tilemap")
                 (:file "fonts")
                 (:file "debug")
                 (:file "movement")
                 (:file "player")
                 (:file "character")
                 (:file "game")
                 (:file "main")))))

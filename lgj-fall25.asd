(defsystem "lgj-fall25"
  :description "Lisp Game Jam Entry for Autum 2025"
  :author "Jason Dempsey"
  :license "MIT"
  :depends-on (:sdl2 :sdl2-image :sdl2-ttf :bordeaux-threads :float-features)
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "utils")
                 (:file "fonts")
                 (:file "debug")
                 (:file "movement")
                 (:file "player")
                 (:file "character" :depends-on ("movement"))
                 (:file "game" :depends-on ("utils" "debug" "player"))
                 (:file "main" :depends-on ("game" "movement" "character"))))))

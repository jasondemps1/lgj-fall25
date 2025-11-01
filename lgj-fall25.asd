(defsystem "lgj-fall25"
  :description "Lisp Game Jam Entry for Autum 2025"
  :author "Jason Dempsey"
  :license "MIT"
  :depends-on (:sdl2 :sdl2-image :bordeaux-threads :cl-fast-ecs :float-features)
  :components ((:file "package")
               (:module "src"
                :components
                ((:file "utils")
                 (:file "movement")
                 (:file "character" :depends-on ("movement"))
                 (:file "game" :depends-on ("utils"))
                 (:file "main" :depends-on ("game" "movement" "character"))))))

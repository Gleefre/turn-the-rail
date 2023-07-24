(asdf:defsystem "turn-the-rail"
  :description "Turn The Rail - Game for the Trijam #229"
  :version "0.1.0"
  :author "Gleefre <varedif.a.s@gmail.com>"
  :licence "Apache 2.0"

  :depends-on ("sketch" "sketch-utils"
               "stopclock"
               "easing"
               "alexandria" "serapeum"
               "deploy"
               "harmony" "cl-mixed-vorbis"
               #+(and linux (not android)) "cl-mixed-pulse"
               #+android "cl-mixed-aaudio"
               #+darwin "cl-mixed-coreaudio"
               #+windows "cl-mixed-wasapi"
               #+bsd "cl-mixed-oss")

  :pathname "src"
  :serial T
  :components ((:file "packages")
               (:file "specials")
               (:file "utils")
               (:file "geometry")
               (:file "music")
               (:file "gameplay")
               (:file "draw")
               (:file "game"))

  :defsystem-depends-on (:deploy)
  :build-operation #-darwin "deploy-op" #+darwin "osx-app-deploy-op"
  :build-pathname "turn-the-rail"
  :entry-point "turn-the-rail:start-toplevel")

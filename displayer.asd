(asdf:defsystem #:displayer
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "0.0.0"
  :license "zlib"
  :description "An unattended display management tool"
  :homepage "https://shirakumo.github.io/displayer/"
  :bug-tracker "https://github.com/shirakumo/displayer/issues"
  :source-control (:git "https://github.com/shirakumo/displayer.git")
  :serial T
  :components ((:file "module")
               (:file "toolkit")
               (:file "playback")
               (:file "task")
               (:file "api")
               (:file "front"))
  :depends-on (:bordeaux-threads
               :filesystem-utils
               :telnetlib
               :r-clip
               :i-json
               :uiop)
  :in-order-to ((asdf:build-op
                 (asdf:load-op :displayer)
                 (asdf:build-op :radiance))))

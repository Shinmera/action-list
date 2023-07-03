(asdf:defsystem action-list
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An implementation of action lists"
  :homepage "https://github.com/Shinmera/action-list"
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "implementation")
               (:file "definition")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :trivial-extensible-sequences))

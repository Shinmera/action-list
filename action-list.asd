#|
 This file is a part of Action List
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem action-list
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
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

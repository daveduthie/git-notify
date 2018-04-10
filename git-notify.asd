#|
  This file is a part of git-notify project.
  Copyright (c) 2018 David Duthie
|#

#|
  Author: David Duthie
|#

(ql:quickload :cl-fad)

(asdf:defsystem "git-notify"
  :version "0.1.0"
  :author "David Duthie"
  :license "Eclipse"
  :depends-on ("cl-fad")
  :components ((:module "src"
                :components
                ((:file "git-notify"))))
  :description "Walk a directory of git repositories and check if they are up to date."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "./README.markdown"))
  :in-order-to ((test-op (test-op "git-notify-test"))))

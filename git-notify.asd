#|
  This file is a part of git-notify project.
  Copyright (c) 2018 David Duthie
|#

#|
  Author: David Duthie
|#

(asdf:defsystem "git-notify"
  :version "0.1.0"
  :author "David Duthie"
  :license "Eclipse"
  :depends-on ()
  :components ((:module "src"
                :serial T
                :components
                ((:file "util")
                 (:file "git-notify"))))
  :description "Walk a directory of git repositories and check if they are up to date."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "./README.md"))
  :in-order-to ((test-op (test-op "git-notify-test"))))


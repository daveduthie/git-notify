#|
  This file is a part of git-notify project.
  Copyright (c) 2018 David Duthie
|#

(asdf:defsystem "git-notify-test"
  :defsystem-depends-on ("prove-asdf")
  :author "David Duthie"
  :license ""
  :depends-on ("git-notify"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "git-notify"))))
  :description "Test system for git-notify"

  :perform (asdf:test-op (op c) (symbol-call :prove-asdf :run-test-system c)))

;; (prove.asdf:run-test-system :git-notify)


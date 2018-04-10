(defpackage git-notify-test
  (:use :cl
        :git-notify
        :prove))
(in-package :git-notify-test)

;; NOTE: To run this test file, execute `(asdf:test-system :git-notify)' in your Lisp.

(plan 1)

(is T T)

(finalize)

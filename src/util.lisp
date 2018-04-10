(defpackage util
  (:use :cl)
  (:export
   :report
   :partition))
(in-package :util)

(defun report (key fmt eater name)
  `((,key) (write-line (format nil ,fmt (funcall ,eater ,name)))))

(defun partition (list cell-size)
  (loop for cell on list by #'(lambda (list)
                                (nthcdr cell-size list))
        collecting (subseq cell 0 cell-size)))

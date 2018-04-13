(defpackage util
  (:use :cl)
  (:export
   :report
   :partition
   :*verbosity*))
(in-package :util)

(defvar *verbosity* 1)

(defvar threshold
  '(:uncommitted-changes 0
    :unpushed-changes 1
    :all-good 2))

(defun print? (key)
  (>= *verbosity* (getf threshold key)))

(defun report (key fmt eater name)
  `((,key) (if (print? ,key)
               (write-line
                (format nil
                        (concatenate 'string "~a " ,fmt)
                        (funcall ,eater ,name))))))

(defun partition (list cell-size)
  (loop for cell on list by #'(lambda (list)
                                (nthcdr cell-size list))
        collecting (subseq cell 0 cell-size)))

(ql:quickload :cl-fad)
(defpackage git-notify
  (:use :cl :cl-fad)
  (:export :check-and-report))
(in-package :git-notify)

;; Courtesy of Rosetta Code
;;
;; (ql:quickload :cl-fad)
;; (defun mapc-directory-tree (fn directory &key (depth-first-p t))
;;   (dolist (entry (cl-fad:list-directory directory))
;;     (unless depth-first-p
;;       (funcall fn entry))
;;     (when (cl-fad:directory-pathname-p entry)
;;       (mapc-directory-tree fn entry))
;;     (when depth-first-p
;;       (funcall fn entry))))

(defun find-git-dirs (directory &optional (dirs '()))
  (dolist (entry (uiop:subdirectories directory))
    (let ((git-repo? (uiop:directory-exists-p
                      (uiop:merge-pathnames* #p".git/" entry))))
      (if git-repo?
          (progn
            (setf dirs (push entry dirs)))
          (when (uiop:directory-pathname-p entry)
            (setf dirs (find-git-dirs entry dirs))))))
  dirs)

(defun check-repo (path)
  (handler-case
      (progn
        (uiop:run-program
         `("git-check" ,(namestring path))
         :output *standard-output*)
        :all-good)
    (uiop/run-program:subprocess-error (c)
      (case (uiop/run-program:subprocess-error-code c)
        ((1) :uncommited-changes)
        ((2) :unpushed-changes)
        (otherwise :idk-lol)))))

(defun eat-prefix (root)
  (let ((prefix (length (namestring (truename root)))))
    (lambda (path)
      (subseq (namestring path) prefix))))

;; FIXME
(defmacro report (key fmt eater name)
  `((,key) (write-line (format nil ,fmt (funcall ,eater ,name)))))

(defun check-and-report (root)
  (let ((eater (eat-prefix root)))
    (mapcar (lambda (dir)
              (let ((name (namestring dir)))
                (case (check-repo dir)
                  ((:all-good) (write-line (format nil "~a ✔" (funcall eater name))))
                  ;; (report :all-good "~a ✔" eater name)
                  ((:uncommited-changes) (write-line (format nil "~a ✖" (funcall eater name))))
                  ;; (report :uncommited-changes "~a ✖" eater name)
                  ((:unpushed-changes) (write-line (format nil "~a ▨" (funcall eater name))))
                  ;; (report :unpushed-changes "~a ▨" eater name)
                  ((:idk-lol) (write-line (format nil "~a ?" (funcall eater name))))
                  ;; (report :idk-lol "~a ?" eater name)
                  )))
            (find-git-dirs root))))


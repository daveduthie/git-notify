(defpackage git-notify
  (:use :cl :util)
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

(defmacro print-case (dir eater name &rest opts)
  (let ((opts (util:partition opts 2)))
    `(case (check-repo ,dir)
       ,@(mapcar (lambda (opt)
                   (apply 'util:report (car opt) (cadr opt) eater name nil))
          opts))))

(defun check-and-report (root)
  (let ((eater (eat-prefix root)))
    (mapcar (lambda (dir)
              (let ((name (namestring dir)))
                (print-case dir eater name
                 :all-good "~a ✔"
                 :uncommited-changes "~a ✖"
                 :unpushed-changes "~a ▨")))
            (find-git-dirs root))))

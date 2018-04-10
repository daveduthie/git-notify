(ql:quickload :cl-fad)
(defpackage git-notify
  (:use :cl :cl-fad))
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
         `("git-check" ,path)
         :output *standard-output*)
        :all-good)
    (uiop/run-program:subprocess-error (c)
      (case (uiop/run-program:subprocess-error-code c)
        ((1) :uncommited-changes)
        ((2) :unpushed-changes)
        (otherwise :idk-lol)))))

;; (check-repo "~/code/learn/scm-to-asm")

;; (check-repo "~/code/learn/common-lisp/git-notify")


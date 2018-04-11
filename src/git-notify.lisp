(defpackage git-notify
  (:use :cl :util)
  (:export :check-and-report))
(in-package :git-notify)

(defun git-repo? (dir)
  (uiop:probe-file*
   (concatenate 'string (namestring dir) ".git/")))

(defun find-git-dirs (directory)
  (if (git-repo? directory)
      (list directory)
      (mapcan (lambda (entry)
                (if (git-repo? entry)
                    (list entry)
                    (when (uiop:directory-pathname-p entry)
                      (find-git-dirs entry))))
              (uiop:subdirectories directory))))

(defun git-run (dir cmd &rest args)
  (let ((dirr (namestring (truename dir))))
    (uiop:run-program
     `("git"
       "--git-dir" ,(concatenate 'string dirr ".git")
       "--work-tree" ,dirr
       ,cmd ,@args)
     :output :string)))

(defun check-repo (path)
  (handler-case
      (let ((status (git-run path "status" "--porcelain"))
            (master (git-run path "rev-list" "--max-count=5" "--branches"))
            (origin (git-run path "rev-list" "--max-count=5" "--remotes")))
        (if (not (equal status ""))
            :uncommited-changes
            (if (not (equal master origin))
                :all-good)))
    (uiop/run-program:subprocess-error (c)
      (write-line (format nil "caught exit code ~d" c))
      :idk-lol)))

(defun eat-prefix (root)
  (let ((prefix (length (namestring (truename root)))))
    (lambda (path)
      (if (equal path root)
          "."
          (subseq (namestring path) prefix)))))

;; FIXME: :unpushed-changes unreachable
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

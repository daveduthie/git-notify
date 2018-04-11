(defpackage git-notify
  (:use :cl :util)
  (:export :check-and-report))
(in-package :git-notify)

(defun find-git-dirs (directory)
  (mapcan (lambda (entry)
            (let ((git-repo? (uiop:directory-exists-p
                              (uiop:merge-pathnames* #p".git/" entry))))
              (if git-repo?
                  (list entry)
                  (when (uiop:directory-pathname-p entry)
                    (find-git-dirs entry)))))
          (uiop:subdirectories directory)))

(defun git-run (dir cmd &optional (args ";"))
  (uiop:run-program
   (list "git" "--work-tree" (namestring dir) cmd args)
   :output :string))

(defun check-repo (path)
  (let ((status (git-run path "status" "--porcelain"))
        (master (git-run path "rev-parse" "--verify master"))
        (origin (git-run path "rev-parse" "--verify origin/master")))
    (if (not (string= status ""))
        :uncommited-changes
        (if (not (string= master origin))
            :unpushed-changes
            :all-good))))

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

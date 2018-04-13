(defpackage git-notify
  (:use :cl :util :unix-opts)
  (:shadowing-import-from :unix-opts :describe)
  (:export :-main))
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
      (let ((status
              (git-run path "status" "--porcelain"))
            (up-to-date
              (git-run path "branch" "--format=\"%(upstream:track)\"")))
        (cond
          ((not (equal status "")) :uncommitted-changes)
          ((not (equal up-to-date "")) :unpushed-changes)
          (:otherwise :all-good)))
    (uiop/run-program:subprocess-error (c)
      (write-line (format nil "caught exit code ~d" c))
      :idk-lol)))

(defun eat-prefix (root)
  (let ((prefix (length (namestring (truename root)))))
    (lambda (path)
      (if (equal path root)
          "."
          (subseq (namestring path) prefix)))))

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
                 :all-good "✔"
                 :uncommitted-changes "✖"
                 :unpushed-changes "▨")))
            (find-git-dirs root))))

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :level
   :description "show uncommitted (0), unpushed (1), or all (2) repos"
   :short #\l
   :long "level"
   :arg-parser #'parse-integer)
  (:name :root
   :description "repository root"
   :short #\r
   :long "root"
   :arg-parser #'namestring))

(defun -main (argv)
  (handler-case
      (multiple-value-bind
            (options free-args) (opts:get-opts argv)
        (when (getf options :help)
          (print (opts:describe))
          (opts:exit))
        (setf util:*verbosity* (getf options :level 1))
        (check-and-report
         (or (getf options :root (first free-args))
             *default-pathname-defaults*)))
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     ()
      (write-line "(∗ ･‿･)ﾉ゛")
      (opts:exit))))

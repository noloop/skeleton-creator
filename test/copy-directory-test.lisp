(in-package #:cl-user)
(defpackage #:noloop.copy-directory-test
  (:use #:common-lisp
        #:simplet)
  (:nicknames #:copy-directory-test)
  (:import-from #:skeleton-creator
                #:pathname-subtract
                #:conc
                #:write-string-in-file
                #:copy-directory))
(in-package #:noloop.copy-directory-test)

(defun test-pathname-subtract ()
  (let* ((path-1 "/home/you/lisp/projects/")
         (path-2 "/home/you/lisp/projects/child-dir/other-dir/")
         (result (pathname-subtract path-1 path-2)))
    (and (cl-fad:pathname-equal #P"child-dir/other-dir/" result))))

(defun test-copy-directory ()
  (let* ((sk-dir "/tmp/.skeleton/")
         (sk-file-1 (conc sk-dir "sk-file-1.lisp"))
         (sk-child-dir (conc sk-dir "skeleton-child-dir/"))
         (sk-file-2 (conc sk-child-dir "README"))
         (sk-file-3 (conc sk-dir ".sk-file-3.lisp"))
         (destination-path "/tmp/new-project-test/")
         (result nil))
    (ensure-directories-exist sk-dir)
    (ensure-directories-exist sk-child-dir)
    (write-string-in-file sk-file-1 "Here?")
    (write-string-in-file sk-file-2 "New project")
    (write-string-in-file sk-file-3 "Or here?")
    (copy-directory sk-dir destination-path :overwrite t)
    (setf result (and (cl-fad:directory-exists-p destination-path)
                      (cl-fad:file-exists-p (conc destination-path "sk-file-1.lisp"))
                      (cl-fad:file-exists-p (conc destination-path ".sk-file-3.lisp"))
                      (cl-fad:directory-exists-p (conc destination-path "skeleton-child-dir/"))
                      (cl-fad:file-exists-p (conc destination-path "skeleton-child-dir/README"))
                      t))
    (cl-fad:delete-directory-and-files sk-dir)
    (cl-fad:delete-directory-and-files destination-path)
    result))

(suite "Suite copy-directory-test"
       (test "Test pathname-subtract" #'test-pathname-subtract)
       (test "Test copy-directory-recursive" #'test-copy-directory))

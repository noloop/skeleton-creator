(in-package #:cl-user)
(defpackage #:noloop.skeleton-creator-test
  (:use #:common-lisp)
  (:nicknames #:skeleton-creator-test)
  (:import-from #:skeleton-creator
                #:replace-markings-in-file-names
                #:delete-project-directory))
(in-package #:noloop.skeleton-creator-test)

;; Simple Test Runner
(defun run ()
  (suite
   (test "Test string-match-markings" #'test-string-match-markings)
   (test "Test string-replace-all" #'test-string-replace-all)
   (test "Test write-string-in-file and get-string-from-file" #'test-write-string-in-file)
   (test "Test pathname-is-file" #'test-pathname-is-file)
   (test "Test not-match-list-ignore" #'test-not-match-list-ignore)
   (test "Test walk-destination-directory" #'test-walk-destination-directory)))

(defun test (stg test-fn)
  (let ((result (funcall test-fn)))
    (format t "~a: ~a~%" stg result)
    result))

(defun suite (&rest results)
  (format t "Test result: ~a"
          (every #'(lambda (el) (equal t el)) results)))

;; Utils
(defmacro conc (&rest string-args)
  "Synthetic sugar for concatenate strings."
  `(concatenate 'string ,@string-args))

(defun test-string-replace-all ()
  (let ((stg "old string")
        (expected-stg "expected string")
        (old-stg "old")
        (new-stg "expected"))
    (and (string= expected-stg
                  (skeleton-creator::string-replace-all stg old-stg new-stg)))))

(defun test-write-string-in-file ()
  (let ((file-name "/tmp/write-string-in-file.tmp")
        (expected-stg "I exists?")
        (actual-stg ""))
    (skeleton-creator::write-string-in-file file-name "I exists?")
    (setf actual-stg (skeleton-creator::get-string-from-file file-name))
    (delete-file file-name)
    (and (string= expected-stg actual-stg))))

(defun test-pathname-is-file ()
  (let ((file-name "/tmp/pathname-is-file.tmp")
         (path-file nil)
         (path-directory t)
         (path-file-not-exist t)
         (path-directory-not-exist t))
    (skeleton-creator::write-string-in-file file-name "I'm here?")
    (setf path-file (skeleton-creator::pathname-is-file "/tmp/pathname-is-file.tmp"))
    (setf path-directory (skeleton-creator::pathname-is-file "/tmp/"))
    (setf path-file-not-exist (skeleton-creator::pathname-is-file "/tmp/pathname-is-file-not-exist.tmp"))
    (setf path-directory-not-exist (skeleton-creator::pathname-is-file "/tmp/pathname-is-file-not-exist/"))
    (delete-file file-name)
    (and (not (null path-file))
         (null path-directory)
         (null path-file-not-exist)
         (null path-directory-not-exist))))

(defun test-string-match-markings ()
  (let ((hash-markings (make-hash-table)))
    (setf (gethash :PROJECT-NAME hash-markings) "new-p")
    (and (not (null (skeleton-creator::string-match-markings "PROJECT-NAME" hash-markings))))))

(defun test-not-match-list-ignore ()
  (let* ((destination-directory "/tmp/not-match-replace-ignore/")
         (ignores '(".git/" "file.lisp" "child-directory/child-file.lisp" "file-without-type"))
         (path-child-directory (conc destination-directory "child-directory/"))
         (path-file-1 (conc destination-directory "file.lisp"))
         (path-child-file-1 (conc destination-directory "child-directory/child-file.lisp"))
         (path-file-2 (conc destination-directory "file-without-type"))
         (not-match-fn (skeleton-creator::not-match-list-ignore
                        destination-directory
                        ignores))
         (result nil))
    (setf result (and (not (funcall not-match-fn "/tmp/not-match-replace-ignore/.git/")) ;nil
                      (funcall not-match-fn path-child-directory) ;T
                      (not (funcall not-match-fn path-file-1)) ;nil
                      (not (funcall not-match-fn path-child-file-1)) ;nil
                      (not (funcall not-match-fn path-file-2)) ;nil
                      (funcall not-match-fn "/tmp/file-2.lisp"))) ;T
    result))

(defun test-walk-destination-directory ()
  (let* ((destination-directory "/tmp/walk-destination-directory/")
         (ignores '(".git/" "child-dir/"))
        (not-match-fn (skeleton-creator::not-match-list-ignore
                       destination-directory
                       ignores))
         (path-file-1 (concatenate 'string destination-directory "PROJECT-NAME.lisp"))
         (path-child-directory (concatenate 'string destination-directory "child-dir/"))
        (path-file-2 (concatenate 'string path-child-directory "file-child.lisp"))
        (paths-collected '()))
    (ensure-directories-exist destination-directory)
    (ensure-directories-exist path-child-directory)
    (skeleton-creator::write-string-in-file path-file-1 "I'm here?")
    (skeleton-creator::write-string-in-file path-file-2 "I...?")
    (skeleton-creator::walk-destination-directory
     destination-directory
     #'(lambda (el) (push el paths-collected))
     ignores)
    (delete-project-directory destination-directory)
    (and (= 2 (length paths-collected))
         (string=
          (conc destination-directory "PROJECT-NAME.lisp")
          (namestring (car paths-collected)))
         (string=
          destination-directory
          (namestring (cadr paths-collected))))))

#|
(defun test-replace-markings-in-file-names ()
  (let* ((path-directory "/tmp/replace-markings-in-file-names/")
         (path-file-1 (concatenate 'string path-directory "PROJECT-NAME.lisp"))
         (path-child-directory (concatenate 'string path-directory "child-dir/"))
         (path-file-2 (concatenate 'string path-child-directory "PROJECT-NAME.lisp"))
         (hash-markings (make-hash-table))
         (result nil))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-child-directory)
    (skeleton-creator::write-string-in-file path-file-1 "I'm here?")
    (skeleton-creator::write-string-in-file path-file-2 "I don't know.")
    (setf (gethash :PROJECT-NAME hash-markings) "new-p")
    (replace-markings-in-file-names path-directory hash-markings)
    (setf result (and
                  (cl-fad:file-exists-p (concatenate 'string path-directory "new-p.lisp"))
                  (cl-fad:file-exists-p (concatenate 'string path-child-directory "new-p.lisp"))))
    ;(delete-project-directory path-directory)
    result))
  |#




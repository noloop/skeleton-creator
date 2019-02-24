(in-package #:cl-user)
(defpackage #:noloop.skeleton-creator-test
  (:use #:common-lisp
        #:simplet)
  (:nicknames #:skeleton-creator-test)
  (:import-from #:skeleton-creator
                #:string-replace-all
                #:conc
                #:write-string-in-file
                #:get-string-from-file
                #:pathname-is-file
                #:not-match-list-ignore
                #:walk-destination-directory
                #:merge-path-with-new-file-name
                #:string-match-markings
                #:set-field
                #:get-field
                #:init-skeleton-creator
                #:replace-markings
                #:replace-markings-in-file-names
                #:replace-markings-in-file
                #:delete-project))
(in-package #:noloop.skeleton-creator-test)

(defun test-string-replace-all ()
  (let ((stg "old string")
        (expected-stg "expected string")
        (old-stg "old")
        (new-stg "expected"))
    (and (string= expected-stg
                  (string-replace-all stg old-stg new-stg)))))

(defun test-write-string-in-file ()
  (let ((file-name "/tmp/write-string-in-file.tmp")
        (expected-stg "I exists?")
        (actual-stg ""))
    (write-string-in-file file-name "I exists?")
    (setf actual-stg (get-string-from-file file-name))
    (delete-file file-name)
    (and (string= expected-stg actual-stg))))

(defun test-pathname-is-file ()
  (let ((file-name "/tmp/pathname-is-file.tmp")
         (path-file nil)
         (path-directory t)
         (path-file-not-exist t)
         (path-directory-not-exist t))
    (write-string-in-file file-name "I'm here?")
    (setf path-file (pathname-is-file "/tmp/pathname-is-file.tmp"))
    (setf path-directory (pathname-is-file "/tmp/"))
    (setf path-file-not-exist (pathname-is-file "/tmp/pathname-is-file-not-exist.tmp"))
    (setf path-directory-not-exist (pathname-is-file "/tmp/pathname-is-file-not-exist/"))
    (delete-file file-name)
    (and (not (null path-file))
         (null path-directory)
         (null path-file-not-exist)
         (null path-directory-not-exist))))

(defun test-not-match-list-ignore ()
  (let* ((destination-directory "/tmp/not-match-replace-ignore/")
         (ignores '(".git/" "file.lisp" "child-directory/child-file.lisp" "file-without-type"))
         (path-child-directory (conc destination-directory "child-directory/"))
         (path-file-1 (conc destination-directory "file.lisp"))
         (path-child-file-1 (conc destination-directory "child-directory/child-file.lisp"))
         (path-file-2 (conc destination-directory "file-without-type"))
         (not-match-fn (not-match-list-ignore
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
         (path-file-1 (concatenate 'string destination-directory "SK-PROJECT-NAME.lisp"))
         (path-child-directory (concatenate 'string destination-directory "child-dir/"))
        (path-file-2 (concatenate 'string path-child-directory "file-child.lisp"))
        (paths-collected '()))
    (ensure-directories-exist destination-directory)
    (ensure-directories-exist path-child-directory)
    (write-string-in-file path-file-1 "I'm here?")
    (write-string-in-file path-file-2 "I...?")
    (walk-destination-directory
     destination-directory
     #'(lambda (el) (push el paths-collected))
     ignores)
    (delete-project destination-directory)
    (and (= 2 (length paths-collected))
         (string=
          (conc destination-directory "SK-PROJECT-NAME.lisp")
          (namestring (car paths-collected)))
         (string=
          destination-directory
          (namestring (cadr paths-collected))))))

(defun test-merge-path-with-new-file-name ()
  (let* ((path "/tmp/rename-path-file/file.lisp")
        (new-file-name "new-file")
         (result (merge-path-with-new-file-name
                 path
                 new-file-name)))
    (not (null (string= "/tmp/rename-path-file/new-file.lisp" (namestring result))))))

(defun test-merge-path-with-new-file-name-with-path-directory ()
  (let* ((path "/tmp/rename-path-file/")
         (new-file-name "new-file")
         (result (merge-path-with-new-file-name
                  path
                  new-file-name)))
    (not (null (string= "/tmp/rename-path-file/new-file" (namestring result))))))

(defun test-string-match-markings ()
  (let ((hash-markings (make-hash-table)))
    (setf (gethash :SK-PROJECT-NAME hash-markings) "new-p")
    (and (not (null (string-match-markings "SK-PROJECT-NAME" hash-markings))))))

(defun test-replace-markings-in-file-names ()
  (let* ((path-directory "/tmp/replace-markings-in-file-names/")
         (path-file-1 (concatenate 'string path-directory "SK-PROJECT-NAME-test.lisp"))
         (path-child-directory (concatenate 'string path-directory "child-dir/"))
         (path-file-2 (concatenate 'string path-child-directory "SK-PROJECT-NAME.lisp"))
         (hash-markings (make-hash-table))
         (result nil))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-child-directory)
    (write-string-in-file path-file-1 "I'm here?")
    (write-string-in-file path-file-2 "I don't know.")
    (setf (gethash :SK-PROJECT-NAME hash-markings) "new-p")
    (replace-markings-in-file-names path-directory hash-markings)
    (setf result (and
                  (cl-fad:file-exists-p (concatenate 'string path-directory "new-p-test.lisp"))
                  (cl-fad:file-exists-p (concatenate 'string path-child-directory "new-p.lisp"))))
    (delete-project path-directory)
    (not (null result))))

(defun test-replace-markings-in-file-names-with-ignores ()
  (let* ((path-directory "/tmp/replace-markings-in-file-names-with-ignores/")
         (path-file-1 (concatenate 'string path-directory "SK-PROJECT-NAME.lisp"))
         (path-child-directory (concatenate 'string path-directory "child-dir/"))
         (path-file-2 (concatenate 'string path-child-directory "SK-PROJECT-NAME.lisp"))
         (hash-markings (make-hash-table))
         (ignores '(".git/" "child-dir/"))
         (result nil))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-child-directory)
    (write-string-in-file path-file-1 "I'm here?")
    (write-string-in-file path-file-2 "I don't know.")
    (setf (gethash :SK-PROJECT-NAME hash-markings) "new-p")
    (replace-markings-in-file-names path-directory hash-markings ignores)
    (setf result (and (cl-fad:file-exists-p (concatenate 'string path-directory "new-p.lisp"))))
    (delete-project path-directory)
    (not (null result))))

(defun test-replace-markings-in-file ()
  (let* ((path-directory "/tmp/replace-markings-in-file/")
         (path-file-1 (concatenate 'string path-directory "SK-PROJECT-NAME.lisp"))
        (path-child-directory (concatenate 'string path-directory "child-dir/"))
         (path-file-2 (concatenate 'string path-child-directory "SK-PROJECT-NAME.lisp"))
        (hash-markings (make-hash-table))
        (result nil))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-child-directory)
    (write-string-in-file path-file-1 "SK-PROJECT-NAME")
    (write-string-in-file path-file-2 "SK-PROJECT-NAME by SK-AUTHOR")
    (setf (gethash :SK-PROJECT-NAME hash-markings) "new-project")
    (setf (gethash :SK-AUTHOR hash-markings) "your")
    (replace-markings-in-file path-directory hash-markings)
    (setf result (and (string= "new-project" (get-string-from-file path-file-1))
                      (string= "new-project by your" (get-string-from-file path-file-2))))
    (delete-project path-directory)
    (not (null result))))

(defun test-replace-markings-in-file-with-ignores ()
  (let* ((path-directory "/tmp/replace-markings-in-file/")
         (path-file-1 (concatenate 'string path-directory "SK-PROJECT-NAME.lisp"))
         (path-child-directory (concatenate 'string path-directory "child-dir/"))
         (path-file-2 (concatenate 'string path-child-directory "SK-PROJECT-NAME.lisp"))
         (hash-markings (make-hash-table))
         (ignores '(".git/" "child-dir/"))
         (result nil))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-child-directory)
    (write-string-in-file path-file-1 "SK-PROJECT-NAME")
    (write-string-in-file path-file-2 "SK-PROJECT-NAME by SK-AUTHOR")
    (setf (gethash :SK-PROJECT-NAME hash-markings) "new-project")
    (setf (gethash :SK-AUTHOR hash-markings) "your")
    (replace-markings-in-file path-directory hash-markings ignores)
    (setf result (and (string= "new-project" (get-string-from-file path-file-1))
                      (string= "SK-PROJECT-NAME by SK-AUTHOR" (get-string-from-file path-file-2))))
    (delete-project path-directory)
    (not (null result))))

(defun test-replace-markings ()
  (let* ((path-directory "/tmp/replace-markings/")
         (path-conf-directory (conc path-directory ".conf/"))
         (path-file-conf (conc path-conf-directory "skeleton-creator.conf"))
         (path-file-1 (conc path-directory "SK-PROJECT-NAME.asd"))
         (path-child-directory (conc path-directory "child-dir/"))
         (path-file-2 (conc path-child-directory "SK-PROJECT-NAME.lisp"))
         (sk nil)
         (ignores '(".git/" ".conf/"))
         (expected-path-file-1 (conc path-directory "new-project.asd"))
         (expected-path-file-2 (conc path-child-directory "new-project.lisp"))
         (result nil))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-conf-directory)
    (ensure-directories-exist path-child-directory)
    (write-string-in-file path-file-conf "(:SK-AUTHOR \"your\" :SK-VERSION \"0.1.0\")")
    (setf sk (init-skeleton-creator "/tmp/replace-markings/.conf/"))
    (set-field sk :SK-PROJECT-NAME "new-project")
    (set-field sk :REPLACE-IGNORE ignores)
    (write-string-in-file path-file-1 "SK-PROJECT-NAME version SK-VERSION")
    (write-string-in-file path-file-2 "SK-PROJECT-NAME by SK-AUTHOR")
    (replace-markings sk path-directory)
    (setf result (and (cl-fad:file-exists-p expected-path-file-1)
          (cl-fad:file-exists-p expected-path-file-2)
          (string= "new-project version 0.1.0" (get-string-from-file expected-path-file-1))
          (string= "new-project by your" (get-string-from-file expected-path-file-2))))
    (delete-project path-directory)
    (not (null result))))

(suite "Suite skeleton-creator-test"
       (test "Test string-replace-all" #'test-string-replace-all)
       (test "Test write-string-in-file and get-string-from-file" #'test-write-string-in-file)
       (test "Test pathname-is-file" #'test-pathname-is-file)
       (test "Test not-match-list-ignore" #'test-not-match-list-ignore)
       (test "Test walk-destination-directory" #'test-walk-destination-directory)
       (test "Test merge-path-with-new-file-name" #'test-merge-path-with-new-file-name)
       (test "Test merge-path-with-new-file-name-with-path-directory" #'test-merge-path-with-new-file-name-with-path-directory)
       (test "Test string-match-markings" #'test-string-match-markings)
       (test "Test replace-markings-in-file-names" #'test-replace-markings-in-file-names)
       (test "Test replace-markings-in-file-names-with-ignores" #'test-replace-markings-in-file-names-with-ignores)
       (test "Test replace-markings-in-file" #'test-replace-markings-in-file)
       (test "Test replace-markings-in-file-with-ignores" #'test-replace-markings-in-file-with-ignores)
       (test "Test replace-markings" #'test-replace-markings))


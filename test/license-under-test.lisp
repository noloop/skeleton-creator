(in-package #:cl-user)
(defpackage #:noloop.license-under-test
  (:use #:common-lisp
        #:simplet)
  (:nicknames #:license-under-test)
  (:import-from #:skeleton-creator
                #:conc
                #:write-string-in-file
                #:get-string-from-file
                #:delete-project-directory
                #:get-string-license
                #:create-license-file
                #:write-license-notices
                ;;#:license-under
                ))
(in-package #:noloop.license-under-test)

(defun test-get-string-license ()
  (let* ((path-conf-directory "/tmp/.conf/")
         (path-licenses-directory (cl-fad:merge-pathnames-as-directory path-conf-directory "licenses/"))
         (path-license (cl-fad:merge-pathnames-as-file path-conf-directory "licenses/null-license.txt"))
         (license-name "null-license")
         (expected-content "not licensed.")
         (actual-content ""))
    (ensure-directories-exist path-conf-directory)
    (ensure-directories-exist path-licenses-directory)
    (write-string-in-file path-license "not licensed.")
    (setf actual-content (get-string-license path-licenses-directory license-name))
    (delete-project-directory path-conf-directory)
    (string= expected-content actual-content)))

(defun test-create-license-file ()
  (let* ((path-conf-directory "/tmp/.conf/")
         (path-licenses-directory (cl-fad:merge-pathnames-as-directory path-conf-directory "licenses/"))
         (path-project-directory "/tmp/create-licenses-file-project-test/")
         (path-license (cl-fad:merge-pathnames-as-file path-conf-directory "licenses/null-license.txt"))
         (license-name "null-license")
         (expected-content "not licensed.")
         (actual-content ""))
    (ensure-directories-exist path-conf-directory)
    (ensure-directories-exist path-licenses-directory)
    (ensure-directories-exist path-project-directory)
    (write-string-in-file path-license "not licensed.")
    (create-license-file path-project-directory path-licenses-directory license-name)
    (setf actual-content
          (get-string-from-file
           (cl-fad:merge-pathnames-as-file path-project-directory "LICENSE")))
    (delete-project-directory path-conf-directory)
    (delete-project-directory path-project-directory)
    (string= expected-content actual-content)))

(defun test-write-license-notices ()
  (let* ((path-directory "/tmp/write-license-notices/")
         (path-conf-directory (cl-fad:merge-pathnames-as-directory path-directory ".conf/"))
         (path-licenses-directory (cl-fad:merge-pathnames-as-directory path-conf-directory "licenses/"))
         (path-license (cl-fad:merge-pathnames-as-file path-conf-directory "licenses/null-license.txt"))
         (path-noticies-directory (cl-fad:merge-pathnames-as-directory path-licenses-directory "noticies/"))
         (path-noticie (cl-fad:merge-pathnames-as-file path-noticies-directory "null-license-noticie.txt"))
         (path-project-directory (cl-fad:merge-pathnames-as-directory path-directory "new-project-test/"))
         (path-project-child-directory (cl-fad:merge-pathnames-as-directory path-project-directory "child-dir/"))
         (path-project-file-1 (cl-fad:merge-pathnames-as-file path-project-directory "file-1.lisp"))
         (path-project-file-2 (cl-fad:merge-pathnames-as-file path-project-directory "file-2.lisp"))
         (path-project-file-3 (cl-fad:merge-pathnames-as-file path-project-directory "child-dir/" "file-3.lisp"))  
         (license-name "null-license")
         (expected-content "new-project-test (C) 2019 by your.")
         (actual-content-1 "")
         (actual-content-2 "")
         (actual-content-3 "")
         (hash-markings (make-hash-table)))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-conf-directory)
    (ensure-directories-exist path-licenses-directory)
    (ensure-directories-exist path-noticies-directory)
    (ensure-directories-exist path-project-directory)
    (ensure-directories-exist path-project-child-directory)
    (write-string-in-file path-license "not licensed.")
    (write-string-in-file path-noticie "PROJECT-NAME (C) DATE-YEAR by AUTHOR.")
    (write-string-in-file path-project-file-1 "(list 1 2 3)")
    (write-string-in-file path-project-file-2 "(= 1 2)")
    (write-string-in-file path-project-file-3 "(+ 1 1)")
    (setf (gethash :PROJECT-NAME hash-markings) "new-project-test")
    (setf (gethash :AUTHOR hash-markings) "your")
    (setf (gethash :EMAIL hash-markings) "your@email.com")
    (setf (gethash :DATE-YEAR hash-markings) "2019")
    (write-license-notices path-project-directory path-licenses-directory license-name hash-markings)
    (setf actual-content-1 (get-string-from-file path-project-file-1))
    (setf actual-content-2 (get-string-from-file path-project-file-2))
    (setf actual-content-3 (get-string-from-file path-project-file-3))
    (delete-project-directory path-conf-directory)
    (delete-project-directory path-project-directory)
    (every #'(lambda (i) (string= i expected-content)) (list actual-content-1 actual-content-2 actual-content-3))))

(suite "Suite license-under-test"
       (test "Test get-string-license" #'test-get-string-license)
       (test "Test create-license-file" #'test-create-license-file)
       (test "Test write-license-notices" #'test-write-license-notices))


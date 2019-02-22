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

(suite "Suite license-under-test"
       (test "Test get-string-license" #'test-get-string-license)
       (test "Test create-license-file" #'test-create-license-file))


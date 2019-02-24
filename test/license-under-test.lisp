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
                #:get-notice-path
                #:write-license-notices
                #:write-in-readme
                #:license-under))
(in-package #:noloop.license-under-test)

(defun test-get-string-license ()
  (let* ((path-directory "/tmp/test-get-string-license/")
         (path-conf-directory (cl-fad:merge-pathnames-as-directory path-directory ".conf/"))
         (path-licenses-directory (cl-fad:merge-pathnames-as-directory path-conf-directory "licenses/"))
         (path-license (cl-fad:merge-pathnames-as-file path-licenses-directory "null-license.txt"))
         (license-name "null-license")
         (expected-content "not licensed.")
         (actual-content ""))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-conf-directory)
    (ensure-directories-exist path-licenses-directory)
    (write-string-in-file path-license "not licensed.")
    (setf actual-content (get-string-license path-licenses-directory license-name))
    (delete-project-directory path-directory)
    (string= expected-content actual-content)))

(defun test-create-license-file ()
  (let* ((path-directory "/tmp/test-create-license-file/")
         (path-conf-directory (cl-fad:merge-pathnames-as-directory path-directory ".conf/"))
         (path-licenses-directory (cl-fad:merge-pathnames-as-directory path-conf-directory "licenses/"))
         (path-project-directory (cl-fad:merge-pathnames-as-directory path-directory "new-project/"))
         (path-license (cl-fad:merge-pathnames-as-file path-licenses-directory "null-license.txt"))
         (license-name "null-license")
         (expected-content "not licensed.")
         (actual-content ""))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-conf-directory)
    (ensure-directories-exist path-licenses-directory)
    (ensure-directories-exist path-project-directory)
    (write-string-in-file path-license "not licensed.")
    (create-license-file path-project-directory path-licenses-directory license-name)
    (setf actual-content
          (get-string-from-file
           (cl-fad:merge-pathnames-as-file path-project-directory "LICENSE")))
    (delete-project-directory path-directory)
    (string= expected-content actual-content)))

(defun test-get-notice-path ()
  (let* ((path-notices-directory "/tmp/test-get-notice-path/notices/")
         (license-name "gplv3")
         (expected-path-license (cl-fad:merge-pathnames-as-file
                        path-notices-directory
                        (conc license-name "-notice.txt")))
         (actual-path-license (get-notice-path
                               path-notices-directory
                               license-name)))
    (cl-fad:pathname-equal expected-path-license actual-path-license)))

(defun test-write-license-notices ()
  (let* ((path-directory "/tmp/test-write-license-notices/")
         (path-conf-directory (cl-fad:merge-pathnames-as-directory path-directory ".conf/"))
         (path-licenses-directory (cl-fad:merge-pathnames-as-directory path-conf-directory "licenses/"))
         (path-notices-directory (cl-fad:merge-pathnames-as-directory path-licenses-directory "notices/"))
         (path-notice (cl-fad:merge-pathnames-as-file path-notices-directory "null-license-notice.txt"))
         (path-project-directory (cl-fad:merge-pathnames-as-directory path-directory "new-project-test/"))
         (path-project-child-directory (cl-fad:merge-pathnames-as-directory path-project-directory "child-dir/"))
         (path-project-file-1 (cl-fad:merge-pathnames-as-file path-project-directory "file-1.lisp"))
         (path-project-file-2 (cl-fad:merge-pathnames-as-file path-project-directory "file-2.lisp"))
         (path-project-file-3 (cl-fad:merge-pathnames-as-file path-project-child-directory "file-3.lisp")) 
         (path-project-readme (cl-fad:merge-pathnames-as-file path-project-directory "README.md"))
         (path-project-license (cl-fad:merge-pathnames-as-file path-project-directory "LICENSE"))
         (path-file-already-licensed (cl-fad:merge-pathnames-as-file path-project-directory "already-licensed.lisp"))
         (license-name "null-license")
         (expected-content (format nil "~a~%~a~%~a~%~%~a"
                                   "#| LICENSE NOTICE"
                                   "new-project (C) 2019 by your."
                                   "|#"
                                   "(list 1 2 3)"))
         (actual-content-1 "")
         (actual-content-2 "")
         (actual-content-3 "")
         (hash-markings (make-hash-table))
         (ignores '(".git/" ".conf/" "LICENSE" "README.md"))
         (result nil))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-conf-directory)
    (ensure-directories-exist path-licenses-directory)
    (ensure-directories-exist path-notices-directory)
    (ensure-directories-exist path-project-directory)
    (ensure-directories-exist path-project-child-directory)
    (write-string-in-file path-notice "SK-PROJECT-NAME (C) SK-DATE-YEAR by SK-AUTHOR.")
    (write-string-in-file path-project-file-1 "(list 1 2 3)")
    (write-string-in-file path-project-file-2 "(list 1 2 3)")
    (write-string-in-file path-project-file-3 "(list 1 2 3)")
    (write-string-in-file path-project-readme "readme...")
    (write-string-in-file path-project-license "license...")
    (write-string-in-file path-file-already-licensed "|# haa LICENSE NOTICE haa... |#")
    (setf (gethash :SK-PROJECT-NAME hash-markings) "new-project")
    (setf (gethash :SK-AUTHOR hash-markings) "your")
    (setf (gethash :SK-DATE-YEAR hash-markings) "2019")
    (write-license-notices path-project-directory path-notices-directory license-name hash-markings ignores)
    (setf actual-content-1 (get-string-from-file path-project-file-1))
    (setf actual-content-2 (get-string-from-file path-project-file-2))
    (setf actual-content-3 (get-string-from-file path-project-file-3))
    (setf result
          (and (string= "readme..." (get-string-from-file path-project-readme))
               (string= "license..." (get-string-from-file path-project-license))
               (string= "|# haa LICENSE NOTICE haa... |#" (get-string-from-file path-file-already-licensed))
               (every #'(lambda (i) (string= i expected-content)) (list actual-content-1 actual-content-2 actual-content-3))))
    (delete-project-directory path-directory)
    result))

(defun test-write-in-readme ()
  (let* ((path-directory "/tmp/test-write-in-readme/")
         (path-conf-directory (cl-fad:merge-pathnames-as-directory path-directory ".conf/"))
         (path-licenses-directory (cl-fad:merge-pathnames-as-directory path-conf-directory "licenses/"))
         (path-notices-directory (cl-fad:merge-pathnames-as-directory path-licenses-directory "notices/"))
         (path-notice (cl-fad:merge-pathnames-as-file path-notices-directory "null-license-notice.txt"))
         (path-project-directory (cl-fad:merge-pathnames-as-directory path-directory "new-project-test/"))
         (path-project-readme (cl-fad:merge-pathnames-as-file path-project-directory "README.md"))
         (license-name "null-license")
         (expected-content (format nil "~a~%~%~a~%~%~a~%"
                                   "readme..."
                                   "### LICENSE"
                                   "new-project (C) 2019 by your."))
         (hash-markings (make-hash-table))
         (result nil))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-conf-directory)
    (ensure-directories-exist path-licenses-directory)
    (ensure-directories-exist path-notices-directory)
    (ensure-directories-exist path-project-directory)
    (write-string-in-file path-notice "SK-PROJECT-NAME (C) SK-DATE-YEAR by SK-AUTHOR.")
    (write-string-in-file path-project-readme "readme...")
    (setf (gethash :SK-PROJECT-NAME hash-markings) "new-project")
    (setf (gethash :SK-AUTHOR hash-markings) "your")
    (setf (gethash :SK-DATE-YEAR hash-markings) "2019")
    (write-in-readme path-project-directory path-notices-directory license-name hash-markings)
    (setf result
          (string= expected-content (get-string-from-file path-project-readme)))
    (delete-project-directory path-directory)
    result))

(defun test-write-in-readme-already-licensed ()
  (let* ((path-directory "/tmp/test-write-in-readme-already-licensed/")
         (path-conf-directory (cl-fad:merge-pathnames-as-directory path-directory ".conf/"))
         (path-licenses-directory (cl-fad:merge-pathnames-as-directory path-conf-directory "licenses/"))
         (path-notices-directory (cl-fad:merge-pathnames-as-directory path-licenses-directory "notices/"))
         (path-notice (cl-fad:merge-pathnames-as-file path-notices-directory "null-license-notice.txt"))
         (path-project-directory (cl-fad:merge-pathnames-as-directory path-directory "new-project-test/"))
         (path-project-readme (cl-fad:merge-pathnames-as-file path-project-directory "README.md"))
         (license-name "null-license")
         (expected-content (format nil "~a~%~%~a~%~%~a~%"
                                   "readme..."
                                   "### LICENSE <ALREADY>"
                                   "new-project (C) 2019 by your."))
         (hash-markings (make-hash-table))
         (result nil))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-conf-directory)
    (ensure-directories-exist path-licenses-directory)
    (ensure-directories-exist path-notices-directory)
    (ensure-directories-exist path-project-directory)
    (write-string-in-file path-notice "PROJECT-NAME (C) SK-DATE-YEAR by SK-AUTHOR.")
    (write-string-in-file path-project-readme (format nil "~a~%~%~a~%~%~a~%"
                                                      "readme..."
                                                      "### LICENSE <ALREADY>"
                                                      "new-project (C) 2019 by your."))
    (setf (gethash :SK-PROJECT-NAME hash-markings) "new-project")
    (setf (gethash :SK-AUTHOR hash-markings) "your")
    (setf (gethash :SK-DATE-YEAR hash-markings) "2019")
    (write-in-readme path-project-directory path-notices-directory license-name hash-markings)
    (setf result
          (string= expected-content (get-string-from-file path-project-readme)))
    (delete-project-directory path-directory)
    result))

(defun test-license-under ()
  (let* ((path-directory "/tmp/test-license-under/")
         (path-conf-directory (cl-fad:merge-pathnames-as-directory path-directory ".conf/"))
         (path-licenses-directory (cl-fad:merge-pathnames-as-directory path-conf-directory "licenses/"))
         (path-license (cl-fad:merge-pathnames-as-file path-conf-directory "licenses/null-license.txt"))
         (path-notices-directory (cl-fad:merge-pathnames-as-directory path-licenses-directory "notices/"))
         (path-notice (cl-fad:merge-pathnames-as-file path-notices-directory "null-license-notice.txt"))
         (path-project-directory (cl-fad:merge-pathnames-as-directory path-directory "new-project-test/"))
         (path-project-child-directory (cl-fad:merge-pathnames-as-directory path-project-directory "child-dir/"))
         (path-project-file-1 (cl-fad:merge-pathnames-as-file path-project-directory "file-1.lisp"))
         (path-project-file-2 (cl-fad:merge-pathnames-as-file path-project-directory "file-2.lisp"))
         (path-project-file-3 (cl-fad:merge-pathnames-as-file path-project-child-directory "file-3.lisp")) 
         (path-project-readme (cl-fad:merge-pathnames-as-file path-project-directory "README.md"))
         (path-project-license (cl-fad:merge-pathnames-as-file path-project-directory "LICENSE"))
         (path-file-already-licensed (cl-fad:merge-pathnames-as-file path-project-directory "already-licensed.lisp"))
         (license-name "null-license")
         (expected-notice-content (format nil "~a~%~a~%~a~%~%~a"
                                   "#| LICENSE NOTICE"
                                   "new-project (C) 2019 by your."
                                   "|#"
                                   "(list 1 2 3)"))
         (expected-readme-content (format nil "~a~%~%~a~%~%~a~%"
                                           "readme..."
                                           "### LICENSE"
                                           "new-project (C) 2019 by your."))
         (actual-content-1 "")
         (actual-content-2 "")
         (actual-content-3 "")
         (hash-markings (make-hash-table))
         (ignores '(".git/" ".conf/" "LICENSE" "README.md"))
         (result nil))
    (ensure-directories-exist path-directory)
    (ensure-directories-exist path-conf-directory)
    (ensure-directories-exist path-licenses-directory)
    (ensure-directories-exist path-notices-directory)
    (ensure-directories-exist path-project-directory)
    (ensure-directories-exist path-project-child-directory)
    (write-string-in-file path-license "not licensed.")
    (write-string-in-file path-notice "SK-PROJECT-NAME (C) SK-DATE-YEAR by SK-AUTHOR.")
    (write-string-in-file path-project-file-1 "(list 1 2 3)")
    (write-string-in-file path-project-file-2 "(list 1 2 3)")
    (write-string-in-file path-project-file-3 "(list 1 2 3)")
    (write-string-in-file path-project-readme "readme...")
    (write-string-in-file path-project-license "license...")
    (write-string-in-file path-file-already-licensed "|# haa LICENSE NOTICE haa... |#")
    (setf (gethash :SK-PROJECT-NAME hash-markings) "new-project")
    (setf (gethash :SK-AUTHOR hash-markings) "your")
    (setf (gethash :SK-DATE-YEAR hash-markings) "2019")
    (license-under
     path-project-directory
     path-licenses-directory
     license-name
     hash-markings
     ignores
     t
     t
     t)
    (setf actual-content-1 (get-string-from-file path-project-file-1))
    (setf actual-content-2 (get-string-from-file path-project-file-2))
    (setf actual-content-3 (get-string-from-file path-project-file-3))
    (setf result
          (and (string= expected-readme-content (get-string-from-file path-project-readme))
               (string= "not licensed." (get-string-from-file path-project-license))
               (string= "|# haa LICENSE NOTICE haa... |#" (get-string-from-file path-file-already-licensed))
               (every #'(lambda (i) (string= i expected-notice-content))
                      (list actual-content-1 actual-content-2 actual-content-3))))
    (delete-project-directory path-directory)
    result))

(suite "Suite license-under-test"
       (test "Test get-string-license" #'test-get-string-license)
       (test "Test create-license-file" #'test-create-license-file)
       (test "Test get-notice-path" #'test-get-notice-path)
       (test "Test write-license-notices" #'test-write-license-notices)
       (test "Test write-in-readme" #'test-write-in-readme)
       (test "Test write-in-readme-already-licensed" #'test-write-in-readme-already-licensed)
       (test "Test license-under" #'test-license-under))


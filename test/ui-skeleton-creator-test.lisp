(in-package #:cl-user)
(defpackage #:noloop.ui-skeleton-creator-test
  (:use #:common-lisp
        #:simplet)
  (:nicknames #:ui-skeleton-creator-test)
  (:import-from #:skeleton-creator
                #:conc
                #:write-string-in-file
                #:get-string-from-file
                #:set-configure-directory
                #:get-configure-directory
                #:configure-skeleton-creator
                #:create-new-project
                #:delete-project-directory
                #:license-project))
(in-package #:ui-skeleton-creator-test)

(defun test-create-new-project ()
  (let* ((destination-path "/tmp/test-create-new-project-ui/")
         (conf-dir (cl-fad:merge-pathnames-as-directory destination-path ".sk-conf/"))
         (conf-file (cl-fad:merge-pathnames-as-file conf-dir "skeleton-creator.conf"))
         (sk-dir (cl-fad:merge-pathnames-as-directory conf-dir "skeleton/"))
         (sk-file-1 (cl-fad:merge-pathnames-as-file sk-dir "SK-PROJECT-NAME-test.lisp"))
         (sk-file-2 (cl-fad:merge-pathnames-as-file sk-dir "README.md"))
         (sk-child-dir (cl-fad:merge-pathnames-as-directory sk-dir "skeleton-child-dir/"))
         (sk-file-3 (cl-fad:merge-pathnames-as-file sk-child-dir "SK-PROJECT-NAME.lisp"))
         (result nil))
    (ensure-directories-exist destination-path)
    (ensure-directories-exist conf-dir)
    (ensure-directories-exist sk-dir)
    (ensure-directories-exist sk-child-dir)
    (write-string-in-file conf-file "(:SK-AUTHOR \"you\" :SK-VERSION \"1.0.4\")")
    (write-string-in-file sk-file-1 "My SK-PROJECT-NAME in vSK-VERSION")
    (write-string-in-file sk-file-2 "SK-PROJECT-DESCRIPTION")
    (write-string-in-file sk-file-3 "My SK-PROJECT-NAME by SK-AUTHOR.")
    (set-configure-directory conf-dir)
    (create-new-project destination-path
                        "new-project"
                        "The project description."
                        :quiet t
                        :force t)
    (let* ((path-new-project
             (cl-fad:merge-pathnames-as-directory destination-path "new-project/"))
           (path-file-1
             (cl-fad:merge-pathnames-as-file path-new-project "new-project-test.lisp"))
           (path-file-2
             (cl-fad:merge-pathnames-as-file path-new-project "README.md"))
           (path-child-dir
             (cl-fad:merge-pathnames-as-directory path-new-project "skeleton-child-dir/"))
           (path-file-3
             (cl-fad:merge-pathnames-as-file path-child-dir "new-project.lisp")))
      (setf result (and (cl-fad:directory-exists-p path-new-project)
                        (cl-fad:file-exists-p path-file-1)
                        (cl-fad:file-exists-p path-file-2)
                        (cl-fad:directory-exists-p path-child-dir)
                        (cl-fad:file-exists-p path-file-3)
                        (string= "My new-project in v1.0.4" (get-string-from-file path-file-1))
                        (string= "The project description." (get-string-from-file path-file-2))
                        (string= "My new-project by you." (get-string-from-file path-file-3))
                        t)))
    (delete-project-directory destination-path)
    result))

(defun test-license-project ()
  (let* ((destination-path "/tmp/test-license-project/")
         (conf-dir (cl-fad:merge-pathnames-as-directory destination-path ".sk-conf/"))
         (conf-file (cl-fad:merge-pathnames-as-file conf-dir "skeleton-creator.conf"))
         (conf-licenses-dir (cl-fad:merge-pathnames-as-directory conf-dir "licenses/"))
         (conf-licenses-file (cl-fad:merge-pathnames-as-file conf-licenses-dir "null-license.txt"))
         (conf-notices-dir (cl-fad:merge-pathnames-as-directory conf-licenses-dir "notices/"))
         (conf-notice-file (cl-fad:merge-pathnames-as-file conf-notices-dir "null-license-notice.txt"))
         (sk-dir (cl-fad:merge-pathnames-as-directory conf-dir "skeleton/"))
         (sk-file-1 (cl-fad:merge-pathnames-as-file sk-dir "SK-PROJECT-NAME-test.lisp"))
         (sk-file-2 (cl-fad:merge-pathnames-as-file sk-dir "README.md"))
         (sk-child-dir (cl-fad:merge-pathnames-as-directory sk-dir "skeleton-child-dir/"))
         (sk-file-3 (cl-fad:merge-pathnames-as-file sk-child-dir "SK-PROJECT-NAME.lisp"))
         (license-notice (format nil "~a~%~a~%~a~%"
                                 "#| LICENSE NOTICE"
                                 "license notice..."
                                 "|#"))
         (result nil))
    (ensure-directories-exist destination-path)
    (ensure-directories-exist conf-dir)
    (ensure-directories-exist conf-licenses-dir)
    (ensure-directories-exist conf-notices-dir)
    (ensure-directories-exist sk-dir)
    (ensure-directories-exist sk-child-dir)
    (write-string-in-file conf-file "(:SK-AUTHOR \"you\" :SK-VERSION \"1.0.4\")")
    (write-string-in-file conf-licenses-file "not licensed.")
    (write-string-in-file conf-notice-file "license notice...")
    (write-string-in-file sk-file-1 "My SK-PROJECT-NAME in vSK-VERSION")
    (write-string-in-file sk-file-2 "SK-PROJECT-DESCRIPTION")
    (write-string-in-file sk-file-3 "My SK-PROJECT-NAME by SK-AUTHOR.")
    (set-configure-directory conf-dir)
    (create-new-project destination-path "new-project" "The project description." :quiet t :force t)
    (license-project
     (cl-fad:merge-pathnames-as-directory destination-path "new-project/")
     "null-license"
     :create-license-file-p t
     :write-license-notices-p t
     :write-in-readme-p t)
    (let ((expected-file-1 (format nil "~a~%~a"
                                   license-notice
                                   "My new-project in v1.0.4"))
          (expected-file-2 (format nil "~a~%~%~a~%~%~a~%"
                                   "The project description."
                                   "### LICENSE"
                                   "license notice..."))
          (expected-file-3 (format nil "~a~%~a"
                                   license-notice
                                   "My new-project by you."))
          (expected-file-license "not licensed.")
          (new-path-file-1 (cl-fad:merge-pathnames-as-file
                            destination-path
                            "new-project/new-project-test.lisp"))
          (new-path-file-2 (cl-fad:merge-pathnames-as-file
                            destination-path
                            "new-project/README.md"))
          (new-path-file-3 (cl-fad:merge-pathnames-as-file
                            destination-path
                            "new-project/skeleton-child-dir/new-project.lisp"))
          (new-path-file-license (cl-fad:merge-pathnames-as-file
                                  destination-path
                                  "new-project/LICENSE")))
      (setf
       result (and
               (string= expected-file-1 (get-string-from-file new-path-file-1))
               (string= expected-file-2 (get-string-from-file new-path-file-2))
               (string= expected-file-3 (get-string-from-file new-path-file-3))
               (string= expected-file-license (get-string-from-file new-path-file-license))  
               t)))
    (delete-project-directory destination-path)
    result))

(suite "Suite ui-skeleton-creator-test"
       (test "Test create-new-project" #'test-create-new-project)
       (test "Test license-project" #'test-license-project))

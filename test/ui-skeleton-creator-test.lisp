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
                #:delete-project-directory))
(in-package #:ui-skeleton-creator-test)

(defun test-create-new-project ()
  (let* ((conf-dir "/tmp/.sk-conf/")
         (conf-file "/tmp/.sk-conf/skeleton-creator.conf")
         (sk-dir "/tmp/.sk-conf/skeleton/")
         (sk-file-1 (conc sk-dir "PROJECT-NAME-test.lisp"))
         (sk-file-2 (conc sk-dir "README"))
         (sk-child-dir (conc sk-dir "skeleton-child-dir/"))
         (sk-file-3 (conc sk-child-dir "PROJECT-NAME.lisp"))
         (destination-path "/tmp/")
         (result nil))
    (ensure-directories-exist conf-dir)
    (ensure-directories-exist sk-dir)
    (ensure-directories-exist sk-child-dir)
    (write-string-in-file conf-file "(:AUTHOR \"you\" :VERSION \"1.0.4\")")
    (write-string-in-file sk-file-1 "My PROJECT-NAME in vVERSION")
    (write-string-in-file sk-file-2 "PROJECT-DESCRIPTION")
    (write-string-in-file sk-file-3 "My PROJECT-NAME by AUTHOR.")
    (set-configure-directory "/tmp/.sk-conf/")
    (create-new-project destination-path "new-project" "The project description." :quiet t)
    (setf result (and (cl-fad:directory-exists-p (conc destination-path "/new-project/"))
                      (cl-fad:file-exists-p (conc destination-path "new-project/new-project-test.lisp"))
                      (cl-fad:file-exists-p (conc destination-path "new-project/README"))
                      (cl-fad:directory-exists-p (conc destination-path "new-project/skeleton-child-dir/"))
                      (cl-fad:file-exists-p (conc destination-path "new-project/skeleton-child-dir/new-project.lisp"))
                      (string= "My new-project in v1.0.4" (get-string-from-file (conc destination-path "/new-project/new-project-test.lisp")))
                      (string= "The project description." (get-string-from-file (conc destination-path "/new-project/README")))
                      (string= "My new-project by you." (get-string-from-file (conc destination-path "/new-project/skeleton-child-dir/new-project.lisp")))
                      t))
    (delete-project-directory conf-dir)
    (delete-project-directory (cl-fad:merge-pathnames-as-directory destination-path #P"new-project/"))
    result))

(suite "Suite ui-skeleton-creator-test"
       (test "Test create-new-project" #'test-create-new-project))

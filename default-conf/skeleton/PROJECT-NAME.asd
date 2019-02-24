;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :PROJECT-NAME
  :author "AUTHOR <EMAIL>"
  :maintainer "AUTHOR <EMAIL>"
  :license "GPLv3"
  :version "VERSION"
  :homepage "https://GIT-SERVICE.com/AUTHOR/PROJECT-NAME"
  :bug-tracker "https://GIT-SERVICE.com/AUTHOR/PROJECT-NAME/issues"
  :source-control (:git "git@GIT-SERVICE.com:AUTHOR/PROJECT-NAME.git")
  :description "PROJECT-DESCRIPTION"
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "PROJECT-NAME" :depends-on ("package")))))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "PROJECT-NAME/test"))))

(defsystem :PROJECT-NAME/test
  :author "AUTHOR <EMAIL>"
  :maintainer "AUTHOR <EMAIL>"
  :license "GPLv3"
  :description "PROJECT-NAME Test."
  :depends-on (:PROJECT-NAME)
  :components ((:module "test"
                :components
                ((:file "PROJECT-NAME-test"))))
  :perform (test-op (op system) (funcall (read-from-string "PROJECT-NAME-test::run"))))

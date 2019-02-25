;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :SK-PROJECT-NAME
  :author "SK-AUTHOR <SK-EMAIL>"
  :maintainer "SK-AUTHOR <SK-EMAIL>"
  :license "SK-LICENSE"
  :version "SK-VERSION"
  :homepage "https://SK-GIT-SERVICE.com/SK-AUTHOR/SK-PROJECT-NAME"
  :bug-tracker "https://SK-GIT-SERVICE.com/SK-AUTHOR/SK-PROJECT-NAME/issues"
  :source-control (:git "git@SK-GIT-SERVICE.com:SK-AUTHOR/SK-PROJECT-NAME.git")
  :description "SK-PROJECT-DESCRIPTION"
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "SK-PROJECT-NAME" :depends-on ("package")))))
  :in-order-to ((test-op (test-op "SK-PROJECT-NAME/test"))))

(defsystem :SK-PROJECT-NAME/test
  :author "SK-AUTHOR <SK-EMAIL>"
  :maintainer "SK-AUTHOR <SK-EMAIL>"
  :license "SK-LICENSE"
  :description "SK-PROJECT-NAME Test."
  :depends-on (:SK-PROJECT-NAME)
  :components ((:module "test"
                :components
                ((:file "SK-PROJECT-NAME-test"))))
  :perform (test-op (op system) (funcall (read-from-string "SK-PROJECT-NAME-test::run"))))

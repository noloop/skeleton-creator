;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :skeleton-creator
  :author "noloop <noloop@zoho.com>"
  :maintainer "noloop <noloop@zoho.com>"
  :license "GNU General Public License v3.0"
  :version "0.0.0"
  :homepage "https://github.com/noloop/skeleton-creator"
  :bug-tracker "https://github.com/noloop/skeleton-creator/issues"
  :source-control (:git "git@github.com:noloop/skeleton-creator.git")
  :description "Create projects from a skeleton directory."
  :depends-on (:conf :cl-fad :cl-ppcre)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "skeleton-creator" :depends-on ("package")))))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "skeleton-creator/test"))))

(defsystem :skeleton-creator/test
  :author "noloop <noloop@zoho.com>"
  :maintainer "noloop <noloop@zoho.com>"
  :license "GNU General Public License v3.0"
  :description "skeleton-creator Test."
  :depends-on (:skeleton-creator)
  :components ((:module "test"
                :components
                ((:file "skeleton-creator-test"))))
  :perform (test-op (op system) (funcall (read-from-string "skeleton-creator-test::run"))))

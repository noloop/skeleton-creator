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
                 (:file "utils" :depends-on ("package"))
                 (:file "copy-directory" :depends-on ("package"))
                 (:file "skeleton-creator" :depends-on ("package" "utils" "copy-directory"))
                 (:file "ui-skeleton-creator" :depends-on ("skeleton-creator")))))
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
                ((:file "copy-directory-test")
                 (:file "skeleton-creator-test")
                 (:file "ui-skeleton-creator-test"))))
  :perform (test-op (op system) (progn (funcall (read-from-string "copy-directory-test::run"))
                                       (funcall (read-from-string "skeleton-creator-test::run"))
                                       (funcall (read-from-string "ui-skeleton-creator-test::run")))))

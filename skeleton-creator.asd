;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :skeleton-creator
  :author "noloop <noloop@zoho.com>"
  :maintainer "noloop <noloop@zoho.com>"
  :license "GPLv3"
  :version "1.0.0"
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
                 (:file "license-under" :depends-on ("skeleton-creator"))
                 (:file "ui-skeleton-creator" :depends-on ("skeleton-creator" "license-under")))))
  :in-order-to ((test-op (test-op "skeleton-creator/test"))))

(defsystem :skeleton-creator/test
  :author "noloop <noloop@zoho.com>"
  :maintainer "noloop <noloop@zoho.com>"
  :license "GPLv3"
  :description "skeleton-creator Test."
  :depends-on (:skeleton-creator :simplet)
  :defsystem-depends-on (:simplet-asdf)
  :components ((:module "test"
                :components
                ((:test-file "copy-directory-test")
                 (:test-file "skeleton-creator-test")
                 (:test-file "license-under-test")
                 (:test-file "ui-skeleton-creator-test"))))
  :perform (test-op (op c) (symbol-call :simplet '#:run)))

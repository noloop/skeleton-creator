(in-package #:cl-user)
(defpackage #:AUTHOR.PROJECT-NAME-test
  (:use #:common-lisp)
  (:nicknames #:PROJECT-NAME-test)
  (:import-from #:PROJECT-NAME
                #:FORM1
                #:FORM2))
(in-package #:AUTHOR.PROJECT-NAME-test)


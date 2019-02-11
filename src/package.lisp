(defpackage #:noloop.skeleton-creator
       (:use #:common-lisp)
       (:nicknames #:skeleton-creator)
       (:import-from #:conf
                     #:init-conf
                     #:get-conf-hash)
       (:import-from #:copy-directory
                     #:copy-directory)
       (:export #:init-skeleton-creator
                #:set-field
                #:get-field
                #:set-configure-directory
                #:get-configure-directory
                #:configure-skeleton-creator
                #:replace-markings-in-file-names
                #:replace-markings-in-file
                #:create-new-project
                #:delete-project-directory))

(defpackage #:noloop.ui-skeleton-creator
  (:use #:common-lisp)
  (:nicknames #:ui-skeleton-creator)
  (:import-from #:skeleton-creator
                #:init-skeleton-creator
                #:set-configure-directory
                #:get-configure-directory
                #:configure-skeleton-creator
                #:create-new-project
                #:delete-project-directory)
  (:export #:ui-skeleton-creator))


(defpackage #:noloop.skeleton-creator
       (:use #:common-lisp)
       (:nicknames #:skeleton-creator)
       (:import-from #:conf
                     #:init-conf
                     #:get-conf-hash
                     #:set-conf-directory
                     #:get-conf-directory)
       (:import-from #:copy-directory
                     #:copy-directory)
       (:export #:init-skeleton-creator
                #:set-field
                #:get-field
                #:set-conf-dir
                #:get-conf-dir
                #:conf-skeleton-creator
                #:replace-markings-in-file-names
                #:replace-markings-in-file
                #:create-project
                #:delete-project))

(defpackage #:noloop.ui-skeleton-creator
  (:use #:common-lisp)
  (:nicknames #:ui-skeleton-creator)
  (:import-from #:skeleton-creator
                #:init-skeleton-creator
                #:set-conf-dir
                #:get-conf-dir
                #:conf-skeleton-creator
                #:create-project
                #:delete-project)
  (:export #:set-configure-directory
           #:get-configure-directory
           #:configure-skeleton-creator
           #:create-new-project
           #:delete-project-directory))


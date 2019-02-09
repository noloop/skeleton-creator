(defpackage #:noloop.skeleton-creator
       (:use #:common-lisp)
       (:nicknames #:skeleton-creator)
       (:import-from #:conf
                     #:init-conf
                     #:get-conf-hash)
       (:export #:set-field
                #:get-field
                #:set-configure-directory
                #:get-configure-directory
                #:configure-skeleton-creator
                #:replace-markings-in-file-names
                #:replace-markings-in-file
                #:create-new-project
                #:delete-project-directory))

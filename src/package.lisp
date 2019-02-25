(defpackage #:noloop.skeleton-creator
       (:use #:common-lisp)
       (:nicknames #:skeleton-creator)
       (:import-from #:conf
                     #:init-conf
                     #:set-conf-directory
                     #:get-conf-directory
                     #:replace-conf
                     #:get-conf-hash)
       (:export #:set-configure-directory
                #:get-configure-directory
                #:configure-skeleton-creator
                #:create-new-project
                #:delete-project-directory
                #:license-project))



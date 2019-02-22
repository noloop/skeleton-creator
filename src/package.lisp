(defpackage #:noloop.skeleton-creator
       (:use #:common-lisp)
       (:nicknames #:skeleton-creator)
       (:import-from #:conf
                     #:init-conf
                     #:get-conf-hash
                     #:set-conf-directory
                     #:get-conf-directory)
       (:export #:set-configure-directory
                #:get-configure-directory
                #:configure-skeleton-creator
                #:create-new-project
                #:delete-project-directory
                ;;#:license-under
                ))



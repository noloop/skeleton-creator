(in-package #:noloop.skeleton-creator)

(let ((sk (init-skeleton-creator "~/.config/skeleton-creator-conf/")))
  
  (defun set-configure-directory (new-directory)
    (set-conf-dir sk new-directory))
  
  (defun get-configure-directory ()
    (get-conf-dir sk))

  (defun configure-skeleton-creator ()
    (require-conf-directory-interactive
     #'(lambda ()
         (format t "~%~a~%" "- SKELETON CREATOR CONFIGURATION OF skeleton-creator.conf -")
         (format t "~%~a" "Do you want to edit the newly created skeleton-creator configuration file?")
         (yes-or-no?-fn
          #'(lambda ()
              (conf-skeleton-creator sk))))))    
    
  (defun create-new-project (&key force)
    (require-conf-directory-interactive
     #'(lambda ()
         (configure-skeleton-creator)
         (create-new-project-interactive force))))

  (defun license-project ()
    (require-conf-directory-interactive
     #'(lambda ()
         (let ((notices-dir-exists-p
                 (cl-fad:directory-exists-p (cl-fad:merge-pathnames-as-directory
                                             (get-configure-directory)
                                             "licenses/notices/"))))
               (license-project-interactive notices-dir-exists-p)))
     :check-license-dir-p t))

  (defun delete-project-directory (project-directory)
    (format t "~%~a ~a?~%" "Are you sure you want to delete the directory" project-directory)
    (if (yes-or-no?)
        (progn (delete-project project-directory)
               (format t "~%~a~%" "Directory deleted successfully!"))
        (format t "~%~a~%" "Nothing was excluded.")))

  (defun create-new-project-interactive (force)
    (format t "~%")
    (let ((destination (read-field "Destination directory: "))
          (name (read-field "Project name: "))
          (description (read-field "Project description: ")))
      (if (confirm-changes-p
           (list "Destination directory:" "Project name:" "Project description:")
           (list destination name description))       
          (create-new-project-not-interactive destination
                                              name
                                              description
                                              :force force)
          (format t "~%~a~%" "Nothing changed!"))))

  (defun license-project-interactive (notices-dir-exists-p)
    (let* ((project-directory (read-field "Project directory to be licensed: "))
           (license-name (read-field (format nil "License Name Used~a: " (get-license-names))))
           (create-license-file-p (to-ask "Create LICENSE file?"))
           (write-license-notices-p (if notices-dir-exists-p
                                        (to-ask "Write license notice in the files?")
                                        nil))
           (string-ignores (if write-license-notices-p
                               (read-field "You can ignore files and/or directories when writing the license notice on files. Accepts quoted names separated by space.(default: \"README.md\" \"LICENSE\" \".git/\"): ")
                               "\"README.md\" \"LICENSE\" \".git/\""))
           (ignores (if (string-empty-p string-ignores)
                        "\"README.md\" \"LICENSE\" \".git/\""
                        string-ignores))
           (write-in-readme-p (if notices-dir-exists-p
                                  (to-ask "Write license notice in the file README.md?")
                                  nil)))
      (if (confirm-changes-p (list "Project directory to be licensed:"
                                   "License Name Used:"
                                   "Create LICENSE file?"
                                   "Write license notice in the files?"
                                   "ignores:"
                                   "Write license notice in the file README.md?")
                             (list project-directory
                                   license-name
                                   create-license-file-p
                                   write-license-notices-p
                                   ignores
                                   write-in-readme-p))
          (license-project-not-interactive (pathname project-directory)
                                           license-name
                                           :create-license-file-p create-license-file-p
                                           :write-license-notices-p write-license-notices-p
                                           :ignores (read-from-string (conc "(" ignores ")"))
                                           :write-in-readme-p write-in-readme-p)
          (format t "~%~a~%" "Nothing changed!"))))

  (defun get-license-names ()
    (remove-if #'null
               (mapcar
                #'(lambda (i)
                    (if (pathname-is-file i)
                        (pathname-name i)))
                (cl-fad:list-directory (cl-fad:merge-pathnames-as-directory
                                        (get-configure-directory)
                                        "licenses/")))))
  
  (defun require-conf-directory-interactive (fn &key check-license-dir-p)
    (let ((conf-dir-exists-p
            (cl-fad:directory-exists-p (get-configure-directory)))
          (conf-file-exists-p
            (cl-fad:file-exists-p (cl-fad:merge-pathnames-as-file
                                   (get-configure-directory)
                                   "skeleton-creator.conf")))
          (skeleton-dir-exists-p
            (cl-fad:directory-exists-p (cl-fad:merge-pathnames-as-directory
                                        (get-configure-directory)
                                        "skeleton/")))
          (license-dir-exists-p
            (if check-license-dir-p
                (cl-fad:directory-exists-p (cl-fad:merge-pathnames-as-directory
                                            (get-configure-directory)
                                            "licenses/"))
                t))
          (msg-conf-dir-not-existing "Configuration directory not existing!")
          (msg-conf-file-not-existing "Configuration file(skeleton-creator.conf) not existing in the configuration directory.")
          (msg-skeleton-not-existing "Skeleton directory(skeleton/) not existing in the configuration directory.")
          (msg-licenses-not-existing "The directory licenses/ or license/notices/ not existing in the configuration directory."))
      (if conf-dir-exists-p
          (if conf-file-exists-p
              (if skeleton-dir-exists-p
                  (if (if check-license-dir-p license-dir-exists-p t)
                      (funcall fn)
                      (y-or-n-default-conf msg-licenses-not-existing fn))
                  (y-or-n-default-conf msg-skeleton-not-existing fn))
              (y-or-n-default-conf msg-conf-file-not-existing fn))
          (y-or-n-default-conf msg-conf-dir-not-existing fn))))
  
  (defun y-or-n-default-conf (warning-msg fn)
    (if (use-default-conf-p warning-msg)      
        (progn (copy-default-conf)
               (funcall fn)
               (format t "~%"))
        (format t "~%~a~%" "No project directory was created! Configure a configuration directory, Use the set-configure-directory and get-configure-directory functions to do this. Your configuration directory should look like this:

skeleton-creator-conf/     ; or whatever name you want
    skeleton/              ; the skeleton cloned while creating your projects
    license/               ; your license files, the files should have type .txt
        notices/           ; your notices files, the files should have type .txt
    skeleton-creator.conf  ; skeleton-creator configuration file")))

  (defun use-default-conf-p (warning-msg)
    (format t "~a~%~%~a "
            warning-msg
            "Do you want to use the skeleton-creator default configuration directory?")
    (if (yes-or-no?) t nil))

  (defun copy-default-conf ()
    (let* ((destination
             (read-field "Configuration directory destination: "))
           (conf-dir-destination
             (cl-fad:merge-pathnames-as-directory destination "skeleton-creator-conf/")))
      (ensure-directories-exist conf-dir-destination)
      (set-configure-directory conf-dir-destination)
      (copy-directory (cl-fad:merge-pathnames-as-directory
                       (asdf:system-source-directory :skeleton-creator)
                       "default-conf/")
                      conf-dir-destination
                      :overwrite t)))

  (defun confirm-changes-p (list-fields list-values)
    (format t "~%~a" "!!!CONFIRM CHANGES!!!")
    (do ((i 0 (incf i))
         (fields list-fields (cdr fields))
         (values list-values (cdr values)))
        ((<= (length list-fields) i)
         (progn (format t "~%~%~a" "Do you want to complete the above changes?")
                (yes-or-no?)))
      (format t "~%~a ~a" (car fields) (car values))))
  
  (defun create-new-project-not-interactive (destination-directory name description
                                             &key quiet
                                                  force)
    (let* ((path-project
             (cl-fad:merge-pathnames-as-directory
              destination-directory
              (conc name "/")))
           (existing-directory-p
             (cl-fad:directory-exists-p path-project)))
      (handler-case
          (if (and existing-directory-p (not force))
              (if (not quiet)
                  (format t "~a" "Project NOT created successfully! The directory already exists."))
              (progn (create-project sk destination-directory name description)
                     (if (not quiet)
                         (format t "~a" "Project created successfully."))))
        (error (c)
          (declare (ignore c))
          (if (empty-directory-p path-project)
              (delete-project-directory path-project))
          (if (not quiet)
              (format t "~a" "The project was NOT created successfully!"))))))

  (defun license-project-not-interactive (project-directory license-name
                                          &key (ignores '("README.md"
                                                          "LICENSE"
                                                          ".git/"))
                                               (create-license-file-p t)
                                               (write-license-notices-p nil)
                                               (write-in-readme-p nil)
                                               quiet)
    (handler-case
        (progn (license-under project-directory
                              (cl-fad:merge-pathnames-as-directory
                               (get-conf-dir sk)
                               "licenses/")
                              license-name
                              (get-hash-markings sk)
                              ignores
                              create-license-file-p
                              write-license-notices-p
                              write-in-readme-p)
               (if (not quiet)
                   (format t "~a" "Project licensed successfully.")))
      (error (c)
        (declare (ignore c))
        (if (not quiet)
            (format t "~a" "The project was NOT licensed successfully!"))))))
  

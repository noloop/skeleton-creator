(in-package #:noloop.skeleton-creator)

(let ((sk (init-skeleton-creator "~/.config/skeleton-creator-conf/")))
  
  (defun set-configure-directory (new-directory)
    (set-conf-dir sk new-directory))
  
  (defun get-configure-directory ()
    (get-conf-dir sk))

  (defun configure-skeleton-creator ()
    (conf-skeleton-creator sk))

  (defun create-new-project (destination-directory name description &key quiet)
    (let ((existing-directory (cl-fad:directory-exists-p
                               (concatenate 'string destination-directory name "/"))))
      (handler-case (if existing-directory
                        (if (not quiet)
                            (format t "~a" "Project NOT created successfully! The directory already exists."))
                        (progn (create-project sk destination-directory name description)
                               (if (not quiet)
                                   (format t "~a" "Project created successfully."))))
        (error (c)
          (declare (ignore c))
          (if existing-directory
              (delete-project-directory
               (concatenate 'string destination-directory name "/")))
          (if (not quiet)
              (format t "~a" "The project was NOT created successfully!"))))))

  (defun delete-project-directory (project-directory)
    (delete-project project-directory)))

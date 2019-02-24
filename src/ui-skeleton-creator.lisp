(in-package #:noloop.skeleton-creator)

(let ((sk (init-skeleton-creator "~/.config/skeleton-creator-conf/")))
  
  (defun set-configure-directory (new-directory)
    (set-conf-dir sk new-directory))
  
  (defun get-configure-directory ()
    (get-conf-dir sk))

  (defun configure-skeleton-creator ()
    (conf-skeleton-creator sk))

  (defun create-new-project (destination-directory name description &key quiet force)
    (let* ((path-project
            (cl-fad:merge-pathnames-as-directory
             destination-directory
             (conc name "/")))
          (existing-directory
            (cl-fad:directory-exists-p path-project)))
      (handler-case
          (if (or existing-directory (not force))
              (if (not quiet)
                  (format t "~a" "Project NOT created successfully! The directory already exists."))
              (progn (create-project sk destination-directory name description)
                     (if (not quiet)
                         (format t "~a" "Project created successfully."))))
        (error (c)
          (declare (ignore c))
          (if existing-directory
              (delete-project-directory path-project))
          (if (not quiet)
              (format t "~a" "The project was NOT created successfully!"))))))

  (defun license-project (project-directory
                          license-name
                          &key (ignores '("README.md"
                                          "LICENSE"
                                          ".git/"))
                               (create-license-file-p t)
                               (write-license-notices-p nil)
                               (write-in-readme-p nil))
    (license-under project-directory
                   (cl-fad:merge-pathnames-as-directory
                    (get-conf-dir sk)
                    "licenses/")
                   license-name
                   (get-hash-markings sk)
                   ignores
                   create-license-file-p
                   write-license-notices-p
                   write-in-readme-p))

  (defun delete-project-directory (project-directory)
    (delete-project project-directory)))

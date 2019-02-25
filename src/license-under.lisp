(in-package #:noloop.skeleton-creator)

(defun license-under (project-directory
                      licenses-directory
                      license-name
                      hash-markings
                      ignores
                      create-license-file-p
                      write-license-notices-p
                      write-in-readme-p)
  (let ((notices-directory (cl-fad:merge-pathnames-as-directory
                            licenses-directory
                            "notices/")))
    (if (null license-name)
        (return-from license-under "No license was specified. Configure in your skeleton-creator.conf file or pass the key argument :license-name with the license name. Read the documentation for see the available names."))
    (if create-license-file-p
        (create-license-file project-directory licenses-directory license-name))
    (if write-license-notices-p
        (write-license-notices project-directory notices-directory license-name hash-markings ignores))
    (if write-in-readme-p
        (write-in-readme project-directory notices-directory license-name hash-markings))
    t))
  
(defun create-license-file (project-directory licenses-directory license-name)
  (write-string-in-file (cl-fad:merge-pathnames-as-file
                         project-directory
                         "LICENSE")
                        (get-string-license licenses-directory license-name)))

(defun get-string-license (licenses-directory license-name)
  (get-string-from-file (cl-fad:merge-pathnames-as-file
                         licenses-directory
                         (conc (string-downcase license-name)
                               ".txt"))))

(defun write-license-notices (project-directory notices-directory license-name hash-markings ignores)
  (walk-destination-directory
   project-directory
   #'(lambda (path)
       (if (and (pathname-is-file path)
                (not (search "LICENSE NOTICE" (get-string-from-file path))))
           (let* ((path-notice (get-notice-path notices-directory license-name))
                  (new-notice (file-string-replace-markings
                               path-notice
                               hash-markings))
                  (new-string-file
                    (format nil "~a~%~a~%~a~%~%~a"
                            "#| LICENSE NOTICE"
                            new-notice
                            "|#"
                            (get-string-from-file path))))  
             (write-string-in-file path new-string-file))))
   ignores))

(defun write-in-readme (project-directory notices-directory license-name hash-markings)
  (let ((path-readme (cl-fad:merge-pathnames-as-file
                      project-directory
                      "README.md")))
    (if (not (search "### LICENSE" (get-string-from-file path-readme)))
        (let* ((path-notice (get-notice-path notices-directory license-name))
               (new-notice (file-string-replace-markings
                            path-notice
                            hash-markings))
               (new-string
                 (format nil "~a~%~%~a~%~%~a~%"
                         (get-string-from-file path-readme)
                         "### LICENSE"
                         new-notice)))
          (write-string-in-file path-readme new-string)))))

(defun get-notice-path (notices-directory license-name)
  (cl-fad:merge-pathnames-as-file
   notices-directory
   (conc (string-downcase license-name)
         "-notice.txt")))

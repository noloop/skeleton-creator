(in-package #:noloop.skeleton-creator)

;; (defun license-under (project-directory
;;                         licenses-directory
;;                         license-name
;;                         &key (create-license-file-p t)
;;                              (write-license-notices-p t))
;;   (if (null license-name)
;;       (return-from license-project "No license was specified. Configure in your skeleton-creator.conf file or pass the key argument :license-name with the license name. Read the documentation for see the available names."))
;;   (if create-license-file-p
;;       (create-license-file project-directory licenses-directory license-name))
;;   (if write-license-notices-p
;;       (write-license-notices project-directory license-name))
;;   (if write-in-readme-p
;;       (write-in-readme project-directory license-name)))
  
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

(defun get-notice-path (notices-directory license-name)
  (cl-fad:merge-pathnames-as-file
   notices-directory
   (conc (string-downcase license-name)
         "-notice.txt")))

;; (search "LICENSE NOTICE" (get-string-from-file stg))
#| LICENSE NOTICE

new-project (C) 2019 by your.
|#

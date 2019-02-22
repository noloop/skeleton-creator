(in-package #:noloop.skeleton-creator)

(defun init-skeleton-creator (conf-directory)
  (reverse (pairlis (list :conf
                          :project-destination-directory
                          :project-name
                          :project-description
                          :replace-ignore)
                    (list (init-conf conf-directory "skeleton-creator.conf")
                          "/tmp/"
                          "PROJECT-NAME"
                          "PROJECT-DESCRIPTION"
                          '(".git/")))))

(defun set-field (skeleton-creator field new-value)
  (setf (cdr (assoc field skeleton-creator)) new-value))

(defun get-field (skeleton-creator field)
  (cdr (assoc field skeleton-creator)))

(defun set-conf-dir (skeleton-creator new-directory)
  (set-conf-directory (get-field skeleton-creator :conf) new-directory))

(defun get-conf-dir (skeleton-creator)
  (get-conf-directory (get-field skeleton-creator :conf)))

;;; !!!!!!!lembrar de copiar diretorio do proprio repositorio chamado default-conf-directory dando opcao de copiar ou o usuario criar o seu
(defun conf-skeleton-creator (skeleton-creator)
  (format t "SKELETON CREATOR CONFIGURATION~%")
  (format t "I will ask you some questions to create/modify 
the skeleton-creator configuration file, you can answer them, 
or leave empty to keep the previous configuration or default/actual configuration.~%")
  (is-ok? (replace-conf (get-field skeleton-creator :conf))))

(defun create-project (skeleton-creator destination-directory name description)
  (let ((path-project (cl-fad:merge-pathnames-as-directory
                       destination-directory
                       (concatenate 'string name "/"))))
    (ensure-directories-exist path-project)
    (copy-skeleton-directory skeleton-creator path-project)
    (set-field skeleton-creator :project-destination-directory path-project)
    (set-field skeleton-creator :project-name name)
    (set-field skeleton-creator :project-description description)
    (replace-markings skeleton-creator path-project)))

(defun copy-skeleton-directory (skeleton-creator destination-directory)
  (copy-directory
   (concatenate 'string (get-conf-dir skeleton-creator) "skeleton/")
   destination-directory
   :overwrite t))

(defun replace-markings (skeleton-creator destination-directory)
  "1 - Replace the file names with the markings values.
2 - Replace the strings within the contents of the files with markings values.
Markings are PROJECT-NAME and PROJECT-DESCRIPTION and all elements of the skeleton.conf configuration file."
  (let ((ignores (get-field skeleton-creator :replace-ignore))
        (hash-markings (alexandria:copy-hash-table
                        (get-conf-hash (get-field skeleton-creator :conf)))))
    (setf (gethash :PROJECT-NAME hash-markings) (get-field skeleton-creator :project-name))
    (setf (gethash :PROJECT-DESCRIPTION hash-markings) (get-field skeleton-creator :project-description))
    (replace-markings-in-file-names destination-directory hash-markings ignores)
    (replace-markings-in-file destination-directory hash-markings ignores)))

(defun replace-markings-in-file-names (destination-directory hash-markings &optional (ignores '()))
  (walk-destination-directory
   destination-directory
   #'(lambda (el)
       (let* ((key (string-match-markings (pathname-name el) hash-markings))
              (new-value (gethash key hash-markings)))
         (if (and (pathname-is-file el)
                  (not (null new-value)))
               (rename-file el (merge-path-with-new-file-name
                               el
                               (string-replace-all (namestring (pathname-name el)) (string key) new-value))))))
   ignores))

(defun merge-path-with-new-file-name (path new-file-name)
  "Merge new-file-name with directory of path, return new path."
  (let ((path-type (pathname-type path)))
    (cl-fad:merge-pathnames-as-file
     (cl-fad:pathname-directory-pathname path)
     (concatenate 'string
                  new-file-name
                  (if (not (null path-type)) ".")
                  path-type))))

(defun string-match-markings (stg hash-markings)
  (maphash #'(lambda (key value)
               (if (search (string key) stg)
                   (return-from string-match-markings key)))
           hash-markings))

(defun replace-markings-in-file (destination-directory hash-markings &optional (ignores '()))
  (walk-destination-directory
   destination-directory
   #'(lambda (path)
       (if (pathname-is-file path)
           (write-string-in-file path (file-string-replace-markings path hash-markings))))
   ignores))

(defun file-string-replace-markings (path hash-markings)
  "Return new string from string file after replace markings."
  (let ((new-stg (get-string-from-file path)))
    (maphash #'(lambda (key value)
                 (setf new-stg (string-replace-all new-stg (string key) (string value))))
             hash-markings)
    new-stg))

(defun walk-destination-directory (destination-directory fn &optional (ignores '()))
  (cl-fad:walk-directory destination-directory fn
                         :directories :BREADTH-FIRST
                         :follow-symlinks nil
                         :test (not-match-list-ignore destination-directory ignores)))

(defun not-match-list-ignore (destination-directory ignores)
  "Returns a lambda that checks whether an element(el) merged to a destination-directory is equal to some element of the ignore list."
    (lambda (el)
      (dolist-ignores el destination-directory ignores)))

(defun dolist-ignores (path destination ignores)
  (let ((result t))
    (dolist (i ignores)
      (if (cl-fad:pathname-equal path (concatenate 'string (namestring destination) i))
          (progn (setf result nil) (return))
          (setf result t)))
    result))

(defun delete-project (project-directory)
  (cl-fad:delete-directory-and-files project-directory))


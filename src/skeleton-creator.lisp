(in-package #:noloop.skeleton-creator)
#|
;;(init-conf "/home/noloop/lisp/portacle/projects/test-git/" "skeleton-creator.conf")

(replace-all "PROJECT-NAME" "skeleton-creator" ";;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :PROJECT-NAME
  :author \"noloop <noloop@zoho.com>\"
  :maintainer \"noloop <noloop@zoho.com>\"
  :license \"GNU General Public License v3.0\"
  :version \"0.0.0\"
  :homepage \"https://github.com/noloop/PROJECT-NAME\"
  :bug-tracker \"https://github.com/noloop/PROJECT-NAME/issues\"
  :source-control (:git \"git@github.com:noloop/PROJECT-NAME.git\")
  :description \"PROJECT-DESCRIPTION.\"
  :components ((:module \"src\"
                :components
                ((:file \"package\")
                 (:file \"file1\" :depends-on (\"package\"))
                 (:file \"file2\" :depends-on (\"package\" \"file1\")))))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* \"README.md\"))
  :in-order-to ((test-op (test-op \"PROJECT-NAME/test\"))))\"
")

((lambda (el)
   (if (not (null (pathname-is-file el)))
       (let* ((old-stg (get-string-from-file el))
              (new-stg old-stg))
         (maphash #'(lambda (key value)
                      (setf new-stg (string-replace-all new-stg (string value) (string key))))
                  *hash*)
         (write-string-in-file el new-stg))))
 "/home/noloop/lisp/portacle/projects/test-git/skeleton-creator.conf")
|#
(defun init-skeleton-creator (directory)
  (reverse (pairlis (list :conf
                          :project-destination-directory
                          :project-name
                          :project-description
                          :replace-ignore)
                    (list (init-conf directory "skeleton-creator.conf")
                          "PROJECT-NAME"
                          "DESCRIPTION"
                          "/tmp/"
                          '(".git/")))))

(defun set-field (skeleton-creator field new-value)
  (setf (cdr (assoc field skeleton-creator)) new-value))

(defun get-field (skeleton-creator field)
  (cdr (assoc field skeleton-creator)))

(defun set-configure-directory (skeleton-creator new-directory)
  (set-conf-directory (get-field skeleton-creator :conf) new-directory))

(defun get-configure-directory (skeleton-creator new-directory)
  (get-conf-directory (get-field skeleton-creator :conf)))

(defun configure-skeleton-creator (skeleton-creator)
  (format t "SKELETON CREATOR CONFIGURATION~%")
  (format t "I will ask you some questions to create/modify 
the skeleton-creator configuration file, you can answer them, 
or leave empty to keep the previous configuration or default configuration.~%")
  (is-ok? (replace-conf (get-field skeleton-creator :conf))))

(defun is-ok? (fn)
  (format t "IS OK?(yes/no)~%")
  (if (string-equal "yes" (read))
      fn
      nil))

(defun create-new-project (skeleton-creator destination-directory name description)
  (copy-skeleton-directory destination-directory)
  (set-field skeleton-creator :project-destination-directory destination-directory)
  (set-field skeleton-creator :project-name name)
  (set-field skeleton-creator :project-description description)
  (replace-markings skeleton-creator destination-directory))

(defun copy-skeleton-directory (destination-directory)
  ;;também copiar no windows e mac(comandos)
  (uiop:run-program '("cp -r" (concatenate 'string
                               (concatenate 'string (get-conf-directory conf) "skeleton/*")
                               destination-directory))))
 
(defun replace-markings (skeleton-creator destination-directory)
  ";;procurar marcacoes de PROJECT-NAME/DESCRIPTION dentro de todos os arquivos recursivamente
  ;;substituir marcacoes encontradas por name e description
  ;;procurar todas marcacoes das keys de conf-hash em todos os arquivos recursivamente
  ;;substiuir marcacoes encontradas pelos valores de conf-hash"
  (let ((ignores (get-field skeleton-creator :replace-ignore))
        (hash-markings (alexandria:copy-hash-table
                        (get-conf-hash (get-field skeleton-creator :conf)))))
    (setf (gethash :PROJECT-NAME hash-markings) (get-field skeleton-creator :project-name))
    (setf (gethash :DESCRIPTION hash-markings) (get-field skeleton-creator :project-description))
    ;;(replace-markings-in-file-names destination-directory hash-markings ignores)
    ;;(replace-markings-in-file destination-directory hash-markings ignores)
    ))

(defun replace-markings-in-file-names (destination-directory hash-markings &optional (ignores '()))
  (walk-destination-directory
   destination-directory
   #'(lambda (el)
       (let ((new-file-name (string-match-markings (pathname-name el) hash-markings)))
         (if (and (pathname-is-file el)
                  (not (null new-file-name)))
             (rename-file el (merge-path-with-new-file-name el new-file-name)))))
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
               (if (string= stg (string key))
                   (return-from string-match-markings value)))
           hash-markings))

#|
(defun replace-markings-in-file (destination-directory hash-markings &optional (ignores '()))
  (walk-destination-directory
   #'(lambda (el)
       (if (not (null (pathname-is-file el)))
           (let* ((old-stg (get-string-from-file el))
                  (new-stg old-stg))
             (maphash #'(lambda (key value)
                          (setf new-stg (string-replace-all old-stg (string key) (string value))))
                      hash-markings)
             (write-string-in-file el new-stg))))
   ignores))
|#
(defun pathname-is-file (path)
  (and (not (cl-fad:directory-exists-p path))
       (cl-fad:file-exists-p path)))

(defun get-string-from-file (file-name)
  (with-open-file (stream file-name)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun write-string-in-file (file-name stg)
  (with-open-file
      (stream file-name
              :direction :output
              :if-exists :supersede
              :if-does-not-exist :create)
    (format stream stg)))

(defun string-replace-all (stg old new)
  (let* ((cl-ppcre:*allow-quoting* t)
         (old (concatenate 'string "\\Q" old)))
    (cl-ppcre:regex-replace-all old stg new)))

(defun walk-destination-directory (destination-directory fn &optional (ignores '()))
  (cl-fad:walk-directory destination-directory fn
                  :directories :BREADTH-FIRST
                  :test (not-match-list-ignore destination-directory ignores)))

(defun not-match-list-ignore (destination-directory ignores)
  "Returns a lambda that checks whether an element(el) merged to a destination-directory is equal to some element of the ignore list."
    (lambda (el)
      (dolist-ignores el destination-directory ignores)))

(defun dolist-ignores (path destination ignores)
  (let ((result t))
    (dolist (i ignores)
      (if (cl-fad:pathname-equal path (concatenate 'string destination i))
          (progn (setf result nil) (return))
          (setf result t)))
    ;(format t "~a - ~a~%" path result)
    result))

;;(defun not-pathname-equal (path-1 path-2) (not (cl-fad:pathname-equal (pathname path-1) (pathname path-2))))

(defun delete-project-directory (project-directory)
  (cl-fad:delete-directory-and-files project-directory))

;;;(defun license-under-unlicense())
;;;(defun license-under-cc0())
;;;(defun license-under-gplv3()) buscar no diretorio licenses a license configurada no arquivo conf
;;;criar arquivo LICENSE com a license escolhida
;;;substituir as marcacoes na string de aviso para a licensa que deve estar em cada arquivo
;;;acrescentar no inicio de todos os arquivos #| aviso license|# com execeção dos arquivos LICENSE(s)
;;;acrescentar no fim dos arquivos README.md(s) um topico "### LICENSE" copiando o aviso de licensa para ele


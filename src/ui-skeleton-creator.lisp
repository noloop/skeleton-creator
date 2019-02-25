(in-package #:noloop.skeleton-creator)

(let ((sk (init-skeleton-creator "~/.config/skeleton-creator-conf/")))
  
  (defun set-configure-directory (new-directory)
    (set-conf-dir sk new-directory))
  
  (defun get-configure-directory ()
    (get-conf-dir sk))

  (defun configure-skeleton-creator ()
    (conf-skeleton-creator sk))

  (defun create-new-project (&key quiet force use-default-conf-p)
    (if use-default-conf-p
        (create-new-project-interactive quiet force use-default-conf-p)
        (let ((conf-dir-exists-p
                (cl-fad:directory-exists-p (get-configure-directory)))
              (conf-file-exits-p
                (cl-fad:file-exists-p
                 (cl-fad:merge-pathnames-as-file
                  (get-configure-directory)
                  "skeleton-creator.conf")))
              (skeleton-dir-exists-p
                (cl-fad:directory-exists-p
                 (cl-fad:merge-pathnames-as-directory
                  (get-configure-directory)
                  "skeleton/")))
              (msg-conf-dir-not-existing "Configuration directory not existing.")
              (msg-conf-file-not-existing "Configuration file(skeleton-creator.conf) not existing in the configuration directory.")
              (msg-skeleton-not-existing "Skeleton directory(skeleton/) not existing in the configuration directory."))
          (if conf-dir-exists-p
              (if conf-file-exits-p
                  (if skeleton-dir-exists-p
                      (create-new-project-interactive quiet force nil)
                      (y-or-n-default-conf msg-skeleton-not-existing quiet force use-default-conf-p))
                  (y-or-n-default-conf msg-conf-file-not-existing quiet force use-default-conf-p))
              (y-or-n-default-conf msg-conf-dir-not-existing quiet force use-default-conf-p)))))
  
  (defun y-or-n-default-conf (warning-msg quiet force use-default-conf-p)
    (if (use-default-conf-p warning-msg)
        (progn (copy-default-conf)
               (configure-skeleton-creator)
               (format t "~%~%")
               (create-new-project-interactive quiet force use-default-conf-p))
        (format t "~%~a~%" "No project directory was created! Configure a configuration directory that has at least one skeleton/ directory and a skeleton-creator.conf file inside it. Use the set-configure-directory and get-configure-directory functions to do this.")))

  (defun create-new-project-interactive (quiet force use-default-conf-p)
    (if use-default-conf-p
        (progn (copy-default-conf)
               (configure-skeleton-creator)
               (format t "~%~%")))
    (let ((destination (read-field "Destination directory: "))
          (name (read-field "Project name: "))
          (description (read-field "Project description: ")))
      (if (confirm-changes-p
           (list "Destination directory" "Project name" "Project description")
           (list destination name description))       
          (create-new-project-not-interactive destination
                                              name
                                              description
                                              :quiet quiet
                                              :force force)
          (format t "~a~%" "Nothing changed!"))))

  (defun copy-default-conf ()
    (let* ((destination
            (read-field "Configuration directory destination: "))
          (conf-dir-destination
            (cl-fad:merge-pathnames-as-directory destination "skeleton-creator-conf/")))
      (ensure-directories-exist conf-dir-destination)
      (set-configure-directory conf-dir-destination)
      (copy-directory
       (cl-fad:merge-pathnames-as-directory
        (asdf:system-source-directory :skeleton-creator)
        "default-conf/")
       conf-dir-destination
       :overwrite t)))

  (defun confirm-changes-p (list-fields list-values)
    (do ((i 0 (incf i))
         (fields list-fields (cdr fields))
         (values list-values (cdr values)))
        ((<= (length list-fields) i)
         (progn (format t "~%~%~a" "Do you want to complete the above changes?")
                (yes-or-no?)))
      (format t "~%~a: ~a" (car fields) (car values))))
  
  (defun read-field (field-name)
    (format t "~a" field-name)
    (read-line))
  
  (defun use-default-conf-p (warning-msg)
    (format t "~a~%~%~a "
            warning-msg
            "Do you want to use the skeleton-creator default configuration directory?")
    (if (yes-or-no?) t nil))
 
  (defun create-new-project-not-interactive (destination-directory name description
                                             &key quiet
                                                  force)
    (let* ((path-project
            (cl-fad:merge-pathnames-as-directory
             destination-directory
             (conc name "/")))
          (existing-directory
            (cl-fad:directory-exists-p path-project)))
      (handler-case
          (if (and existing-directory (not force))
              (if (not quiet)
                  (format t "~a" "Project NOT created successfully! The directory already exists."))
              (progn (create-project sk destination-directory name description)
                     (if (not quiet)
                         (format t "~a" "Project created successfully."))))
        (error (c)
          (format t "Got an exception: ~a~%" c)
          (if existing-directory
              (delete-project-directory path-project))
          (if (not quiet)
              (format t "~a" "The project was NOT created successfully!"))))))

  (defun license-project ()
    ;; Verificar se o diretorio padrão contem um diretorio chamado licenses
    ;; se não, então peguntar se quer usar o diretório padrão default-conf
    ;; se sim, então continuar a função
    ;; caso contrário perguntar se quer configurar um novo diretorio de configuração
    ;; se sim configurá-lo,
    ;; se não, retornar aviso que nada foi feito e que é preciso de um diretorio license
    ;; dentro do diretorio de configuração
    ;; caso tenha, verificar se existe um diretorio notices dentro de licenses,
    ;; se não, então configurar as keys write-license-notices e write-in-readme para nil
    ;; se tiver, então perguntar se quer escrever avisos nos arquivos e se quer escrever também no readme
    ;; por sobrescrever arquivos é necessário que essa função confirme, mostrando tudo que irá ser alterado, antes de fazer as alterações, para que seja confirmado
    )

  (defun license-project-not-interactive (project-directory license-name
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
    (format t "~%~a ~a?~%" "Tem certeza que deseja excluir o diretório" project-directory)
    (if (yes-or-no?)
        (progn (delete-project project-directory)
               (format t "~%~a~%" "Directory deleted successfully!"))
        (format t "~%~a~%" "Nothing was excluded."))))

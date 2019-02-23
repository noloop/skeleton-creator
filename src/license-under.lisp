(in-package #:noloop.skeleton-creator)

;; (defun license-under (project-directory
;;                         licenses-directory
;;                         license-name
;;                         &key (create-license-file-p t)
;;                              (write-license-notices-p t)
;;                              (write-in-readme-p t))
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

(defun write-license-notices (project-directory licenses-directory license-name hash-markings)
  )

;; (defun write-in-readme ())

(defun get-string-license (licenses-directory license-name)
  (get-string-from-file (cl-fad:merge-pathnames-as-file
                         licenses-directory
                         (conc (string-downcase license-name)
                               ".txt"))))

;; substituir as marcacoes na string de aviso para a licensa que
;; deve estar em cada arquivo
;; pesquisar nos aquivos se já existe a string de aviso ou uma LICENSE
;; string no inicio do arquivo
;; acrescentar no inicio de todos os arquivos #| aviso license|# com execeção dos arquivos LICENSE(s)
;; acrescentar no fim dos arquivos README.md(s) um topico "### LICENSE" copiando o aviso de licensa para ele

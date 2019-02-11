(in-package #:noloop.ui-skeleton-creator)

(let ((sk (init-skeleton-creator "~/.config/skeleton-creator-conf/")))
  
  (defun set-configure-directory (new-directory)
    (set-conf-directory sk new-directory))
  
  (defun get-configure-directory ()
    (get-conf-directory sk))

  (defun configure-skeleton-creator ()
    (conf-skeleton-creator sk))

  (defun create-new-project (destination-directory name description)
    (create-project (sk destination-directory name description)))

  (defun delete-project-directory (project-directory)
    (delete-project project-directory)))


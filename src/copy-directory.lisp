(in-package #:noloop.skeleton-creator)

(defun copy-directory (origin destination &key (overwrite nil))
  (let ((list-dir (cl-fad:list-directory origin :follow-symlinks nil)))
    (ensure-directories-exist destination)
    (dolist (path list-dir)
      (cond ((cl-fad:directory-exists-p path)
             (progn (ensure-directories-exist (merge-directory-with-subtract-path path origin destination))
                    (copy-directory path
                                              (merge-directory-with-subtract-path path origin destination)
                                              :overwrite t)))
            ((pathname-is-file path)
             (cl-fad:copy-file path (merge-file-with-subtract-path path origin destination)
                               :overwrite overwrite))))))

(defun pathname-is-file (path)
  (and (not (cl-fad:directory-exists-p path))
       (cl-fad:file-exists-p path)))

(defun merge-directory-with-subtract-path (path origin destination)
  (cl-fad:merge-pathnames-as-directory destination (pathname-subtract origin path)))

(defun merge-file-with-subtract-path (path origin destination)
  (cl-fad:merge-pathnames-as-file
   (cl-fad:merge-pathnames-as-file destination (pathname-subtract origin path))
   (concatenate 'string (pathname-name path) (if (pathname-type path) ".") (pathname-type path))))

(defun pathname-subtract (path-1 path-2)
  "Compare path-1 with path-2, and return new pathname with rest of path-2 at the point where it differentiated."
  (let* ((list-path-1 (pathname-directory path-1))
         (list-path-2 (pathname-directory path-2))
         (new-list (list-subtract list-path-1 list-path-2))
         (new-path "/"))
    (dolist (el new-list)
      (setf new-path (cl-fad:merge-pathnames-as-directory new-path
                                                          (concatenate 'string el "/"))))
    (pathname (subseq (namestring new-path) 1))))

(defun list-subtract (list-1 list-2)
  "Compare elements of list-1 with elements of list-2, return new list with elements of list-2 not contained in list-1. Return immediately for elements differents, the comparison follow order of elements."
;;; Example:
;;; (list-subtract '("home" "you" "lisp")' ("home" "new" "you" "lisp" "child-dir" "you"))
  (do ((c 0 (incf c))
       (i list-1 (cdr i))
       (j list-2 (cdr j))
       (new-list list-2
                 (if (string= (car i)
                              (car j))
                     (progn (pop new-list)
                            new-list)
                     (return new-list))))
      ((>= c (length list-1)) new-list)))


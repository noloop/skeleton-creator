(in-package #:noloop.skeleton-creator)

(defmacro conc (&rest string-args)
  "Synthetic sugar for concatenate strings."
  `(concatenate 'string ,@string-args))

(defun is-ok? (fn)
  (format t "IS OK?(yes/no)~%")
  (if (string-equal "yes" (read))
      fn
      nil))

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
    (write-string stg stream)))

(defun string-replace-all (stg old new)
  (let* ((cl-ppcre:*allow-quoting* t)
         (old (concatenate 'string "\\Q" old)))
    (cl-ppcre:regex-replace-all old stg new)))


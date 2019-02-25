(in-package #:noloop.skeleton-creator)

(defmacro conc (&rest string-args)
  "Synthetic sugar for concatenate strings."
  `(concatenate 'string ,@string-args))

(defun yes-or-no? ()
  (format t "(yes/no): ")
  (let ((response (read-line)))
    (if (or (string-equal "yes" response)
            (string-equal "y" response))
        t
        nil)))

(defun yes-or-no?-fn (fn)
  (format t "(yes/no): ")
  (let ((response (read-line)))
    (if (or (string-equal "yes" response)
            (string-equal "y" response))
        (funcall fn)
        nil)))

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

(defun get-date-year-string ()
  "Returns current year as a string."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore sec min hr day mon dow dst-p tz))
    (format nil "~a" yr)))

(defun read-field (field-name)
  (format t "~a" field-name)
  (read-line))

(defun empty-directory-p (path)
  (and (null (directory (concatenate 'string path "/*")))
       (null (directory (concatenate 'string path "/*/")))))

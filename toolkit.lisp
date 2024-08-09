(in-package #:displayer)

(defun run-args (args)
  (flet ((frob (arg)
           (etypecase arg
             (string arg)
             (real (princ-to-string arg))
             (pathname (uiop:native-namestring arg)))))
    (loop for arg in args
          append (if (listp arg) (mapcar #'frob arg) (list (frob arg))))))

(defun run (program &rest args)
  (let ((out (make-string-output-stream)))
    (if (= 0 (nth-value 2 (uiop:run-program (list* program (run-args args))
                                            :output out :error-output out
                                            :ignore-error-status T)))
        (get-output-stream-string out)
        (error "~a" (get-output-stream-string out)))))

(defun make-thumbnail (input output &key (w 1920/4) (h 1080/4))
  (run "ffmpeg" "-hide_banner" "-loglevel" "error"
       "-i" input
       "-ss" "00:00:01.000"
       "-vf" (format NIL "scale=~a:~a:force_original_aspect_ratio=decrease" w h)
       "-vframes" "1"
       output))

(defun download-video (url output)
  (run "yt-dlp" url
       "-o" output))

(defun download-thumbnail (url output)
  (run "yt-dlp" url
       "--write-thumbnail"
       "--skip-download"
       "-o" output))

(defun video-file (name)
  (radiance:environment-module-pathname #.*package* :data (make-pathname :name (string-downcase name) :type "mp4")))

(defun list-videos ()
  (directory (radiance:environment-module-pathname #.*package* :data (make-pathname :name :wild :type "mp4"))))

(defun video-thumbnail (input)
  (if (pathnamep input)
      (video-thumbnail (pathname-name input))
      (radiance:environment-module-pathname #.*package* :data (make-pathname :name (string-downcase input) :type "mp4"))))

(defun video-name (input)
  (pathname-name input))

(defun copy-video (input name)
  (let ((output (video-file name))
        (thumbnail (video-thumnbail output)))
    (etypecase input
      (string
       (download-video input output)
       (download-thumbnail input thumbnail))
      (pathname
       (uiop:copy-file input output)
       (make-thumbnail input thumbnail)))
    output))

(defun playlist ()
  (radiance:environment-module-pathname #.*package* :data "playlist.m3u"))

(defun make-playlist (&optional (videos (list-videos)))
  (with-open-file (stream (playlist) :direction :output :if-exists :supersede)
    (dolist (video videos)
      (format stream "~&~a~%" (uiop:native-namestring video)))))

(defun mktab (&rest keys)
  (let ((table (make-hash-table :test 'eql)))
    (loop for (k v) on keys by #'cddr
          do (setf (gethash k table) v))
    table))

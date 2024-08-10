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
       "-y" "-i" input
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

(defun probe-length (input)
  (read-from-string
   (run "ffprobe" "-hide_banner" "-loglevel" "error"
        "-select_streams" "v:0"
        "-show_entries"
        "stream=duration"
        "-of" "default=noprint_wrappers=1:nokey=1"
        input)))

(defun list-videos ()
  (directory (radiance:environment-module-pathname #.*package* :data (make-pathname :name :wild :type "mp4"))))

(defun video-file (name)
  (if (pathnamep name)
      name
      (radiance:environment-module-pathname #.*package* :data (make-pathname :name (string-downcase name) :type "mp4"))))

(defun video-thumbnail (input)
  (if (pathnamep input)
      (video-thumbnail (pathname-name input))
      (radiance:environment-module-pathname #.*package* :cache (make-pathname :name (string-downcase input) :type "png"))))

(defun video-length (input)
  (probe-length input))

(defun video-name (input)
  (pathname-name input))

(defun video-data (input)
  (let* ((file (probe-file (video-file input)))
         (name (video-name file)))
    (if file
        (mktab :name name
               :file (uri-to-url "/api/displayer/video/file"
                                 :representation :external
                                 :query `(("name" . ,name)))
               :thumbnail (uri-to-url "/api/displayer/video/thumbnail"
                                      :representation :external
                                      :query `(("name" . ,name)))
               :length (video-length file)
               :size (file-size file))
        (error "No such video ~a" input))))

(defun copy-video (input &optional (name (pathname-name input)))
  (let* ((output (video-file name))
         (thumbnail (video-thumbnail output)))
    (etypecase input
      (string
       (download-video input output)
       (download-thumbnail input thumbnail))
      (pathname
       (unless (equal input output)
         (uiop:copy-file input output)
         (uiop:delete-file-if-exists input))
       (make-thumbnail input thumbnail)))
    output))

(define-trigger startup ()
  (ensure-directories-exist
   (radiance:environment-module-directory #.*package* :data))
  (ensure-directories-exist
   (radiance:environment-module-directory #.*package* :cache)))

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

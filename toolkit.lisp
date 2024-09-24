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
       "-S" "ext"
       "-o" output))

(defun download-thumbnail (url output)
  (run "yt-dlp" url
       "--skip-download"
       "--write-thumbnail"
       "--convert-thumbnails" "png"
       "-o" (make-pathname :type NIL :defaults output)))

(defun probe-length (input)
  (read-from-string
   (run "ffprobe" "-hide_banner" "-loglevel" "error"
        "-select_streams" "v:0"
        "-show_entries"
        "stream=duration"
        "-of" "default=noprint_wrappers=1:nokey=1"
        input)))

(defun vpath (kind name type)
  (radiance:environment-module-pathname #.*package* kind (make-pathname :name name :type type)))

(defun list-videos ()
  (directory (vpath :data :wild "mp4")))

(defun video-file (name)
  (if (pathnamep name)
      name
      (vpath :data (string-downcase name) "mp4")))

(defun video-thumbnail (input)
  (if (pathnamep input)
      (video-thumbnail (pathname-name input))
      (let ((path (vpath :cache (string-downcase input) "png")))
        (unless (probe-file path)
          (make-thumbnail (video-file input) path))
        path)))

(defun video-length (input)
  (if (pathnamep input)
      (video-length (pathname-name input))
      (let ((path (vpath :cache (string-downcase input) "txt")))
        (unless (probe-file path)
          (with-open-file (stream path :direction :output)
            (prin1 (probe-length (video-file input)) stream)))
        (with-open-file (stream path)
          (read stream)))))

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

(defun delete-video (input)
  (let ((name (string-downcase (if (pathnamep input) (pathname-name input) input))))
    (uiop:delete-file-if-exists (vpath :cache (string-downcase name) "txt"))
    (uiop:delete-file-if-exists (vpath :cache (string-downcase name) "png"))
    (uiop:delete-file-if-exists (vpath :data (string-downcase name) "mp4"))
    name))

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

(defun mktab (&rest keys)
  (let ((table (make-hash-table :test 'eql)))
    (loop for (k v) on keys by #'cddr
          do (setf (gethash k table) v))
    table))

(defun format-duration (secs)
  (format NIL "~2,'0d:~2,'0d"
          (truncate secs 60)
          (mod (round secs) 60)))

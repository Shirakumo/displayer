(in-package #:displayer)

(defun run-args (args)
  (flet ((frob (arg)
           (etypecase arg
             (string arg)
             (real (princ-to-string arg))
             (pathname (pathname-utils:native-namestring arg)))))
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
  (ensure-directories-exist output)
  (run "ffmpeg" "-hide_banner" "-loglevel" "error"
       "-y" "-i" input
       "-ss" "00:00:01.000"
       "-vf" (format NIL "scale=~a:~a:force_original_aspect_ratio=decrease" w h)
       "-vframes" "1"
       output))

(defun download-video (url output)
  (ensure-directories-exist output)
  (let ((tmp (org.shirakumo.filesystem-utils:make-temporary-file :type (pathname-type output))))
    (run "yt-dlp" url
         "-S" "ext"
         "-f" "bv[height<=1080]+ba/b[height<=1080]"
         ;;"--use-postprocessor" "FFmpegCopyStream"
         ;;"--ppa" "CopyStream:-c:v libx264 -preset veryfast -c:a aac -f mp4"
         "-o" tmp)
    (org.shirakumo.filesystem-utils:rename-file* tmp output)))

(defun download-thumbnail (url output)
  (ensure-directories-exist output)
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

(defun vpath (kind name type &rest args)
  (radiance:environment-module-pathname #.*package* kind (apply #'make-pathname :name name :type type args)))

(defun list-videos ()
  (directory (vpath :data :wild "mp4")))

(defun list-enabled-videos ()
  (directory (vpath :data :wild "mp4" :directory '(:relative "enabled"))))

(defun video-file (name)
  (if (pathnamep name)
      name
      (vpath :data (string-downcase name) "mp4")))

(defun video-enabled-file (name)
  (if (pathnamep name)
      (video-enabled-file (pathname-name name))
      (vpath :data (string-downcase name) "mp4" :directory '(:relative "enabled"))))

(defun video-enabled-p (input)
  (probe-file (video-enabled-file input)))

(defun enable-video (input)
  (let ((file (video-enabled-file input)))
    (unless (probe-file file)
      (org.shirakumo.filesystem-utils:create-symbolic-link
       (pathname-utils:native-namestring file)
       (pathname-utils:native-namestring (video-file input))))))

(defun disable-video (input)
  (let ((file (video-enabled-file input)))
    (when (probe-file file)
      (delete-file file))))

(defun video-thumbnail (input &rest args &key (create T))
  (if (pathnamep input)
      (apply #'video-thumbnail (pathname-name input) args)
      (let ((path (vpath :cache (string-downcase input) "png")))
        (when (and create (not (probe-file path)))
          (make-thumbnail (video-file input) path))
        path)))

(defun video-length (input &key (create T))
  (if (pathnamep input)
      (video-length (pathname-name input))
      (let ((path (vpath :cache (string-downcase input) "txt")))
        (when (and create (null (probe-file path)))
          (with-open-file (stream path :direction :output)
            (prin1 (probe-length (video-file input)) stream)))
        (with-open-file (stream path :if-does-not-exist NIL)
          (if stream
              (read stream)
              0)))))

(defun video-name (input)
  (pathname-name input))

(defun video-data (input)
  (let* ((file (probe-file (video-file input)))
         (name (video-name file)))
    (if file
        (mktab :name name
               :enabled (video-enabled-p file)
               :file (uri-to-url "/api/displayer/video/file"
                                 :representation :external
                                 :query `(("name" . ,name)))
               :thumbnail (uri-to-url "/api/displayer/video/thumbnail"
                                      :representation :external
                                      :query `(("name" . ,name)))
               :length (video-length file :create NIL)
               :size (file-size file))
        (error "No such video ~a" input))))

(defun delete-video (input)
  (let ((name (string-downcase (if (pathnamep input) (pathname-name input) input))))
    (uiop:delete-file-if-exists (vpath :cache (string-downcase name) "txt"))
    (uiop:delete-file-if-exists (vpath :cache (string-downcase name) "png"))
    (uiop:delete-file-if-exists (vpath :data (string-downcase name) "mp4"))
    (uiop:delete-file-if-exists (vpath :data (string-downcase name) "mp4" :directory '(:relative "enabled")))
    name))

(defun copy-video (input &optional (name (pathname-name input)))
  (let* ((output (video-file name))
         (thumbnail (video-thumbnail output :create NIL)))
    (etypecase input
      (string
       (download-video input output)
       (download-thumbnail input thumbnail))
      (pathname
       (unless (equal input output)
         (uiop:copy-file input output)
         (uiop:delete-file-if-exists input))
       (make-thumbnail input thumbnail)))
    (video-length output)
    output))

(define-trigger startup ()
  (ensure-directories-exist
   (radiance:environment-module-directory #.*package* :data))
  (ensure-directories-exist
   (radiance:environment-module-pathname #.*package* :data "enabled/"))
  (ensure-directories-exist
   (radiance:environment-module-directory #.*package* :cache))
  (make-instance 'restart-video)
  (restart-task-runner))

(define-trigger shutdown ()
  (stop T))

(defun mktab (&rest keys)
  (let ((table (make-hash-table :test 'eql)))
    (loop for (k v) on keys by #'cddr
          do (setf (gethash k table) v))
    table))

(defun format-duration (secs)
  (format NIL "~2,'0d:~2,'0d"
          (truncate secs 60)
          (mod (round secs) 60)))
